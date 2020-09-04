! $Header: m:/default/source/RCS/comm.f,v 1.273 2016/11/07 19:35:38 mfl Exp $
      SUBROUTINE COMM
! -------------------------------------------------------------------
!  NEMS Commercial Demand Module (CDM)                              *
!                                                                   *
!  A component of the U.S. Energy Information Administration of the *
!  Department of Energy's National Energy Modeling System (NEMS)    *
!                                                                   *
!  LANGUAGE:      FORTRAN                                           *
!  CALLED BY:     PROGRAM NEMS (Integrating Module)                 *
!                                                                   *
!  ANALYSIS:      AEO2017                                           *
!  CASE:          Reference                                         *
!  DATE:          October 24, 2016                              	    *
! -------------------------------------------------------------------
!*******************************************************************
! AEO2017 CHANGES			
!	-Read in KMELS for MELs, Office PC, and Office Non PC	    *
!	-Read in Kmels for Office Misc and MELS by index            *
!	-Take out hard coded dates				    *
!	-Zero out flrspace  line 874				    *
!	-Remove 0.90 non-building natural gas benchmarking factor   *
!          (!STEObenchNG)					    *
!	-Remove 0.90 non-building natural gas benchmarking factor   *
!          (!STEObenchNG)					    *
!  - Adjusted shares of PV generation subtracted from end-use       *
!    consumption (i.e., cooling, ventilation, lighting, and other)  *
!	-Updated MELs shares video is in Non PC, Data Servers, & PCs*
!	-Updated initialization of subsidies to 0 instead MAXCOST   *
!	-Updated call to calculate EE Costs to include CA EE costs  *
!	 in No CPP cases for electricity pricing purposes. Zero out *
!		costs for all but CD9 using			    *
!		EPA111d switch in cost calc 			    *
!       rather than subroutine callso can call subroutine in all    *
!       cases, also read bldbase in all cases(!EEcosts) 	    *
!********************************************************************
!  AEO2016 CHANGES                                                  *
!    -Adjust non-building natural gas use for February 2016 STEO    *
!      benchmarking (!STEObenchNG)                                  *
!    -Remove AEO2015 exclusion of natural gas from post-STEO        *
!      benchmarking ramp-down (!STEObm)                             *
!    -Add switch to KPARM.txt (NoHist) to allow side case turning   *
!      off all historical and STEO calibration-checks pure model    *
!      results and could be counterfactual scenario (! NoHistory)   *
!    -Read STEO values from new steoblock include and adjust        * 
!      historical calibration factor to avg of last 5 years         *
!      instead of last historical year (! STEOread-avg)             *
!    -Add variables for end-use consumption including PV            * 
!      self-gen for uldsm to build load shapes (!PVdispatch)        *
!    -Add 111d subsidies for PV and Wind (!111(d)ren)               *
!    -Alter code to read in growth rate of floorspace from          *
!      Macroeconomic Activity Module rather than proprietary        *
!      floorspace data                                              *
!    -Set AEOyr to 27 for current AEO                               *
!    -Correct MiscElConsump end-use numbers to correct continuity   *
!    -Updated calculation of solar PV generation based              *
!     on PVWatts 5 (!PVgen)                                         *
!    -Updated DG database output to have dynamic start year instead *
!      of fixed in 2000                                             *
!********************************************************************
!  AEO2015 CHANGES                                                  *
!    -Change 111(d) indirect cost multiplier to 1.5                 *
!    -Add read-in of equipment subsidies from KTEK, with output of  *
!      equipment & subsidy investments to commercial database       *
!      (investsub)                                                  *
!    -Add savings calculations for 111(d), including read-in of     *
!      baseline electricity consumption                             *
!    -Update shell improvement to account for adoption of more      *
!      recent ASHRAE codes                                          *
!       -Add variables NewShAdjHt, NewShAdjCl (!shlcodes)           *
!    -Reallocate shares of PV generation subtracted from end-use    *
!      consumption (i.e., cooling, ventilation, lighting, and other)*
!      (!PVshare)                                                   *
!    -Set iKtechCostyr to 2013 for latest KTEK update               *
!    -Exclude natural gas from post-STEO benchmarking ramp-down     *
!      (!STEObm)                                                    *
!    -Fixed unintentional interaction between ebill and DG non-     *
!      negative consumption test code                               *
!    -Set AEOyr to 26 for current AEO                               *
!********************************************************************
!  AEO2014 CHANGES                                                  *
!    -Add "other" and "other gaseous" cogen fuels to CGCOMMGEN      *
!      (used by EMM, not CDM)                                       *
!    -Updated fossil-fuel heat rate for renewables consumption;     *
!      added variable KFossilHR to code and comvars to facilitate   *
!      future updates                                               *
!    -Updated coefficients for miscdetail ElQs and reorganized      *
!      writing of misc to KDebug & KDBout                           *
!    -Added security systems, kitchen ventilation, video displays,  *
!      large video boards, and lab refrigerators and freezers;      *
!      removed liquid-immersed transformers (mostly used by         *
!      utilities); combined X-Ray, MRI, CTS, and other medical      *
!    -Set 3% floor for effective hurdle rate per FEMP title 10      *
!      CFR 436.14                                                   *
!    -Renamed XDEGRED and XDEGREDATION to XDEGRAD and XDEGRADATION, *
!      respectively                                                 *
!    -NewShAdj, NewShAvgHt, and NewShAvgCl calculations changed to  *
!      case statements                                              *
!    -Add code to turn off retrofit behavior (STRetBehav=0)         *
!    -Added variable AEOYR to correspond to current AEO report year *
!      (used mostly for side cases) and for retrofit behavior       *
!      mentioned above                                              *
!    -Moved parameter M (max rows in KTEK) to COMPARM               *
!********************************************************************
!  AEO2013 CHANGES                                                  *
!    -Convert PV, solar thermal, and wind output to fossil fuel     *
!      equivalent for reporting in FTAB                             *
!    -Increase DG penetration rate into existing bldgs              *
!    -Remove 'AEO2012 only' STEO adjustments                        *
!    -Update CHP variables (eliminate reference to old)             *
!    -Remove reference to unused KCALTRN and KCALNUG input files    *
!    -Modified read-in of reorganized KDEGDAY file for use in both  *
!      the CDM and RDM                                              *
!    -Add credit calculations and global variables for clean energy *
!      credits for NG CHP (!ceschp)                                 *
!********************************************************************
!  AEO2012 CHANGES                                                  *
!    -Turned off endogenous PV builds before 2011 to prevent        *
!      double-counting                                              *
!    -Adjusted 2012 STEO electricity mistie (for AEO2012 only)      *
!    -Added laboratory fume hoods, lodge/health/merc & servc        *
!      laundry, and other medical to miscdetails                    *
!    -Updated coefficients for miscdetail ElQs                      *
!    -Removed explicit shares of elevators from food sales and      *
!      escalators from food sales, food service, small offices, and *
!      warehouses                                                   *
!    -Removed 'No EPACT 05 dry transformer standard'                *
!    -Header for KSDOUT moved to bottom of text file so Comm DB can *
!      read without having to modify                                *
!    -Added efficiency to ksdout ("effksdout")                      *
!    -'Run Scenario' modified to allow 10 characters                *
!    -Add Shell Factors to CommDB (!shellkdbout)                    *
!       -Also uses stored shell indices to replace additional in-   *
!        stream calculationss using NewShAdj                        *
!    -Add warehouse variables for use in industrial agriculture     *
!      Technology Possibility Curve (TPC) development (only prints  *
!      if MAXITR > 0 (!indytpc)                                     *
!    -Updated treatment of minor fuels, new regression results and  *
!      elasticities (!Minorfuel Project)                            *
!********************************************************************
!  AEO2011 CHANGES                                                  *
!    -Interconnection limitations added for distributed generation  *
!      ("inxlimit")                                                 *
!    -Changes to implement separate factors for shell effects on    *
!      heating and cooling ("shlfactor")                            *
!********************************************************************
!  AEO2010 CHANGES                                                  *
!    -Steve's streamlining and alternative treatment for price-     *
!      induced changes to hurdle rates                              *
!    -Set CMLastYr using run-time option AEOLSTYR                   *
!    -IJUMPYR now set to year 61 instead of 41 or 46                *
!    -Add regional dimension to tech availability to model regional *
!      efficiency standards                                         *
!********************************************************************
!  AEO2009 CHANGES                                                  *
!    -American Recovery and Reinvestment Act of 2009 ("arra09")     *
!    -Use MNUMYR for dimensions and IJUMPYR for loops and IF Blocks.*
!      This would allow for extension to 2050 without recompiling   *
!      all the code as IJUMPYR is not a parameter but a model input *
!    -Read ktek.xml file instead of ktech.wk1 file                  *
!    -Adjust assumptions for growth of non-bldg NG use              *
!    -Includes enhanced RPS modeling in sub distgen                 *
!********************************************************************
!  AEO2008 CHANGES                                                  *
!    -Update DG module with internal rate of return, add wind,      *
!      niches for PV, gas techs, wind                               *
!    -Update ktech year to 2007 for tech updates                    *
!    -Regionalize ktech costs (allow varied costs by CD)            *
!    -"ds08" update district services                               *
!    -"nrg07" changes for energy bill 2007                          *
!********************************************************************
!  AEO2007 CHANGES                                                  *
!    -"regsteo" update to use regional STEO data                    *
!    -"Loop CBECS03" run multiple DG size classes                   *
!    -"Inverters" discrete replacements of PV Inverters             *
!    -Now MSEDYR=14(2003) - revert to MSEDYR+2 (2005) for "as good  *
!      as SEDS"                                                     *
!    -"miscdetail" - breakout misc categories based on August 2006  *
!      TIAX report                                                  *
!    -Update base/CBECS year to 2003 (model starts 2004)            *
!********************************************************************
!  AEO2006 CHANGES                                                  *
!    -Update to handle extension to 2030, change MNUMYR references  *
!      in code to CMLastYr. Change array dimensions to MJUMPYR.     *
!      Needed because MNUMYR set to 61(2050), but model extended to *
!      2030 (41)                                                    *
!    -"moredata" - denoting adds for output database                *
!    -"acceldepr" - streamline depreciation calc                    *
!    -"taxcreditc/o" - carryover of the PV credit                   *
!    -"Add CRI to TechChoice" - lighting enhancements               *
!    -"varyxkw" (add DG year-varying capacity)                      *
!    -"ebill" adjust other elec Q to account for transformer and    *
!      traffic signal standards in energy bill, reduce HW Q         *
!      starting in 2006 to implement pre-rinse spray valve standard *
!    -Assume through 2004 is "as good as SEDS"                      *
!    -No STEO Benchmarking to residual fuel oil 9-14-05             *
!********************************************************************
!  AEO2005 CHANGES                                                  *
!    -Assume through 2003 is "as good as SEDS"                      *
!    -Includes population shifts in weather data, with values       *
!      through 2025 for HDD and CDD ("popDD")                       *
!    -Update to bench all fuels to within 2% instead of exactly to  *
!      STEO using STEOCLS kparm parameter                           *
!    -STEOTieDecayFactor extended to all fuels but residual fuel oil*
!    -Update ktech year to 2004 for tech updates                    *
!    -Adjust biomass Q variable for updated SEDS                    *
!********************************************************************
!  AEO2004 CHANGES                                                  *
!    -Add building type dimension to EquipSD for report             *
!      ("add b to ksdout")                                          *
!    -Include non-bldg use in regional share "ebe<date>             *
!    -Relax cap on dgen penetration in new construction for projects*
!      with payback periods < 15 years ("relax dg cap in new")      *
!    -Adjust dgen tax treatment of energy costs                     *
!      ("refine tax accounting")                                    *
!    -Allow more liberal dgen penetration into existing construction*
!      if penetration into new is high ("relax dg pen in existing") *
!********************************************************************
!    -Endogenous Shell Improvements (marked "endshel")              *
!    -Equipment investment info added(marked "invest")              *
!    -End-use prices for electricity (marked "eup")                 *
!    -Treatment of Misc. end use to match EUIs and include gas and  *
!      oil ("Misc")                                                 *
!    -Added commercial benchmark switch and KSTEOYR treatment of    *
!      Electricity. Also treat RS nonbldg use the same as DS        *
!      nonbldg use. Block starts with comment "Adjustment factors"  *
!    -Add price elasticity to all fuels  non-building use (adjusted *
!      to SEDS) calculations ("mfelas")                             *
!    -Adjustment in calc of baseyear shares to include cooling HPs  *
!      in base efficiency ("bashp")                                 *
!    -Adjust rebound effect to guard against efficiency of zero in  *
!      previous year ("Rbndfix")                                    *
!    -Add accounting for solar thermal water heating to calculate   *
!      solar Q from technology selections ("solacc")                *
!    -Added solarPV, fuel cell and cogen accounting ("distgen")     *
!    -Elasticity added to discount rates for technology choice for  *
!      increasing prices ("del")                                    *
!    -PITC -- price-induced technical change                        *
!    -Purchased efficiency calcs (marked "peff")                    *
!    -Code to cap and check price elas & rebound effects ("noneg")  *
!      Rebound elasticity is 0.15                                   *
!    -Code to defend against zero fuel prices ("! defend")          *
!    -UtilLink & temp -- altered Cogen common block setup.          *
!      Need to adjust once Electric Utility Module changes are made.*
!    -STEO benchmark (except for weather) to one less than available*
!      years, reporting of misties "sides"                          *
!    -Deleted DSM equipment-specific code (see default code for     *
!      AEO2001 if it needs to be revived)                           *
!    -Added Efficiency Index Calc' "Efficiency Index"               *
!    -Added data center rqmts to sdi for cooling, ventilation,      *
!      other (marked "dcadjust")                                    *
!    -Added explicit estimate for waste consumption to SEDS biomass *
!      to match revised MER estimates                               *
!********************************************************************


      IMPLICIT NONE
      INTEGER  FILE_MGR

      include'parametr'! system-wide parameter constants
      include'ncntrl'  ! control variables set by integrating module
      include'comparm' ! commercial module parameters
      include'comvars' ! commercial module common variables
      include'eusprc'                                                 ! elastruns
      include'uefpout'                                                ! elastruns
      include'apq'                                                    ! elastruns
      include'steoblock' ! common STEO inputs                         ! STEOread-avg
!  Note: when doing elasticity runs, include PQ block instead of APQ  ! elastruns
!      MUST also make this change in the Distributed Gen Subroutine   ! elastruns
!      include'pq.'                                                   ! elastruns
      include'qsblk'       ! seds values

      REAL*4 BASELINEBKWHCM                                           ! 111(d) - Stores restart file values of electricity consumption by Census division and year
      COMMON /BASE111DCM/ BASELINEBKWHCM(MNUMCR,MNUMYR)               ! 111(d)

      REAL*4 com111drensub                                            ! 111(d)ren - subsidies for PV and Wind
	  Integer iGenCapCostYR                                           ! 111(d)ren - moving capital cost year to common for calc111d subroutine
	  COMMON /EPA111drensub/ com111drensub(mnumyr,mnumcr,11),    &    ! 111(d)ren - last dimension is number of commercial techs, parameter not set here
	                         iGenCapCostYR                            ! 111(d)ren
							 
      INTEGER*2 AEOYR      !Index corresponding to current AEO year (AEO report year - 1989)
      COMMON /AEOReportYear/ AEOYR

      INTEGER*4 RCPRM,   & ! KPARM  file handle
                RCDBG,   & ! KDEBUG file handle
                RCRPT,   & ! KRPT   file handle
                RCQUAN     ! KQUANT file handle

      ! Preserve file handles which must persist between calls:
      COMMON /COMFileHandles/ RCDBG,RCRPT,RCQUAN

      INTEGER*2 DecayBM ! STEO Mistie Benchmarking Switch
      INTEGER*4 LastDecayYr ! Year in which STEO Benchmarking is 0
      INTEGER*2 NoHist ! Switch - set to 1 in KPARM to turn off calibration to history and STEO ! NoHistory
      ! Pass these benchmarking parameters from KPARM:
      COMMON /STEOBenchSwitches/ LastDecayYr,DecayBM,NoHist

      INTEGER*2 STRetBehav ! Same Tech behavior during Retrofit
      COMMON /CMBehaveSwitches/ STRetBehav

      INTEGER*2 CostTrendSwitch ! Use/don't use cost trend approach
      COMMON /CMCostSwitches/ CostTrendSwitch

      INTEGER*4 iforward(3)   !   calculation of price effects on tech menu  ! PITC
      INTEGER*4 IFMAX         !   maximum forward effect                     ! PITC
      INTEGER*4 iLastSTEOyr   !   calendar year for last STEO data           ! PITC
      COMMON /pitc/iforward,IFMAX,iLastSTEOyr                                ! PITC

      INTEGER*2 I, J  ! general purpose loop indices
      INTEGER*4 IOS,count,InputCount,NumErr ! Input error checking
      INTEGER*4 ReportOption ! KRPT reporting switch
      INTEGER*4 SDReportOpts(2)                                        ! sw10-95
      INTEGER*2 y, r, modyear, endmodyear, s, f                        ! elastrun
      REAL*4 ELfactor, NGfactor, DSfactor                              ! elastrun

      INTEGER EPA111D  !111(d)
      INTEGER AB32SW

      INTEGER*4 AEOLSTYR   ! run-time option set to calendar year for last year of AEO projection period
      INTEGER*4 RTOVALUE   ! External routine to get options
      EXTERNAL RTOVALUE

      INTEGER*4 infile     !  Reusable file handle

      AEOLSTYR = RTOVALUE("AEOLSTYR  ",0)  ! Get calendar year for last year of AEO projection period

      EPA111D = RTOVALUE("EPA111D ",0)  ! SCEDES switch to activate subsidies for 111(d) analysis
      AB32SW = RTOVALUE("AB32SW  ",0)

      !  Base calendar year corresponding to curiyr=1 (as found in parametr include file)
      !BASEYR=1990 (do not uncomment)

      ! Index corresponding to current AEO year (AEO report year - 1989)
      AEOYR = 27
 
      ! Index of the first year to forecast (first year after CBECSyear)
      CMFirstYr = CBECSyear - BASEYR + 2

      ! Index of the last year to forecast (commercial model and assumption data extended to 2050, but MNUMYR extended to 2050)
      CMLastYr = AEOLSTYR - BASEYR + 1

      ! Index of dollar year for KTEK technology costs (actual dollar year-BASEYR+1)
      iKtechCostYr = 2013 - BASEYR + 1

!  The NEMS Commercial Sector Demand Module is based on data derived from the quadrennial CBECS survey, the latest version
!  of which forms the starting point from which the forecast is evolved.  The year of the latest CBECS survey is given by
!  CBECSyear.  The forecast for prior years is left unchanged from the initial default forecast contained in the NEMS
!  RESTART file.  In addition, the year immediately following CBECSyear must be contained in the user-specified forecast
!  horizon to enable continuous evolution, or else the initial default NEMS RESTART forecast is left unchanged:

      IF (CURIYR .LT. CMFirstYr) THEN

         y= CURIYR                  ! for brevity

        DO 151 r= 1, MNUMCR-2
         QELCM(r,y)= QSELCM (r,y)
         QNGCM(r,y)= QSNGCM (r,y)
         QDSCM(r,y)= QSDSCM (r,y)
         QRSCM(r,y)= QSRSCM (r,y)
         QLGCM(r,y)= QSLGCM (r,y)
         QPRCM(r,y)= QSLGCM (r,y)
         QCLCM(r,y)= QSCLCM (r,y)
         QMGCM(r,y)= QSMGCM (r,y)
         QKSCM(r,y)= QSKSCM (r,y)
         QBMCM(r,y)= QSBMCM (r,y) 
!        QSTCM(r,y)= QSSTCM (r,y)
!        QPVCM(r,y)= QSPVCM (r,y)
         QTPCM(r,y)= QSTPCM (r,y)
         QTRCM(r,y)= QSTRCM (r,y)
         QTSCM(r,y)= QSTSCM (r,y)

151     CONTINUE

          QELCM(MNUMCR,y)= QSELCM(MNUMCR,y) ! US Tot
          QNGCM(MNUMCR,y)= QSNGCM(MNUMCR,y)
          QDSCM(MNUMCR,y)= QSDSCM(MNUMCR,y)
          QRSCM(MNUMCR,y)= QSRSCM(MNUMCR,y)
          QLGCM(MNUMCR,y)= QSLGCM(MNUMCR,y)
          QPRCM(MNUMCR,y)= QSLGCM(MNUMCR,y)
          QCLCM(MNUMCR,y)= QSCLCM(MNUMCR,y)
          QMGCM(MNUMCR,y)= QSMGCM(MNUMCR,y)
          QKSCM(MNUMCR,y)= QSKSCM(MNUMCR,y)
          QBMCM(MNUMCR,y)= QSBMCM(MNUMCR,y)
!         QSTCM(MNUMCR,y)= QSSTCM(MNUMCR,y)
!         QPVCM(MNUMCR,y)= QSPVCM(MNUMCR,y)
          QTPCM(MNUMCR,y)= QSTPCM(MNUMCR,y)
          QTRCM(MNUMCR,y)= QSTRCM(MNUMCR,y)
          QTSCM(MNUMCR,y)= QSTSCM(MNUMCR,y)

      RETURN
      END IF! Current Year less than first model year, return SEDS values

      IF (FIRSYR .GT. CMFirstYr) RETURN

      IF (CURIYR.EQ.CMFirstYr .AND. CURITR.EQ.1) THEN ! Initialize


       CMServices(1)= 'Heating'
       CMServices(2)= 'Cooling'
       CMServices(3)= 'Water Heating'
       CMServices(4)= 'Ventilation'
       CMServices(5)= 'Cooking'
       CMServices(6)= 'Lighting'
       CMServices(7)= 'Refrigeration'
       CMServices(8)= 'Office Equip-PCs'
       CMServices(9)= 'Office Equip-NonPCs'
       CMServices(10)='Other'

       CMMajor_Fuels(1)='Electricity'
       CMMajor_Fuels(2)='Natural Gas'
       CMMajor_Fuels(3)='Distillate'
       CMMinor_Fuels(1)='Residual'
       CMMinor_Fuels(2)='LPG'
       CMMinor_Fuels(3)='Steam Coal'
       CMMinor_Fuels(4)='Motor Gas'
       CMMinor_Fuels(5)='Kerosene'

       CMRegions(1)= 'New England'
       CMRegions(2)= 'Middle Atlantic'
       CMRegions(3)= 'East North Central'
       CMRegions(4)= 'West North Central'
       CMRegions(5)= 'South Atlantic'
       CMRegions(6)= 'East South Central'
       CMRegions(7)= 'West South Central'
       CMRegions(8)= 'Mountain'
       CMRegions(9)= 'Pacific'
       CMRegions(10)='California'
       CMRegions(11)='United States'

       CMAllFuels(1)='Electricity'
       CMAllFuels(2)='Natural Gas'
       CMAllFuels(3)='Distillate'
       CMAllFuels(4)='Residual'
       CMAllFuels(5)='LPG'
       CMAllFuels(6)='Steam Coal'
       CMAllFuels(7)='Motor Gas'
       CMAllFuels(8)='Kerosene'

       CMEmissionName (1)= 'Carb'
       CMEmissionName (2)= 'CO'
       CMEmissionName (3)= 'CO2'
       CMEmissionName (4)= 'SOx'
       CMEmissionName (5)= 'NOx'
       CMEmissionName (6)= 'VOC'
       CMEmissionName (7)= 'Meth'
       CMEmissionName (8)= 'Part'

       BldgName(1)=  'Assembly'
       BldgName(2)=  'Education'
       BldgName(3)=  'Food Sales'
       BldgName(4)=  'Food Service'
       BldgName(5)=  'Health Care'
       BldgName(6)=  'Lodging'
       BldgName(7)=  'Office-Large'
       BldgName(8)=  'Office-Small'
       BldgName(9)=  'Merc/Service'
       BldgName(10)= 'Warehouse'
       BldgName(11)= 'Other'

!   Open KDEBUG and KQUANT if PRTDBGK run option set to 1

        IF (PRTDBGK.EQ.1) THEN
          RCDBG= FILE_MGR ('O','KDEBUG',.TRUE.)
         RCQUAN= FILE_MGR ('O','KQUANT',.TRUE.)
         WRITE (RCDBG,100) SCEN, DATE
        END IF

! Open Commercial I/O files:
!
!   Commercial Parameter File
!     Initialize arrays and variables in case data set is incomplete
!

        dbgr= 0   ! initialize switches to defaults prior to reading
        dbgb= 0
        dbgs= 0
        dbgy1= 0
        dbgy2= 0

        BaseYrPCShrofOffEqEUI = 0.0
        ReportOption = 0
        CoolingTechIndexHP= 0
        DecayBM = 0
        LastDecayYr = 0
        STRetBehav = 0
        CostTrendSwitch = 0
        RCPRM= FILE_MGR ('O','KPARM',.FALSE.)
        IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KPARM data set error trapping:'
!
        count = 0
        NumErr = 0
        InputCount = 14  !Number of rows of data inputs to be read in from KPARM (to test for premature EOF)

        ! read key options and certain variables from KPARM:
        READ (RCPRM,'(99(/))')   ! skip header
        count = count + 1
        READ (RCPRM,*,ERR=160,END=172,IOSTAT=IOS) &
                      dbgr, dbgb, dbgs, dbgy1, dbgy2
         GO TO 161
160      CONTINUE
         NumErr = NumErr + 1
         IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'Comm_KPARM read err',IOS,' on record', &
                          count,'; skip record and continue read.'
161     CONTINUE
        READ (RCPRM,'(/)')   ! skip two lines after last read
        count = count + 1
        READ (RCPRM,*,ERR=162,END=172,IOSTAT=IOS) &
                      BaseYrPCShrofOffEqEUI
         GO TO 163
162      CONTINUE
         NumErr = NumErr + 1
         IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'Comm_KPARM read err',IOS,' on record', &
                          count,'; skip record and continue read.'
163     CONTINUE
        READ (RCPRM,'(/)')   ! skip two lines after last read
        count = count + 1
        READ (RCPRM,*,ERR=164,END=172,IOSTAT=IOS) &
                      ReportOption  ! KRPT switch
         GO TO 165
164      CONTINUE
         NumErr = NumErr + 1
         IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'Comm_KPARM read err',IOS,' on record', &
                          count,'; skip record and continue read.'
165     CONTINUE

        READ (RCPRM,'(/)')   ! skip two lines after last read
        count = count + 1
        READ (RCPRM,*,ERR=166,END=172,IOSTAT=IOS) &
                      CoolingTechIndexHP ! cooling equip comparable
                                         ! to heatpumps
         GO TO 167
166      CONTINUE
         NumErr = NumErr + 1
         IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'Comm_KPARM read err',IOS,' on record', &
                          count,'; skip record and continue read.'
167     CONTINUE

        READ (RCPRM,'(/)')   ! skip two lines after last read
        count = count + 1
        READ (RCPRM,*,ERR=168,END=172,IOSTAT=IOS) &
                      DecayBM ! Benchmarking to ramp STEO
                              ! mistie down to 0

        GO TO 169
168     CONTINUE
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
         WRITE(RCDBG,*) 'Comm_KPARM read err',IOS,' on record', &
                          count,'; skip record and continue read.'
169     CONTINUE

        READ (RCPRM,'(/)')   ! skip two lines after last read
        count = count + 1
        READ (RCPRM,*,ERR=170,END=172,IOSTAT=IOS) &
                      LastDecayYr ! Yr where the final STEO
                                  ! mistie is ramped down to 0
        READ (RCPRM,'(///////)')   ! skip 7 lines after last read      ! sw10-95
        READ (RCPRM,*) SDReportOpts(1)                                 ! sw10-95
        READ (RCPRM,'(/)')   ! skip line after last read               ! sw10-95
        READ (RCPRM,*) SDReportOpts(2)                                 ! sw10-95

        READ (RCPRM,'(/)')   ! skip two lines after last read
        READ (RCPRM,*) STRetBehav

        READ (RCPRM, '(/)')  ! skip 2 lines after last read
        READ (RCPRM,*) CostTrendSwitch

        READ (RCPRM, '(/)')  ! skip 2 lines after last read
        READ (RCPRM,*) ComSTEOBM  ! Switch to benchmark to STEO for STEO forecast years
                                  ! Both STEOBM in SCEDES file and ComSTEOBM in KPARM.txt
                                  ! must be 1 for benchmarking to occur
        READ (RCPRM, '(//)') ! skip 3 lines after last read
        READ (RCPRM,*) NoHist   ! Switch to turn off all historical
                                  ! and STEO calibration. Used to check
                                  ! pure model results - potentially
                                  ! obtain couunterfactual run.
        READ (RCPRM, '(//)') ! skip 3 lines after last read
        READ (RCPRM,*) KSTEOCLS   ! Switch to benchmark to within
                                  ! 2 percent of STEO value for last
                                  ! year of STEO forecast for Natural
                                  ! Gas, Distillate, Residual, and
                                  ! Kerosene. Must be set to 1 to activate.
        READ (RCPRM, '(//)')                                             ! elastruns
        READ (RCPRM,*,ERR=170,END=172,IOSTAT=IOS)                      & ! elastruns
                       ModYear, EndModYear, ELfactor, NGfactor, DSfactor ! elastruns
      READ (RCPRM, '(/)')                                                ! PITC
        READ (RCPRM,*,ERR=170,END=172,IOSTAT=IOS) IFMAX                  ! PITC
                                                                         ! PITC
       do f=1,3                                                          ! PITC
       iforward(f)=0                                                     ! PITC
       enddo                                                             ! PITC
                                                                         ! elastruns
! Overwrite prices in the common block for elasticity runs.              ! elastruns
                                                                         ! elastruns
             DO y=ModYear,EndModYear                                     ! elastruns
               DO r= 1, MNUMCR-2                                         ! elastruns
          PELCM (r,y) =  PELCM (r,y) * ELfactor                          ! elastruns
          PELME (r,y) =  PELME (r,y) * ELfactor                          ! elastruns
!          PELCM (r,y) =  PELCM(r,ModYear)**2 /PELCM(r,y)                ! elastruns
          DO s= 1, CMnumServ                                             ! elastruns
           PELCMOUT(r,y,s)=PELCMOUT(r,y,s) * Elfactor                    ! elastruns
!           PELCMOUT(r,y,s)=PELCMOUT(r,ModYear,s)**2 / PELCMOUT(r,y,s)   ! elastruns
                                                                         ! elastruns
          Enddo  ! s                                                     ! elastruns
                                                                         ! elastruns
                PNGCM (r,y) =  PNGCM (r,y) * NGfactor                    ! elastruns
                PDSCM (r,y) =  PDSCM (r,y) * DSfactor                    ! elastruns
         ENDDO  ! r                                                      ! elastruns
             ENDDO  ! y                                                  ! elastruns
                                                                         ! elastruns
             IF(PRTDBGK.EQ.1)                                          & ! elastruns
          WRITE(RCDBG,*) 'Price Adjustment Factors:',                  & ! elastruns
                 ModYear, EndModYear, ELfactor, NGfactor, DSfactor       ! elastruns


        GO TO 171
170      CONTINUE
         NumErr = NumErr + 1
         IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'Comm_KPARM read err',IOS,' on record', &
                          count,'; skip record and continue read.'
171     CONTINUE

        GO TO 173
172     NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) Write(RCDBG,*)'KPARM EOF reached prematurely.'

173     CONTINUE           ! EOF reached in KPARM when expected
        ! Close KPARM
        RCPRM= FILE_MGR ('C','KPARM',.FALSE.)

        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,174)
174       FORMAT(/,' KPARM data set error trapping complete!',/)
         ENDIF

        ! Open KRPT if KPARM option ReportOption set to 1:
        IF (ReportOption.EQ.1) &
           RCRPT= FILE_MGR ('O','KRPT',.TRUE.)

      END IF ! First call initialization


      ! Main work:

      CALL COMFloorspace       (RCDBG)

      CALL COMServiceDemand    (RCDBG)

      Call cdistgen (curiyr,curitr,rcrpt,rcdbg)

      CALL COMTechnologyChoice (RCDBG)

      CALL COMConsumption      (RCDBG)

      CALL COMBenchmarking     (RCDBG,RCQUAN)

      IF(EPA111D.EQ.1) CALL CALC111D (RCDBG)    !111(d)

      IF (CURIYR.EQ.LASTYR .AND. FCRL.EQ.1)                           & 
         CALL COMReport (RCDBG,RCRPT,SDReportOpts,ReportOption) 
		  
		  !IF(EPA111D.EQ.1) CALL CALC111D (RCDBG)    !111(d)
	  CALL CALC111D		(RCDBG) !EE costs

! FORMAT Statements
100   FORMAT ('Commercial Model Debug file',/,'Run Scenario= ',A10,/, &
         'Run Datekey= ',A8,/)

      END

!*****


      SUBROUTINE COMFloorspace (RCDBG)
      IMPLICIT NONE

      INTEGER FILE_MGR  ! External NEMS routine
      include'parametr' ! nems system parameters
      include'ncntrl'   ! nems run-time control parameters
      include'macout'   ! nems macroeconomic module linkage
      include'comparm'  ! commercial module parameters 'partial'
      include'comvars'  ! commercial module variable declarations
      include'commrep'  ! nems ftab report variables

      REAL*4 CMSurvRate   ! Surviving Proportion Logistic Function
      REAL*4 SurvFraction
      INTEGER*4 OrigYear  ! Original stock construction year
      REAL*4 BldgAge      ! Age (years) of surviving floorspace
      INTEGER*4 IYR,IBLDTP,IREG,IVINT,IOS,count,NumErr
      INTEGER*4 y,   & ! NEMS forecast year index (1 -> 1990)
                b,   & ! NEMS-CBECS building type index
                r   ! Census Division index
      INTEGER*4 RC1, RCL1, RCDBG  ! Miscellaneous file handles
      REAL*4 ConstFlrGrowth  ! Rate to use for Floorspace Growth (Special)
      ! Names of NEMS' Macroeconomic Activity Module (MAM) building types:
      INTEGER*2 bMAMamusement_religion    /1/, &
                bMAMeducation             /2/, &
                bMAMhealthcare            /3/, &
                bMAMhotel_dorm            /4/, &
                bMAMoffice                /5/, &
                bMAMautomotive            /6/, &
                bMAMstores                /7/, &
                bMAMwarehouse             /8/, &
                bMAMpublic_miscNR         /9/

      ! Names of CBECS - COMM Building Types and index values:
      INTEGER*2 bAssembly          /1/, &
                bEducation         /2/, &
                bFoodSales         /3/, &
                bFoodService       /4/, &
                bHealthCare        /5/, &
                bLodging           /6/, &
                bLargeOffice       /7/, &
                bSmallOffice       /8/, &
                bMercService       /9/, &
                bWarehouse        /10/, &
                bOther            /11/


      y= CURIYR  ! for brevity in array expressions

      ! On first pass through, initialize from files:
      IF (CURIYR.EQ.CMFirstYr .AND. CURITR.EQ.1) THEN

! Read Existing Floorspace from most recent CBECS survey.
! CBECSyear is a parameter set to the year of the survey.
! The data is read into the CBECSFlrSpc array temporarily, then
! backcast to original year of construction.

!   Initialize array in case data set is incomplete

       DO IBLDTP= 1, CMnumBldg
        DO IVINT= 1, CMnumBldgVint
         DO IREG= 1, MNUMCR-2
          CBECSFlrSpc (IREG,IBLDTP,IVINT)= 0.0
         END DO  ! IREG
        END DO  ! IVINT
       END DO  ! IBLDTP

       RC1= FILE_MGR ('O','KFLSPC',.FALSE.)
       IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KFLSPC data set error trapping:'
!
       count = 0
       NumErr = 0
       READ (RC1,'(99(/))')   ! skip header
       DO 115 IBLDTP= 1, CMnumBldg
        DO 115 IVINT= 1, CMnumBldgVint
        count = count + 1
        READ (RC1,*,ERR=114,END=116,IOSTAT=IOS) &
             (CBECSFlrSpc (IREG,IBLDTP,IVINT), IREG= 1, MNUMCR-2)
        GO TO 115
!
 114      CONTINUE ! Report read errors and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KFLSPC read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 115    CONTINUE
        GO TO 117  ! EOF reached in KFLSPC when expected
 116    CONTINUE   ! EOF reached in KFLSPC prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KFLSPC EOF reached prematurely at b =' &
                     ,IBLDTP,' v =',IVINT
 117    CONTINUE
        RC1= FILE_MGR ('C','KFLSPC',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,118)
118       FORMAT(/,' KFLSPC data set error trapping complete!',/)
         ENDIF


! Read Building Vintages (CMVintage(region,building type,vintage cohort)).
! = Median construction year of each age cohort group. The CBECS age cohorts
! are: Pre 1920, 1920-1945, 1946-1959, 1960-1969, 1970-1979, 1980-1989, 1990-1999
!		2000-2003, 2004-2007, 2008-2012
!
!   Initialize array in case file is incomplete
!
       DO IREG= 1, MNUMCR-2
         DO IBLDTP= 1, CMnumBldg
           DO IVINT= 1, CMnumBldgVint
             CMVintage(IREG,IBLDTP,IVINT)=CMOldestBldgVint
           END DO  ! IVINT
         END DO  ! IBLDTP
       END DO  ! IREG

       IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KVINT data set error trapping:'
       RC1= FILE_MGR ('O', 'KVINT',.FALSE.)
       count = 0
       NumErr = 0
       READ (RC1,'(99(/))')   ! skip header
       DO 125 IBLDTP= 1, CMnumBldg
        DO 125 IREG= 1, MNUMCR-2
         count = count + 1
         READ (RC1,*,ERR=124,END=126,IOSTAT=IOS) &
          (CMVintage(IREG,IBLDTP,IVINT),IVINT=1,CMnumBldgVint)
          DO IVINT=1,CMnumBldgVint
           IF(CMVintage(IREG,IBLDTP,IVINT) .LT. CMOldestBldgVint) THEN
             NumErr = NumErr + 1
             IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'CMVintage(',IREG,IBLDTP, &
              IVINT,') =',CMVintage(IREG,IBLDTP,IVINT),' <', &
              CMOldestBldgVint,'.  Code sets to',CMOldestBldgVint
             CMVintage(IREG,IBLDTP,IVINT) = CMOldestBldgVint
           END IF
          END DO
          GO TO 125
!
 124      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KVINT read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 125    CONTINUE
        GO TO 127  ! EOF reached in KVINT when expected
 126    CONTINUE   ! EOF reached in KVINT prematurely; write msg
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KVINT EOF reached prematurely at r =' &
                ,IREG,' b = ',IBLDTP
 127    CONTINUE
        RCL1= FILE_MGR ('C','KVINT',.FALSE.)
!
        IF(PRTDBGK.EQ.1) THEN
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,128)
 128      FORMAT(/,' KVINT data set error trapping complete!',/)
        ENDIF

! Read parameters for Floorspace Logistic Survival Function:
!
!   Initialize array in case data set is incomplete
!
       DO IBLDTP= 1, CMnumBldg
         CMAvgAge (IBLDTP) = 0.0
         CMGamma (IBLDTP) = 0.0
       END DO
       RC1= FILE_MGR ('O','KBLDG',.FALSE.)
       READ (RC1,'(99(/))')  ! Skip over header
       IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KBLDG data set error trapping:'
!
       count = 0
       NumErr = 0
       count = count + 1
       READ (RC1,*,ERR=26,END=30,IOSTAT=IOS) &
         (CMAvgAge (IBLDTP), IBLDTP= 1, CMnumBldg)
      GO TO 27
  26  CONTINUE
      NumErr = NumErr + 1
      IF(PRTDBGK.EQ.1) &
       WRITE(RCDBG,*) 'Comm_KBLDG read err',IOS,' on record', &
                       count,'; skip record and continue read.'
  27  CONTINUE
      READ (RC1,*)
      count = count + 1
      READ (RC1,*,ERR=28,END=30,IOSTAT=IOS) &
            (CMGamma (IBLDTP), IBLDTP= 1, CMnumBldg)
      GO TO 29
  28  CONTINUE
      NumErr = NumErr + 1
      IF(PRTDBGK.EQ.1) &
       WRITE(RCDBG,*) 'Comm_KBLDG read err',IOS,' on record', &
                       count,'; skip record and continue read.'
  29  CONTINUE
      GO TO 31
  30  NumErr = NumErr + 1
      IF(PRTDBGK.EQ.1) Write(RCDBG,*)'KBLDG EOF reached prematurely.'

  31  CONTINUE           ! EOF reached in KBLDG when expected
       RC1= FILE_MGR ('C','KBLDG',.FALSE.)

        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,32)
 32       FORMAT(/,' KBLDG data set error trapping complete!',/)
         ENDIF

!Initializing cmnewfloorspace 

		CMNewFloorSpace=0.0    !Zero out for new CBECS kk
		
!  Calculate CBECS original stock for vintage years

      DO 130 IREG= 1, MNUMCR-2
       DO 130 IBLDTP= 1, CMnumBldg
        DO 130 IVINT= 1, CMnumBldgVint
         OrigYear= CMVintage (IREG,IBLDTP,IVINT)
         ! Backcast from survey year to construction year:
         BldgAge= CBECSyear - OrigYear
         SurvFraction= CMSurvRate (IREG,IBLDTP,BldgAge)
         CMNewFloorSpace (IREG,IBLDTP,OrigYear-(BASEYR-1)) = &
             CBECSFlrSpc (IREG,IBLDTP,IVINT) / SurvFraction
130   CONTINUE

!  Aggregate across vintage cohorts to initialize total floorspace
!  array for the CBECS year.  This is required for the calculation
!  of new floorspace in the first forecast year.
      DO r= 1, MNUMCR-2
       ! Initialize Census Division total for CBECS survey year
       CMTotalFlspc (r,CMnumBldg+1,CBECSyear-(BASEYR-1))= 0.0
       DO b= 1, CMnumBldg
        CMTotalFlspc (r,b,CBECSyear-(BASEYR-1))= 0.0  ! Initialize
        DO IVINT= 1, CMnumBldgVint
         CMTotalFlspc (r,b,CBECSyear-(BASEYR-1))= &
        CMTotalFlspc(r,b,CBECSyear-(BASEYR-1))+CBECSFlrSpc(r,b,IVINT)
        END DO ! IVINT
        ! Add floorspace for this b into Census Division total:
        CMTotalFlspc (r,CMnumBldg+1,CBECSyear-(BASEYR-1))= &
         CMTotalFlspc (r,CMnumBldg+1,CBECSyear-(BASEYR-1)) &
         + CMTotalFlspc (r,b,CBECSyear-(BASEYR-1))
       END DO  ! b
      END DO   ! r


!  Aggregate floorspace for reporting purposes:
      DO 90 IBLDTP= 1, CMnumBldg
       CBECSFlrRxB (MNUMCR,IBLDTP)= 0.0
       DO 90 IREG= 1, MNUMCR-2
        CBECSFlrRxB (IREG,IBLDTP)= 0.0
        DO 85 IVINT= 1, CMnumBldgVint
         CBECSFlrRxB (IREG,IBLDTP)= CBECSFlrRxB (IREG,IBLDTP) + &
                                    CBECSFlrSpc (IREG,IBLDTP,IVINT)
   85   CONTINUE
       CBECSFlrRxB (MNUMCR,IBLDTP)= CBECSFlrRxB (MNUMCR,IBLDTP) + &
                                    CBECSFlrRxB (IREG,IBLDTP)
   90 CONTINUE

      END IF
! >>>>> End of floor space initialization during first forecast year


!  Calculate surviving, new, and total floorspace for this year:

      DO 100 r= 1, MNUMCR-2


!  Surviving floorspace is calculated by considering the proportion
!  of original new stock from each prior year that survives into
!  the current year:

       DO 140 IBLDTP= 1, CMnumBldg
        SurvFloorTotal (r,IBLDTP,CURIYR) = 0.0
        AgedNew (r,IBLDTP,CURIYR) = 0.0

!       Accumulate survivors from each original stock year:
        DO 140 IYR= CMOldestBldgVint-(BASEYR-1), CURIYR-1
         BldgAge= CURIYR - IYR
         SurvFraction= CMSurvRate (r,IBLDTP,BldgAge)
         SurvFloorTotal (r,IBLDTP,CURIYR) = &
              SurvFloorTotal (r,IBLDTP,CURIYR) &
            + CMNewFloorSpace (r,IBLDTP,IYR) * SurvFraction

         IF (IYR .GT. CBECSyear-(BASEYR-1)) THEN ! for post-CBECS yr stock
           AgedNew (r,IBLDTP,CURIYR) = &
              AgedNew (r,IBLDTP,CURIYR) &
            + CMNewFloorSpace (r,IBLDTP,IYR) * SurvFraction
         END IF

140    CONTINUE


!  New floorspace is calculated using the results of the surviving CBECS floorspace calculation above and floorspace growth rates
!  forecast from the NEMS U.S. Macroeconomic Activity Module (MAM).  The calculation is performed based on the assumption that
!  the total CBECS-equivalent floorspace for a given building type and Census division should change at the same rate as the MAM.

!  If the calculation yields a negative value for new floorspace, a value of zero is used.

!  Certain assumptions regarding the mapping of MAM building types to CBECS building types are embodied in mapping
!  coefficients, documented below where they occur. In this version of the floorspace calculation, coefficients appear in the
!  calculations explicitly only where required, but all are retained in the comments in the event they are required subsequently.

!  The floorspace units within the CDM are million square feet, unless otherwise specifically noted.
!  MC_COMMFLSP values are in terms of growth rate from previous to current year rather than square feet.

!  Calculate new floorspace for this Census division and year.

!  Map growth rates of MAM building types to NEMS building types to calculate new and total floorspace growth from CBECS base year

!IF (CURIYR.EQ.CMFirstYr .AND. CURITR.EQ.1) THEN

!  CBECS Assembly floorspace is assumed to grow at the rate of
!  MAM Amusement & Religious floorspace.
       CMNewFloorSpace (r,bAssembly,y) = CMTotalFlspc (r,bAssembly,y-1) * (MC_COMMFLSP (r,bMAMamusement_religion,y) +1) - SurvFloorTotal (r,bAssembly,y)

       IF (CMNewFloorSpace (r,bAssembly,y) .LT. 0.0) &
           CMNewFloorSpace (r,bAssembly,y) = 0.0

       CMTotalFlspc (r,bAssembly,y) = CMNewFloorSpace (r,bAssembly,y) + SurvFloorTotal (r,bAssembly,y)

!  CBECS Education floorspace is assumed to grow at the rate of
!  MAM Education floorspace.
       CMNewFloorSpace (r,bEducation,y) = CMTotalFlspc (r,bEducation,y-1) * (MC_COMMFLSP (r,bMAMeducation,y) +1) - SurvFloorTotal (r,bEducation,y)

       IF (CMNewFloorSpace (r,bEducation,y) .LT. 0.0) &
           CMNewFloorSpace (r,bEducation,y) = 0.0

       CMTotalFlspc (r,bEducation,y) = CMNewFloorSpace (r,bEducation,y) + SurvFloorTotal (r,bEducation,y)

!  CBECS Food Sales floorspace is assumed to grow at the rate of 0.0691 times
!  MAM Stores floorspace.                                   flmap
       CMNewFloorSpace (r,bFoodSales,y) = CMTotalFlspc (r,bFoodSales,y-1) * (MC_COMMFLSP (r,bMAMstores,y) +1) - SurvFloorTotal (r,bFoodSales,y)

       IF (CMNewFloorSpace (r,bFoodSales,y) .LT. 0.0) &
           CMNewFloorSpace (r,bFoodSales,y) = 0.0

       CMTotalFlspc (r,bFoodSales,y) = CMNewFloorSpace (r,bFoodSales,y) + SurvFloorTotal (r,bFoodSales,y)

!  CBECS Food Service floorspace is assumed to grow at the rate of 0.0911 times
!  MAM Stores floorspace.                                  flmap
       CMNewFloorSpace (r,bFoodService,y) = CMTotalFlspc (r,bFoodService,y-1) * (MC_COMMFLSP (r,bMAMstores,y) +1) - SurvFloorTotal (r,bFoodService,y)

       IF (CMNewFloorSpace (r,bFoodService,y) .LT. 0.0) &
           CMNewFloorSpace (r,bFoodService,y) = 0.0

       CMTotalFlspc (r,bFoodService,y) = CMNewFloorSpace (r,bFoodService,y) + SurvFloorTotal (r,bFoodService,y)

!  CBECS Health care floorspace is assumed to grow at the rate of 0.4595 times
!  MAM Health Care floorspace.                flmap
       CMNewFloorSpace (r,bHealthCare,y) = CMTotalFlspc (r,bHealthCare,y-1) * (MC_COMMFLSP (r,bMAMhealthcare,y) +1) - SurvFloorTotal (r,bHealthCare,y)

       IF (CMNewFloorSpace (r,bHealthCare,y) .LT. 0.0) &
           CMNewFloorSpace (r,bHealthCare,y) = 0.0

       CMTotalFlspc (r,bHealthCare,y) = CMNewFloorSpace (r,bHealthCare,y) + SurvFloorTotal (r,bHealthCare,y)

!  CBECS Lodging floorspace is assumed to grow at the rate of the sum of
!  0.8071 times MAM Hotel & Dormitory and 0.1929 times MAM Health care floorspace growth.
       CMNewFloorSpace (r,bLodging,y) = CMTotalFlspc (r,bLodging,y-1) &
        * (0.8071*MC_COMMFLSP (r,bMAMhotel_dorm,y) + &
         0.1929*MC_COMMFLSP(r,bMAMhealthcare,y) +1) &
        - SurvFloorTotal (r,bLodging,y)

       IF (CMNewFloorSpace (r,bLodging,y) .LT. 0.0) &
           CMNewFloorSpace (r,bLodging,y) = 0.0

       CMTotalFlspc (r,bLodging,y) = CMNewFloorSpace (r,bLodging,y) + SurvFloorTotal (r,bLodging,y)

!  CBECS Large Office floorspace is assumed to grow at the rate of the sum of
!  0.9466 times MAM Office and 0.0534 MAM Health care floorspace growth.     flmap
       CMNewFloorSpace (r,bLargeOffice,y) = &
        CMTotalFlspc (r,bLargeOffice,y-1) &
        * (0.9466*MC_COMMFLSP (r,bMAMoffice,y) + &
          0.0534*MC_COMMFLSP (r,bMAMhealthcare,y) +1) &
        - SurvFloorTotal (r,bLargeOffice,y)

       IF (CMNewFloorSpace (r,bLargeOffice,y) .LT. 0.0) &
           CMNewFloorSpace (r,bLargeOffice,y) = 0.0

       CMTotalFlspc (r,bLargeOffice,y) = CMNewFloorSpace (r,bLargeOffice,y) + SurvFloorTotal (r,bLargeOffice,y)

!  CBECS Small Office floorspace is assumed to grow at the rate of the sum of
!  0.8651 times MAM Office and 0.1349 Health care floorspace growth.
       CMNewFloorSpace (r,bSmallOffice,y) = &
        CMTotalFlspc (r,bSmallOffice,y-1) &
        * (0.8651*MC_COMMFLSP (r,bMAMoffice,y) + &
          0.1349*MC_COMMFLSP (r,bMAMhealthcare,y) +1) &
        - SurvFloorTotal (r,bSmallOffice,y)

       IF (CMNewFloorSpace (r,bSmallOffice,y) .LT. 0.0) &
           CMNewFloorSpace (r,bSmallOffice,y) = 0.0

       CMTotalFlspc (r,bSmallOffice,y) = CMNewFloorSpace (r,bSmallOffice,y) + SurvFloorTotal (r,bSmallOffice,y)

!  CBECS Mercantile & Service floorspace is assumed to grow at the rate of the sum of
!  0.1980 times MAM Automotive and 0.8020 times Stores floorspace growth.       flmap
       CMNewFloorSpace (r,bMercService,y) = CMTotalFlspc (r,bMercService,y-1) &
        * (0.1980*MC_COMMFLSP (r,bMAMautomotive,y) + &
          0.8020*MC_COMMFLSP (r,bMAMstores,y) +1) - SurvFloorTotal (r,bMercService,y)

       IF (CMNewFloorSpace (r,bMercService,y) .LT. 0.0) &
           CMNewFloorSpace (r,bMercService,y) = 0.0

       CMTotalFlspc (r,bMercService,y) = CMNewFloorSpace (r,bMercService,y) + SurvFloorTotal (r,bMercService,y)

!  CBECS Warehouse floorspace is assumed to grow at the rate of
!  MAM Warehouse floorspace.
       CMNewFloorSpace (r,bWarehouse,y) = CMTotalFlspc (r,bWarehouse,y-1) * (MC_COMMFLSP (r,bMAMwarehouse,y) +1) - SurvFloorTotal (r,bWarehouse,y)

       IF (CMNewFloorSpace (r,bWarehouse,y) .LT. 0.0) &
           CMNewFloorSpace (r,bWarehouse,y) = 0.0

       CMTotalFlspc (r,bWarehouse,y)= CMNewFloorSpace (r,bWarehouse,y) + SurvFloorTotal (r,bWarehouse,y)

!  CBECS Other floorspace is assumed to grow at the rate of
!  MAM Public & Miscellaneous Non-residential floorspace.
       CMNewFloorSpace (r,bOther,y) = CMTotalFlspc (r,bOther,y-1) * (MC_COMMFLSP (r,bMAMpublic_miscNR,y) +1) - SurvFloorTotal (r,bOther,y)

       IF (CMNewFloorSpace (r,bOther,y) .LT. 0.0) &
           CMNewFloorSpace (r,bOther,y) = 0.0

       CMTotalFlspc (r,bOther,y) = CMNewFloorSpace (r,bOther,y) + SurvFloorTotal (r,bOther,y)

!  Calculate total floorspace forecast for this Census division:
       CMTotalFlspc (r,CMnumBldg+1,y) = 0.0
       DO b= 1, CMnumBldg
        CMTotalFlspc (r,CMnumBldg+1,y) = CMTotalFlspc (r,CMnumBldg+1,y) + CMTotalFlspc (r,b,y)
       END DO ! b


! Special Floorspace Growth
! The following lines, from here through the line that
! begins with "! End of Special Floorspace Growth", represents
! a special capability for overriding the MACRO floorspace
! forecast, and using instead a constant growth rate that is
! uniformly applied to all buildings in the U.S..  That growth
! rate is specified by the ConstFlrGrowth variable following
! this description, where a value of, say, 0.1 represents
! a 10% annual growth rate.  Negative values may also be
! specified, but floorspace will decline no more than the
! death rate by building type would indicate.  A value of
! zero will cause just enough new floorspace to exactly
! offset the normal death rate decline.  To activate this
! special processing, replace the !s in column 1 with blanks,
! and set ConstFlrGrowth to the desired value.

!      ConstFlrGrowth= 0.0
!      DO b= 1, CMnumBldg
!
!       CMNewFloorSpace (r,b,y)= CMTotalFlspc (r,b,y-1) *
!                                  (1.0 + ConstFlrGrowth)
!                              - SurvFloorTotal (r,b,y)
!
!       IF (CMNewFloorSpace (r,b,y) .LT. 0.0)
!           CMNewFloorSpace (r,b,y) = 0.0
!
!       CMTotalFlspc (r,b,y)= CMnewFloorSpace (r,b,y)
!                           + SurvFloorTotal  (r,b,y)
!
!      END DO ! b

! End of Special Floorspace Growth

  100 CONTINUE  ! across Census Divisions


!  Compute National Floorspace Totals for NEMS FTAB Report & KRPT:

      IYR= CURIYR
      CMUSSurvFloorTot (IYR)= 0.0
      CMUSNewFloorTot  (IYR)= 0.0

      DO 185 IBLDTP= 1, CMnumBldg

       CMNewFlrSpace (IBLDTP,IYR)= 0.0
       CMSurvFloorTot (IBLDTP,IYR)= 0.0

       DO 180 IREG= 1, MNUMCR-2
        CMSurvFloorTot (IBLDTP,IYR)= CMsurvFloorTot (IBLDTP,IYR) + &
                         SurvFloorTotal (IREG,IBLDTP,IYR)
        CMNewFlrSpace (IBLDTP,IYR)= CMNewFlrSpace (IBLDTP,IYR) + &
                         CMNewFloorSpace  (IREG,IBLDTP,IYR)
  180  CONTINUE

       CMUSSurvFloorTot (IYR)= CMUSSurvFloorTot (IYR) + &
                               CMSurvFloorTot (IBLDTP,IYR)
       CMUSNewFloorTot (IYR)=  CMUSNewFloorTot  (IYR) + &
                               CMNewFlrSpace  (IBLDTP,IYR)
       SurvFloorTotal (MNUMCR,IBLDTP,IYR)= &
              CMSurvFloorTot (IBLDTP,IYR)
       CMNewFloorSpace (MNUMCR,IBLDTP,IYR)= &
                CMNewFlrSpace (IBLDTP,IYR)

  185 CONTINUE

      ! Convert totals from millions to billions of sq ft:
      CMUSSurvFloorTot (CURIYR)= CMUSSurvFloorTot (CURIYR) / 1000.0
      CMUSNewFloorTot  (CURIYR)= CMUSNewFloorTot  (CURIYR) / 1000.0
      DO IBLDTP= 1,CMnumBldg
       CMNewFlrSpace (IBLDTP,CURIYR)= &
        CMNewFlrSpace (IBLDTP,CURIYR) / 1000.0
       CMSurvFloorTot (IBLDTP,CURIYR)= &
        CMSurvFloorTot (IBLDTP,CURIYR) / 1000.0
      END DO


! ******* FORMAT STATEMENTS *****
102   FORMAT (/,'>>>>> Subroutine COMFloorspace called ', &
              I4,1X,'Iter ',I2)


      RETURN
      END

      SUBROUTINE COMServiceDemand (RCDBG)
      IMPLICIT NONE
      INTEGER  FILE_MGR
      include'parametr'
      include'ncntrl'
      include'comparm'
      include'comvars'
      include'commrep'       ! miscdetail - to use national flsp totals
      include'apq'
      include'macout'
!
      REAL*4 CMSurvRate, ReplacementProportion, sum
      REAL*4 Epsilon(2) !  Epsilon criteria
      INTEGER*4 IREG, I, I2, IBLDTP, IFUEL, IVINT, ISERV, IOS, FLAG
      INTEGER*4 RCODE1, RC1, RCL1, RCDBG, NumErr, RCL2
      INTEGER*4 r,       & !  Region (Census Division)
                b,       & !  Building type
                s,       & !  Service
                f,       & !  Fuel
                t,       & !  Technology
                v,       & !  Vintage
                y          !  Year (1 = 1990)


      REAL*4 numerator     ! general holding variable for division
      REAL*4 denominator   ! holding variable to avoid division by 0
      REAL*4 CforStotal    ! holding var: total consump in r for s
      REAL*4 CforSrestrict ! consump in r for s in buildings a given
                           ! type of equip is restricted to
      INTEGER*4 BaseRestrict (CMnumBldg)  ! holding var to compute base yr
                           ! efficiencies - including hps   bashp

      INTEGER EPA111D      ! 111(d)
      INTEGER AB32SW
      REAL*4 ElTemp        ! placeholder used to skip QELRS and discard for 111(d) because both residential and commercial data are input from BLDBASE
      INTEGER DIV, YEAR    ! placeholder used to skip Census division and year labels in read-in from BLDBASE for 111(d) analysis

      INTEGER*4 infile     ! Reusable file handle
      INTEGER*4 count,KTEKACCNT      !  General purpose counter variable
      INTEGER*4 vmax       !  Largest model number in a technology
      CHARACTER*200 ISTR   !  holding variable for input record         awe
      CHARACTER*200 ISTR2  !  duplicate input rec                       awe
      INTEGER*4 InitYrTemp !  holding variable for TechAvailability(r,t,v,1) awe
      INTEGER*4 LastYrTemp !  holding variable for TechAvailability(r,t,v,2) awe
      REAL*4 markshar      !  holding variable for TechShareofService
      REAL*4 effic         !  holding variable for TechEff
      REAL*4 CRIforLighting    !Add CRI to TechChoice
      REAL*4 CCTemp        !  holding variable for TechCost(r,t,v,1)    awe
      REAL*4 OMTemp        !  holding variable for TechCost(r,t,v,2)    awe
      REAL*4 SBTemp        !  holding variable for TechCost(r,t,v,3)    investsub
      REAL*4 SB111dTemp    !  holding variable for TechCost(r,t,v,4)    investsub 111(d)
      REAL*4 SLTemp        !  holding variable for TechLife             awe
      REAL*4 RetroTemp     !  holding variable for RetroCostFract       awe
      CHARACTER*44 ENTemp  !  holding variable for EquipName            awe
      REAL*4 MinCost       !  holding variable for minimum cost
      INTEGER*4 tech       !  alternate variable for subscript t
      INTEGER*4 vint       !  alternate variable for subscript v
      INTEGER*4 TsubS      !  index into TechsforService array
      INTEGER*4 IYR        !  index                                     !endshel
      INTEGER*4 YRCHNG     !  index for year after which shell efficiency changes start   arra09
      REAL*4 ExistShBaseStock, ExistShBaseStockHt, ExistShBaseStockCl   !shlfactor
      REAL*4 TotNewFS, NewShAdj, NewShAdjHt, NewShAdjCl, NewShAdjCA, NewShAdjHtCA, NewShAdjClCA                 !shlcodes
      REAL*4 NewShlAvg, NewShlAvgHt, NewShlAvgCl, NewShlAvgHtCA, NewShlAvgClCA                        !shlfactor
      REAL*4 ExistImprv, NewImprv, TempAvg, TempAvgHt, TempAvgCl, NewImprvCA        !endshel-shlfactor
      REAL*4 TempAdj,TempAdjHt, TempAdjCl, TempAdjHtCA, TempAdjClCA                               !shlcodes
      REAL*4 ExistShlMax, ExistShlMaxHt, ExistShlMaxCl                  !shlfactor
       REAL*4     SSDnoShell(MnumCR-2,CMnumBldg,CMnumServ)                       !Efficiency Index
       REAL*4     RSDnoShell(MnumCR-2,CMnumBldg,CMnumServ)                       !Efficiency Index
       REAL*4     NSDnoShell(MnumCR-2,CMnumBldg,CMnumServ)                       !Efficiency Index
       REAL*4     SDbyFuel(CMNumMajFl,CMnumServ,CMnumBldg,MNUMCR-2,MNUMYR)       !Efficiency Index
      COMMON /EffInd/ SSDnoShell,RSDnoShell,NSDnoShell,SDbyFuel                  !Efficiency Index
      REAL*4 EffectHurdle
!      REAL*4 PriceDelta (CMnumMajFl) ! Change in fuel price relative to base yr
!      REAL*4 PriceDelta3(CMnumMajFl) !3-Year Avg change in price rel to base yr
      COMMON /ShellAdjustments/ NewShAdj                                !Add ACTShell

      REAL*4 SDCompositeElas ! composite SD elasticity factor
      Data Epsilon /.00001,.001/

      INTEGER*4 rbound  ! the r subscript in KTECH usually refers
                        ! to Census Division, but for lighting and
                        ! ventilation it represents the building
                        ! type index, and rbound is the appropriate
                        ! upper limit. Also for refrigeration.
      INTEGER*4 realr   ! for use with lighting and ventilation
                        ! data, where the b subscript is read
                        ! into r. Also for refrigeration.

!   Add Declarations for adjustment to intensities for data centers     !kkn
      REAL*4    DatCtrShare
      REAL*4    dcf(CMnumServ)
	  REAL*4	OfficePC(CNUMMELS)
	  REAL*4	OfficeNonPC(CNUMMELS)
	  REAL*4	MELsMisc(CNUMMELS)
	  REAL*4	OfficePCIndex(CNUMMELS)
	  REAL*4	OfficeNonPCIndex(CNUMMELS)
	  REAL*4	MELsMiscIndex(CNUMMELS)
	  REAL*4	FOFFICEPC
	  REAL*4	LOFFICEPC
	  REAL*4	FOFFICENONPC
	  REAL*4	LOFFICENONPC
	  REAL*4	FMISC
	  REAL*4	LMISC
	  REAL*4 MarketPenetrationMels(CNUMMELS,MNUMYR)								
		REAL*4 MelsElQ(CNUMMELS)

!   Add Declarations for detailed miscellaneous end use calculations       ! miscdetail
      COMMON /MiscEl/ CoffeeBrewers,XfmrsDry,Security,ElVehicles,        & ! miscdetail
                 KitchenVent,LabRefFrz,VidDisplay,LrgVidBoard,           & ! miscdetail
                 FumeHoods,Laundry,MedImaging,Elevators,Escalators,      & ! miscdetail
                 xmisccalc                                                 ! miscdetail
      REAL*4    CoffeeBrewers,XfmrsDry,Security,ElVehicles,              & ! miscdetail
                 KitchenVent,LabRefFrz,VidDisplay,LrgVidBoard,           & ! miscdetail
                 FumeHoods,Laundry,MedImaging,Elevators,Escalators         ! miscdetail
      REAL*4    xmisccalc    ! holding variable for CURIYR-14 to use in misc equations
      REAL*4    SurvFlrbsf,CMNewFlrbsf, BrewerFlrBase, LaundryFlrBase,   & ! miscdetail
                 MedFlrBase, ElevatorFlrBase, EscalatorFlrBase,          & ! miscdetail
                 TotFlrNoWhse, LabFlrBase, KitchenFlrBase                  ! miscdetail
      REAL*4    BaseElTotR                                                 ! miscdetail
      INTEGER*4 nb        !  alternate index for building type
       ! floorspace in current CD and building type in billion sf for calcs with national totals
      REAL*4    xplicitmiscshr(CMnumBldg)

      COMMON /MiscElQ/ CoffeeBrewersElQ,XfmrsDryElQ,SecurityElQ,         & ! miscdetail
                 KitchenVentElq,LabRefFrzElq,LrgVidBoardElq, & ! miscdetail
                 ElVehiclesElQ,FumeHoodsElQ,LaundryElQ,MedImagingElQ,    & ! miscdetail
                 ElevatorsElQ,EscalatorsElQ,TotExplicitMiscElQ             ! miscdetail
      REAL*4 CoffeeBrewersElQ(MNUMCR-2,CMnumBldg,MNUMYR)                   ! miscdetail
      REAL*4 XfmrsDryElQ(MNUMCR-2,CMnumBldg,MNUMYR)                        ! miscdetail
      REAL*4 SecurityElQ(MNUMCR-2,CMnumBldg,MNUMYR)                        ! miscdetail
      REAL*4 ElVehiclesElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 KitchenVentElq(MNUMCR-2,CMnumBldg,MNUMYR)                     ! miscdetail
      REAL*4 LabRefFrzElQ(MNUMCR-2,CMnumBldg,MNUMYR)                       ! miscdetail
      !REAL*4 VidDisplayElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 LrgVidBoardElQ(MNUMCR-2,CMnumBldg,MNUMYR)                     ! miscdetail
      REAL*4 FumeHoodsElQ(MNUMCR-2,CMnumBldg,MNUMYR)                       ! miscdetail
      REAL*4 LaundryElQ(MNUMCR-2,CMnumBldg,MNUMYR)                         ! miscdetail
      REAL*4 MedImagingElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 ElevatorsElQ(MNUMCR-2,CMnumBldg,MNUMYR)                       ! miscdetail
      REAL*4 EscalatorsElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 TotExplicitMiscElQ(MNUMCR-2,CMnumBldg,MNUMYR)                 ! miscdetail

      REAL*4 BASELINEBKWHCM                                             ! 111(d) - Stores restart file values of electricity consumption by Census division and year
      COMMON/BASE111DCM/BASELINEBKWHCM(MNUMCR,MNUMYR)                     ! 111(d)

      INTEGER*4 NRGBILL    ! switch for energy bill provisions: 0 - no bill, 1 - bill without unfunded provisions, 2 - bill with unfunded items - ebill
      INTEGER*4 NRG2007    ! switch for dec 2007 energy bill provisions: 0 - no bill, 1 - bill without unfunded provisions, 2 - bill with unfunded items - nrg07
      INTEGER*4 STIMULUS   ! switch for feb 2009 stimulus package provisions: 0 - no stimulus, 1 - with stimulus; default is with stimulus - arra09
      INTEGER*4 RTOVALUE   ! External routine to get options
      EXTERNAL RTOVALUE


!   Add Declarations for ktechwk1 read from lotus 123 file 
!   Now for ktek.xml read from xml file

      ! Follow Stephanie's change (INTEGER*4 -> INTEGER*2)
      INTEGER*2 maxrec        ! number of rows in the ktek.xml file SET and READ from KTEK.xml              !ktekxml

      INTEGER*2, Allocatable :: tch(:),vnt(:),reg(:),serv(:),fuel(:),yintro(:),ylast(:),life(:),retrofit(:) !ktechwk1
      INTEGER*2, Allocatable :: r1(:),r2(:),r3(:),r4(:),r5(:),r6(:),r7(:),r8(:),r9(:),r10(:),r11(:)         !ktechwk1
      INTEGER*2, Allocatable :: trndshape(:),trndstart(:)                                                   !ktechwk1
      CHARACTER*50, Allocatable :: techname(:)                                                              !ktechwk1
      CHARACTER*10, Allocatable :: trndtype(:)                                                              !ktechwk1
      REAL*4, Allocatable :: share(:),capcst(:),maintcst(:),subcst(:),sub111dcst(:),eff(:),trndpct(:),CRI(:)!ktechwk1 !Add CRI to TechChoice    !investsub 111(d)

!!!PH: Define variables for the new tech menu file
      INTEGER*2, Allocatable :: tch_new(:),vnt_new(:),reg_new(:),serv_new(:),fuel_new(:),yintro_new(:),ylast_new(:),life_new(:),retrofit_new(:) !ktechwk1
      INTEGER*2, Allocatable :: r1_new(:),r2_new(:),r3_new(:),r4_new(:),r5_new(:),r6_new(:),r7_new(:),r8_new(:),r9_new(:),r10_new(:),r11_new(:)         !ktechwk1
      INTEGER*2, Allocatable :: trndshape_new(:),trndstart_new(:)                                                   !ktechwk1
      CHARACTER*50, Allocatable :: techname_new(:)                                                              !ktechwk1
      CHARACTER*10, Allocatable :: trndtype_new(:)                                                              !ktechwk1
      REAL*4, Allocatable :: share_new(:),capcst_new(:),maintcst_new(:),subcst_new(:),sub111dcst_new(:),eff_new(:),trndpct_new(:),CRI_new(:)!ktechwk1 !Add CRI to TechChoice    !investsub 111(d)

          NRGBILL = RTOVALUE("NRGBILL  ",0)  ! Get energy bill switch value
          NRG2007 = RTOVALUE("NRG2007  ",0)  ! Get dec 2007 energy bill switch value nrg07
          STIMULUS = RTOVALUE("STIMULUS  ",0)! Get stimulus package switch value arra09
          EPA111D = RTOVALUE("EPA111D ",0)   ! SCEDES switch to activate subsidies for 111(d) analysis
          AB32SW = RTOVALUE("AB32SW  ",0)    ! SCEDES switch to activate subsidies for AB32SW analysis

      xmisccalc=FLOAT(CURIYR-(CMFirstYr-1))  !miscdetail

      IF (CURIYR.EQ.CMFirstYr .AND. CURITR.EQ.1) THEN
        Allocate (tch(KTMAX),vnt(KTMAX),reg(KTMAX),serv(KTMAX),fuel(KTMAX),yintro(KTMAX),ylast(KTMAX),life(KTMAX),retrofit(KTMAX))  !ktechwk1
        Allocate (r1(KTMAX),r2(KTMAX),r3(KTMAX),r4(KTMAX),r5(KTMAX),r6(KTMAX),r7(KTMAX),r8(KTMAX),r9(KTMAX),r10(KTMAX),r11(KTMAX))  !ktechwk1
        Allocate (trndshape(KTMAX),trndstart(KTMAX))                                                                                !ktechwk1
        Allocate (techname(KTMAX))                                                                                                  !ktechwk1
        Allocate (trndtype(KTMAX))                                                                                                  !ktechwk1
        Allocate (share(KTMAX),capcst(KTMAX),maintcst(KTMAX),subcst(KTMAX),sub111dcst(KTMAX),eff(KTMAX),cri(KTMAX),trndpct(KTMAX))  !ktechwk1    !investsub 111(d)

!!!PH: Define variables for the new tech menu file
        Allocate (tch_new(KTMAX),vnt_new(KTMAX),reg_new(KTMAX),serv_new(KTMAX),fuel_new(KTMAX),yintro_new(KTMAX),ylast_new(KTMAX),life_new(KTMAX),retrofit_new(KTMAX))  !ktechwk1
        Allocate (r1_new(KTMAX),r2_new(KTMAX),r3_new(KTMAX),r4_new(KTMAX),r5_new(KTMAX),r6_new(KTMAX),r7_new(KTMAX),r8_new(KTMAX),r9_new(KTMAX),r10_new(KTMAX),r11_new(KTMAX))  !ktechwk1
        Allocate (trndshape_new(KTMAX),trndstart_new(KTMAX))                                                                                !ktechwk1
        Allocate (techname_new(KTMAX))                                                                                                  !ktechwk1
        Allocate (trndtype_new(KTMAX))                                                                                                  !ktechwk1
        Allocate (share_new(KTMAX),capcst_new(KTMAX),maintcst_new(KTMAX),subcst_new(KTMAX),sub111dcst_new(KTMAX),eff_new(KTMAX),cri_new(KTMAX),trndpct_new(KTMAX))  !ktechwk1    !investsub 111(d)

!       Read technology database in order to compute base year
!       Service Demand Intensities from the input EUIs.

      ! Initialize arrays that will receive values from input files,
      ! but not necessarily into all array positions.  This can occur
      ! when reading the technology characterization table.
      ! Generally, initial values will be set to zero.  Costs,
      ! however, will be initialized to a large constant specified by
      ! MAXCOST, due to dependence of the technology choice solution
      ! algorithm on identifying least cost equipment.

       DO t= 1, CMnumTechs
        DO f= 1, CMnumMajFl
         FuelbyTech (t,f)= 0
         DO s= 1, CMnumServ
          TechbyService (s,t)= 0
          DO v= 1, CMnumEqVint
           TechbyModel(t,v)=0
           TechLife (t,v)= 0.0
           CostTrend (t,v)= " "
           CostParam1 (t,v)= 0
           CostParam2 (t,v)= 0
           CostParam3 (t,v)= 0.0
           DO r= 1, MNUMCR
            TechEff (r,s,t,v)= 0.0
            TechCRI (r,s,t,v) = 0.0   !CRI
            TechCost (r,t,v,1)= MAXCOST   !regionalize techcost
            TechCost (r,t,v,2)= MAXCOST   !regionalize techcost
            TechCost (r,t,v,3)= 0.0   !regionalize techcost    !investsub
            TechCost (r,t,v,4)= 0.0   !regionalize techcost    !investsub 111(d)
            TechAvailability (r,t,v,1)= 0 !regionalize tech availability
            TechAvailability (r,t,v,2)= 0 !regionalize tech availability
            DO b= 1, CMnumBldg
             TechShareofServiceBASE (r,b,s,t,v)= 0.0
             EquipRestriction (t,v,b,r)= 1
            END DO ! b
           END DO  ! r
          END DO   ! v
         END DO    ! s
        END DO     ! f
       END DO      ! t

!
!!!PH: read in commercial technology menu file with updated tech efficiency (stored in the input folder)
        infile= FILE_MGR ('O','KINTENS',.FALSE.)
        READ (infile,'(519(/))')  ! Skip over 520 line header

        KTEKACCNT = 0
        DO 62 count= 1, 5579
          READ(INFILE,*,ERR=39,END=33,IOSTAT=IOS) &
               TCH_new(count), VNT_new(count), REG_new(count), SERV_new(count), FUEL_new(count), &
               SHARE_new(count), EFF_new(count), CAPCST_new(count), MAINTCST_new(count), SUBCST_new(count), &
               SUB111DCST_new(count), LIFE_new(count), YINTRO_new(count), YLAST_new(count), RETROFIT_new(count), &
               TECHNAME_new(count), R1_new(count), R2_new(count), R3_new(count), R4_new(count), &
               R5_new(count), R6_new(count), R7_new(count), R8_new(count), R9_new(count), &
               R10_new(count), R11_new(count), TRNDTYPE_new(count), TRNDSTART_new(count), TRNDSHAPE_new(count), &
               TRNDPCT_new(count), CRI_new(count)
          KTEKACCNT=KTEKACCNT+1

62   CONTINUE                       ! updated KTEK read

!   PRINT READ ERR MESSAGE AND RETURN
39  CONTINUE
      WRITE(RCDBG,*) 'Comm_EFFICIENCY (updated) read error number for record: ', count

!   IF THE READ WAS SUCESSFUL, PRINT SUMMARY INFORMATION
33  CONTINUE
!   FIRST, CLOSE FILE AND USE RAW DATA TO ASSIGN POINTERS
      INFILE=FILE_MGR('C','KINTENS',.FALSE.)
      WRITE(RCDBG,*) 'Comm_EFFICIENCY (updated) REACHED OK; COUNT = ', KTEKACCNT

!       Read Technology data; print data errors to KDEBUG
!   Code for reading ktekx from a "xlsx" spreadsheet, ktekx.xlsx   
!    File name = ktekx.xlxs
!
!  OPEN WORKSHEET FILE USING FILE MANAGER
        infile= FILE_MGR ('O','KTEKX',.FALSE.)
!  CALL SUBROUTINE TO READ ALL DEFINED RANGES FROM WORKSHEET
!  This stores the ranges in a temporary data area that can
!  get overwritten by the next model if they use it.  So all
!  ranges have to be extracted from the temporary area immediately.
         CALL ReadRngXLSX(infile,'ktek')  ! read worksheet named 'ktek' in excel workbook ktekx.xlsx
!
!  CLOSE WORKSHEET FILE
          infile= FILE_MGR ('C','KTEKX',.FALSE.)

!***************************************************************
!
!  Copy each range from worksheet data area to variables
!  GETRNGI : Copies an Integer*2 variable from the worksheet
!            data area into the variable.  The variable
!            dimensions are passed as the 3rd,4th,&5th
!            arguments, (eg, ... 1,1,1).
!            A variable with dimesions of 1,1,1 is a scalar.
!            A variable with dimensions of 26,1,1 is a one-
!            dimensional array with 26 elements.
!  GETRNGR:  Copies a REAL variable from the worksheet
!            data area into the variable.
!  GETRNGC:  Copies a Character variable from the worksheet
!            data area into the variable 
!            (Max string length set in wk1block -- maxstrings=50)

!  READ IN RECORD COUNT FROM SPREADSHEET CALCULATION
     
       CALL GETRNGI('MAXREC          ',MAXREC   ,1,1,1)
         write (6,*)'maxrec', maxrec

!  NOW READ THE VECTORS OF KTEK, LATER TO BE PARSED INTO THE COMMERCIAL VARIABLES
!   DIMENSIONED BY TECH AND VINT, ETC
       CALL GETRNGI('TCH             ',TCH          ,maxrec,1,1)
       CALL GETRNGI('VNT             ',VNT          ,maxrec,1,1)
       CALL GETRNGI('REG             ',REG          ,maxrec,1,1)
       CALL GETRNGI('SERV            ',SERV         ,maxrec,1,1)
       CALL GETRNGI('FUEL            ',FUEL         ,maxrec,1,1)
       CALL GETRNGR('SHARE           ',SHARE        ,maxrec,1,1)
       CALL GETRNGR('EFFICIENCY      ',EFF          ,maxrec,1,1)
       CALL GETRNGR('CAPCST          ',CAPCST       ,maxrec,1,1)
       CALL GETRNGR('MAINTCST        ',MAINTCST     ,maxrec,1,1)
       CALL GETRNGR('SUBCST          ',SUBCST       ,maxrec,1,1)  !investsub
       CALL GETRNGR('SUB111DCST      ',SUB111DCST   ,maxrec,1,1)  !investsub 111(d)
       CALL GETRNGI('LIFE            ',LIFE         ,maxrec,1,1)
       CALL GETRNGI('YINTRO          ',YINTRO       ,maxrec,1,1)
       CALL GETRNGI('YLAST           ',YLAST        ,maxrec,1,1)
       CALL GETRNGI('RETROFIT        ',RETROFIT     ,maxrec,1,1)
       CALL GETRNGC('TECHNAME        ',TECHNAME     ,maxrec,1,1)
       CALL GETRNGI('RESTRICT1       ',R1           ,maxrec,1,1)
       CALL GETRNGI('RESTRICT2       ',R2           ,maxrec,1,1) 
       CALL GETRNGI('RESTRICT3       ',R3           ,maxrec,1,1)
       CALL GETRNGI('RESTRICT4       ',R4           ,maxrec,1,1)
       CALL GETRNGI('RESTRICT5       ',R5           ,maxrec,1,1)
       CALL GETRNGI('RESTRICT6       ',R6           ,maxrec,1,1)
       CALL GETRNGI('RESTRICT7       ',R7           ,maxrec,1,1)
       CALL GETRNGI('RESTRICT8       ',R8           ,maxrec,1,1)
       CALL GETRNGI('RESTRICT9       ',R9           ,maxrec,1,1)
       CALL GETRNGI('RESTRICT10      ',R10          ,maxrec,1,1)
       CALL GETRNGI('RESTRICT11      ',R11          ,maxrec,1,1)
       CALL GETRNGC('TRNDTYPE        ',TRNDTYPE     ,maxrec,1,1)
       CALL GETRNGI('TRNDSTART       ',TRNDSTART    ,maxrec,1,1)
       CALL GETRNGI('TRNDSHAPE       ',TRNDSHAPE    ,maxrec,1,1)
       CALL GETRNGR('TRNDPCT         ',TRNDPCT      ,maxrec,1,1)
       CALL GETRNGR('CRI             ',CRI          ,maxrec,1,1)  !Add CRI to TechChoice

        IF(PRTDBGK.EQ.1) write(RCDBG,*)'KTEK data set error trapping:'
        DO 60 count= 1, MaxRec

           t=tch(count)
           v=vnt(count)
           r=reg (count)
           s=serv(count)
           f=fuel(count)
           markshar=share(count)
           effic=EFF_new(count)
           cctemp=capcst(count)
           omtemp=maintcst(count)
           sbtemp=subcst(count)          !investsub
           sb111dtemp=sub111dcst(count)  !investsub 111(d)
           inityrtemp=yintro(count)
           lastyrtemp=ylast(count)
           sltemp=life(count)
           retrotemp=retrofit(count)
           entemp=techname(count)
           equiprestriction(t,v,1,r)=r1(count)
           equiprestriction(t,v,2,r)=r2(count)
           equiprestriction(t,v,3,r)=r3(count)
           equiprestriction(t,v,4,r)=r4(count)
           equiprestriction(t,v,5,r)=r5(count)
           equiprestriction(t,v,6,r)=r6(count)
           equiprestriction(t,v,7,r)=r7(count)
           equiprestriction(t,v,8,r)=r8(count)
           equiprestriction(t,v,9,r)=r9(count)
           equiprestriction(t,v,10,r)=r10(count)
           equiprestriction(t,v,11,r)=r11(count)
           costtrend(t,v)=trndtype(count)
           costparam1(t,v)=trndstart(count)
           costparam2(t,v)=trndshape(count)
           costparam3(t,v)=trndpct(count)
           CRIforLighting=cri(count)                !Add CRI to TechChoice

!   End Code for reading ktek from a "xml" spreadsheet

!
!    Check for t, v, r, s, f in bounds. If not, skip record.
!
          IF (t .LT. 1 .OR. t .GT. CMnumTechs) THEN
            IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 't=',t,' out of bounds, record',COUNT, &
                            ' skipped: '
            GO TO 57
          END IF
          IF (v .LT. 1 .OR. v .GT. CMnumEqVint) THEN
            IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 'v=',v,' out of bounds, record',COUNT, &
                            ' skipped: '
            GO TO 57
          ENDIF
          rbound= MNUMCR-2    ! the normal Census Division limit
          IF ((s .GE. 6) .OR. (s.EQ.4)) rbound= CMnumBldg
               ! lighting, refrigeration, and ventilation
               ! are specified by bldg type instead of by
               ! Census division.

          IF ((r.LT.1) .OR. (r.GT.rbound)) THEN

            IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 'r=',r,' out of bounds, record',COUNT, &
                            ' skipped: '
            GO TO 57
          ENDIF
          IF (s .LT. 1 .OR. s .GT. CMnumMajServ) THEN
            IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 's=',s,' out of bounds, record',COUNT, &
                            ' skipped: '
            GO TO 57
          ENDIF
          IF (f .LT. 1 .OR. f .GT. CMnumMajFl) THEN
            IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 'f=',f,' out of bounds, record',COUNT, &
                            ' skipped: '
            GO TO 57
          ENDIF
!
!   Check for duplicate record: TechEff(r,s,t,v) > 0.0 indicates
!     duplicate record (except for lighting, refrig & ventilation)
!
      IF ((TechEff(r,s,t,v).GT.0.0).AND.(s.LT.6).AND.(s.NE.4)) Then
            IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 'Following record skipped (duplicate):'
            GO TO 57
          ENDIF
!
!    Check for efficiency, capital cost, O&M cost, and subsidy costs < 0.  !investsub
!       If so, skip record.
!
          IF (effic .LE. 0.0) THEN
            IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 'Efficiency=',effic,' not reasonable', &
                            ' record',COUNT,' skipped: '
            GO TO 57
          END IF

          IF (CCTemp .LE. 0.0) THEN
            IF(PRTDBGK.EQ.1) &
             WRITE(RCDBG,*) 'Capital Cost=',CCTemp,' not reasonable', &
                           ' record',COUNT,' skipped: '
            GO TO 57
          ENDIF

          IF (OMTemp .LE. 0.0) THEN
            IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'O&M Cost=',OMTemp,' not reasonable', &
               ' record',COUNT,' skipped: '
            GO TO 57
          ENDIF

          IF (SBTemp .LT. 0.0) THEN
            IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Subsidy Cost=',SBTemp,' not reasonable', &
               ' record',COUNT,' skipped: '
            GO TO 57
          ENDIF

          IF (SB111dTemp .LT. 0.0) THEN
            IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Subsidy 111d Cost=',SB111dTemp,' not reasonable', &
               ' record',COUNT,' skipped: '
            GO TO 57
          ENDIF

!    Service Life, Initial Year, and Last Year are stored only by
!      technology and vintage, not region.  If input data varies by
!      region, only last data point read in is stored.  Check for
!      changes in input data by region, use existing data to correct
!      errors where possible, and write error messages to KDEBUG.
!    Also check for Service Life < 1. and Last Year < First Year
!      If so skip record.
!
          IF (TechLife(t,v).GT.0.0 .AND. TechLife(t,v).NE.SLTemp) THEN
            IF (SLTemp .LT. 1.0) THEN
             IF(PRTDBGK.EQ.1)THEN
              WRITE(RCDBG,*) 'Sevice Life =',SLTemp,' not reasonable' &
              ,' in record',COUNT,' as follows: '
              WRITE(RCDBG,*) ISTR2
              WRITE(RCDBG,*) ' Replaced with Service Life =' &
               ,TechLife(t,v),' from previous record.'
             ENDIF       ! PRTDBGK test
             SLTemp = TechLife(t,v)
            ELSE         ! SLTemp test
             IF(PRTDBGK.EQ.1)THEN
              WRITE(RCDBG,*) 'Sevice Life =',SLTemp,' in record', &
                              COUNT,' as follows: '
              WRITE(RCDBG,*) ISTR2
              WRITE(RCDBG,*)'  Replaces previous Service Life =' &
               ,TechLife(t,v),' from previous record.'
             ENDIF       ! PRTDBGK Test
            ENDIF        ! SLTemp Test
!
          ELSEIF (SLTemp .LT. 1.0) THEN    ! IF TechLife(t,v) .LE. 0.0
                                        ! or TechLife(t,v)  = SLTemp
             IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 'Sevice Life=',SLTemp,' not reasonable', &
                            ' record',COUNT,' skipped: '
            GO TO 57
          END IF
!
          IF (TechAvailability(r,t,v,1) .GT. 0 .AND. &        !regionalize tech availability
              TechAvailability(r,t,v,1).NE.InitYrTemp) THEN   !regionalize tech availability
            IF (InitYrTemp .GT. LastYrTemp) THEN
              IF(PRTDBGK.EQ.1)THEN
                WRITE(RCDBG,*) 'Initial Year =',InitYrTemp, &
                 'not reasonable in record',COUNT,' as follows: '
                WRITE(RCDBG,*) ISTR2
                WRITE(RCDBG,*) ' Replaced with Initial Year =' &
                 ,TechAvailability(r,t,v,1),' from previous record'   !regionalize tech availability
              ENDIF
              InitYrTemp = TechAvailability(r,t,v,1)                  !regionalize tech availability
            ELSE
              IF(PRTDBGK.EQ.1)THEN
                WRITE(RCDBG,*) 'Initial Year =',InitYrTemp, &
                 ' in record',COUNT,' as follows: '
                WRITE(RCDBG,*) ISTR2
                WRITE(RCDBG,*)'  Replaces previous Initial Year =' &
                 ,TechAvailability(r,t,v,1),' from previous record.'  !regionalize tech availability
              ENDIF
            ENDIF
          ENDIF
!
          IF (TechAvailability(r,t,v,2) .GT. 0 .AND. &                !regionalize tech availability
              TechAvailability(r,t,v,2).NE.LastYrTemp) THEN           !regionalize tech availability
            IF (LastYrTemp .LT. InitYrTemp) &
              THEN
              IF(PRTDBGK.EQ.1)THEN
                WRITE(RCDBG,*) 'Last Year =',LastYrTemp, &
                 'not reasonable in record',COUNT,' as follows: '
                WRITE(RCDBG,*) ISTR2
                WRITE(RCDBG,*) ' Replaced with Last Year =' &
                 ,TechAvailability(r,t,v,2),' from previous record.'  !regionalize tech availability
              ENDIF
              LastYrTemp = TechAvailability(r,t,v,2)                  !regionalize tech availability
            ELSE
              IF(PRTDBGK.EQ.1)THEN
                WRITE(RCDBG,*) 'Last Year =',LastYrTemp, &
                 ' in record',COUNT,' as follows: '
                WRITE(RCDBG,*) ISTR2
                WRITE(RCDBG,*)'  Replaces previous Last Year =' &
                  ,TechAvailability(r,t,v,2),' from previous record.' !regionalize tech availability
              ENDIF
            ENDIF
          ENDIF
!
!    Further checking on InitYrTemp and LastYrTemp for values not
!      updated above.
!
          IF (LastYrTemp .LT. InitYrTemp) &
            THEN
            IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 'Initial Year = ',InitYrTemp, &
            ' and Last Year =',LastYrTemp,' not reasonable.  Record' &
              ,COUNT,' skipped: '
            GO TO 57
          ENDIF
!
!    Check for retrofit cost factor < 0.0.  If so, skip record.
!
          IF (RetroTemp .LT. 0.0) THEN
            IF(PRTDBGK.EQ.1) &
              WRITE(RCDBG,*) 'Retrofit Cost Factor =',RetroTemp, &
               ' not reasonable.  Record',COUNT,' skipped: '
            GO TO 57
          END IF
!
!    If market share < 0. or market share > 1., set to 0.0
!    Market shares applicable only to technologies available in base
!       year.  All others must be 0.0.
!
          FLAG = 0
          IF(markshar .LT. 0.0 .OR. markshar .GT. 1.0) FLAG=1

          IF( InitYrTemp .GT. CMBaseYear .AND. markshar .GT. 0.0) THEN
            IF(PRTDBGK.EQ.1) &
             WRITE(RCDBG,*)'Market Share =',markshar,' for vintage ', &
              'with initial year=',InitYrTemp,'.  Market share must ' &
             ,'be 0.0 unless technology is available in',CMBaseYear
            FLAG=1
          ENDIF
          IF (FLAG.EQ.1) THEN
           IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*)'Code sets market share to 0 in following:' &
              ,ISTR2
           markshar = 0.0
          ENDIF

!
!   All indices in bound; assign temporary values to array positions
!
          ! lighting & ventilation use the r subscript for building
          ! type; propagate the values across r: Also for refrig
          IF ((s .LT. 6) .AND. (s.NE.4)) THEN
            TechEff(r,s,t,v) = effic
			If((AB32SW .EQ. 1) .AND. (r .EQ. 9)) SB111dTemp=SB111dTemp*2
            If(EPA111D .EQ.1) then 
			TechCost(r,t,v,1) = CCTemp - SBTemp - SB111dTemp   !regionalize techcost - removing subsidy costs from             !investsub 111(d)
              ELSE
			If(AB32SW .EQ. 0) then 
			SB111dTemp = SB111dTemp*Float(EPA111D)
			TechCost(r,t,v,1) = CCTemp - SBTemp													!                       capital cost for later calculations
            else
			If(r .EQ. 9) then 
			TechCost(r,t,v,1) = CCTemp - SBTemp - SB111dTemp 
			else 
			SB111dTemp = SB111dTemp*Float(EPA111D)
			TechCost(r,t,v,1) = CCTemp - SBTemp
			end if
			end if
			end if
			TechCost(r,t,v,2) = OMTemp                    !regionalize techcost
            TechCost(r,t,v,3) = SBTemp                    !regionalize techcost !investsub
            TechCost(r,t,v,4) = SB111dTemp                !regionalize techcost !investsub 111(d)
            TechAvailability(r,t,v,1)=InitYrTemp          !regionalize tech availability
            TechAvailability(r,t,v,2)=LastYrTemp          !regionalize tech availability
          ELSE
            DO realr= 1, MNUMCR-2
               sb111dtemp=sub111dcst(count)               !investsub 111(d)
               IF ((AB32SW .EQ. 1) .AND. (realr .EQ. 9)) SB111dTemp=SB111dTemp*2		!double EE rebates if CD=9 KKN							!double EE rebates if CD=9 KKN
               TechEff (realr,s,t,v) = effic
               TechCRI (realr,s,t,v) = CRIforLighting       !Add CRI to TechChoice
               If (EPA111D .EQ. 1) then
                  TechCost(realr,t,v,1) = CCTemp - SBTemp - SB111dTemp !regionalize techcost - removing subsidy costs from          !investsub 111(d)
               else                                         !   capital cost for later calculations
                  If (AB32SW .EQ. 0) then
                     SB111dTemp = SB111dTemp*Float(EPA111D)
                     TechCost(realr,t,v,1) = CCTemp - SBTemp
                  else
                     If (realr .EQ. 9) THEN 
                        TechCost(realr,t,v,1) = CCTemp - SBTemp - SB111dTemp
                     else
                        SB111dTemp = SB111dTemp*Float(EPA111D)
                        TechCost(realr,t,v,1) = CCTemp - SBTemp
                     end if
                  end if
               end if
               TechCost(realr,t,v,2) = OMTemp               !regionalize techcost
               TechCost(realr,t,v,3) = SBTemp               !regionalize techcost !investsub
               TechCost(realr,t,v,4) = SB111dTemp           !regionalize techcost !investsub 111(d)
               TechAvailability(realr,t,v,1)=InitYrTemp     !regionalize tech availability
               TechAvailability(realr,t,v,2)=LastYrTemp     !regionalize tech availability
            ENDDO
          ENDIF
          TechLife(t,v)=SLTemp
          RetroCostFract(t,v)=RetroTemp
          EquipName(t,v)=ENTemp

!
!   Identify technology with the given fuel, service, and model:
!
          FuelbyTech (t,f)= 1
          TechbyService (s,t)= 1
          TechbyModel(t,v)=1
!
          ! Except for lighting, refrigeration, & ventilation,
          ! the technology market shares are initially assumed
          ! constant over building type in 2012, but computed
          ! for subsequent years.
          ! For lighting, refrigeration  & ventilation, the
          ! shares are specified by building type and assumed
          ! constant across Census Divisions.
          IF ((s .LT. 6).AND.(s.NE.4)) THEN
           DO b= 1, CMnumBldg
            TechShareofServiceBASE (r,b,s,t,v)= markshar
           ENDDO
          ELSE
           DO realr= 1, MNUMCR-2
            TechShareofServiceBASE (realr,r,s,t,v)= markshar
           ENDDO
          ENDIF
!
!   No read errors.  Loop to next read
!
          GO TO 60
!
!   Report errors and loop to next read.
!
58        CONTINUE
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KTEK read err',IOS,' on record', &
                            count,'; skip record and continue read.'
          GO TO 60
!
59        CONTINUE
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KTEK read err',IOS,' on record', &
                    count,'  Skip the following and continue read:'
57        IF(PRTDBGK.EQ.1) WRITE(RCDBG,*) ISTR2
!
   60   CONTINUE                       ! KTEK read
   30   CONTINUE                       ! end of KTEK reached
!        infile= FILE_MGR ('C','KTECH',.FALSE.)                     !ktechwk1
!
! Deallocate dynamic arrays
      Deallocate (tch,vnt,reg,serv,fuel,yintro,ylast,life,retrofit)  !ktechwk1
      Deallocate (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11)                !ktechwk1
      Deallocate (trndshape,trndstart)                               !ktechwk1
      Deallocate (techname)                                          !ktechwk1
      Deallocate (trndtype)                                          !ktechwk1
      Deallocate (share,capcst,maintcst,subcst,sub111dcst,eff,cri,trndpct) !ktechwk1  !investsub 111(d)

      Deallocate (tch_new,vnt_new,reg_new,serv_new,fuel_new,yintro_new,ylast_new,life_new,retrofit_new)  !ktechwk1
      Deallocate (r1_new,r2_new,r3_new,r4_new,r5_new,r6_new,r7_new,r8_new,r9_new,r10_new,r11_new)                !ktechwk1
      Deallocate (trndshape_new,trndstart_new)                               !ktechwk1
      Deallocate (techname_new)                                          !ktechwk1
      Deallocate (trndtype_new)                                          !ktechwk1
      Deallocate (share_new,capcst_new,maintcst_new,subcst_new,sub111dcst_new,eff_new,cri_new,trndpct_new) !ktechwk1  !investsub 111(d)



!    Index technology subscripts to reduce execution time and
!      write a message to KDEBUG if a service is not represented.
!
        DO 50 s= 1, CMnumMajServ
         count= 0
         flag = 0
         DO 55 t= 1, CMnumTechs
          IF (TechbyService (s,t) .EQ. 1) THEN
           count= count + 1
           flag = 1
           TechsforService (s,count)= t
           ! Determine largest model number for this technology
           vmax = 0
           DO 56 v= 1, CMnumEqVint
            IF ( TechLife (t,v) .GT. 0.0 ) THEN  !regionalize techcost
              IF (v .GT. vmax) vmax = v
            ENDIF
   56      CONTINUE
           CMnumEqV (t)= vmax
          END IF  ! a technology providing this service
   55    CONTINUE
        CMnumTechsforService (s)= count
!
        IF( flag.EQ.0 .AND. PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) '***Error: Service',s, &
           ' not represented in data.  Update data and resubmit job.'
   50   CONTINUE
!
!   Write a message to KDEBUG if a technology is not represented.
!
        DO t=1,CMnumTechs
          Flag=0.0
          DO s=1,CMnumMajServ
            IF(TechbyService(s,t).EQ.1) Flag=1
          END DO  ! s
          IF( flag.EQ.0 .AND. PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Technology',t,' not represented in data.'
        END DO    ! t
!
!   Write a message to KDEBUG if a fuel has no associated technology.
!
        DO f=1,CMnumMajFl
          Flag=0.0
          DO t=1,CMnumTechs
            IF(FuelbyTech(t,f).EQ.1) Flag=1
          END DO  ! t
          IF( flag.EQ.0 .AND. PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Error: Fuel',f,' has no associated ', &
            'technology in data.  Update data and resubmit job.'
        END DO    ! f
!
!   For represented services and technologies, make sure
!     Market Shares add to 0.0 or 1.0 +/- Epsilon within a service
!     and region.
!   Lighting, refrigeration, and ventilation are treated separately,
!   because for other services shares are identical across building
!   types, while for lighting, refrigeration, & ventilation, shares
!   are identical across Census divisions:

      DO r=1,MNUMCR-2
        DO s=1,CMnumMajServ
         IF ((s.GE.6).OR.(s.EQ.4)) GOTO 40  ! light, refrig & vent
          sum=0.0
          DO t=1,CMnumTechs
            DO v=1,CMnumEqV(t)
              IF (TechbyService(s,t).EQ.1 .AND. &
                  TechAvailability(r,t,v,1) .LE. CMBaseYear) Then   !regionalize tech availability
                sum = sum + TechShareofServiceBASE(r,1,s,t,v)
              ENDIF
!
!    For every technology represented in this service,
!      if TechEff(r,s,t,v).EQ.0.0, a vintage or a region is missing.
!
              IF (TechbyService(s,t).EQ.1 .AND. &
                  TechbyModel(t,v).EQ.1   .AND. &
                  TechEff(r,s,t,v).EQ.0.0 .AND. &
                  PRTDBGK.EQ.1) &
                Write(RCDBG,*) 'Error: No data for r =',r,' s =',s, &
                 ' t =',t,' v =',v,'.  Update data and resubmit job.'
            END DO  ! v
          END DO    ! t
!
          IF (sum .EQ. 0.0) Then
            IF(PRTDBGK.EQ.1) Write(RCDBG,540) r,s,CMBaseYear
540         Format(' Error: Market Share sum for r =',I3,' and s =' &
              ,I3,' is 0.0 in',I5,'.  Update data and resubmit job.')
          ELSEIF ( sum .GT. 1.0 + Epsilon(1) .OR. &
                   sum .LT. 1.0 - Epsilon(1) ) Then
            IF(PRTDBGK.EQ.1) Write(RCDBG,541) r,s,sum
541         Format(1x,'Market Share sum for r =',I3,' and s =',I3, &
                   ' is',F8.5,' not 1.0.',/, &
                   '  Code divides each share by sum to normalize.')
            DO t=1,CMnumTechs
              DO v=1,CMnumEqV(t)
                IF (TechbyService(s,t).EQ.1 .AND. sum .GT. 0.0 .AND. &
                    TechAvailability(r,t,v,1) .LE. CMBaseYear) Then   !regionalize tech availability
                  TechShareofServiceBASE(r,1,s,t,v)= &
                    TechShareofServiceBASE(r,1,s,t,v) / sum
                ENDIF
                DO b = 2 , CMnumBldg
                  TechShareofServiceBASE(r,b,s,t,v)= &
                    TechShareofServiceBASE(r,1,s,t,v)
                END DO    ! b
              END DO      ! v
            END DO        ! t
          END IF          ! Test value of 'sum'
 40      CONTINUE         ! for skipping s=6, s=7, and s=4
        END DO            ! s
      END DO              ! r

      ! lighting, refrigeration & ventilation:
      DO b=1,CMnumBldg
        DO s= 4,7   ! ventilation, lighting & refrig
         IF (s .NE. 5) THEN
          sum=0.0
          DO t=1,CMnumTechs
            DO v=1,CMnumEqV(t)
              IF (TechbyService(s,t).EQ.1 .AND. &
                  TechAvailability(b,t,v,1) .LE. CMBaseYear) Then  !regionalize tech availability - set to b for s defined by BT
                sum = sum + TechShareofServiceBASE(1,b,s,t,v)
              ENDIF
!
!    For every technology represented in this service,
!      if TechEff(r=1,s,t,v).EQ.0.0, a vintage is missing.
!
              IF (TechbyService(s,t).EQ.1 .AND. &
                  TechbyModel(t,v).EQ.1   .AND. &
                  TechEff(1,s,t,v).EQ.0.0 .AND. &
                  PRTDBGK.EQ.1) &
                Write(RCDBG,*) 'Error: No data for r = 1, s =',s, &
                 ' t =',t,' v =',v,'.  Update data and resubmit job.'
            END DO  ! v
          END DO    ! t
!
          IF (sum .EQ. 0.0) Then
            IF(PRTDBGK.EQ.1) Write(RCDBG,5540) b,s,CMBaseYear
5540         Format(' Error: Market Share sum for b =',I3,' and s =' &
              ,I3,' is 0.0 in',I5,'.  Update data and resubmit job.')
          ELSEIF ( sum .GT. 1.0 + Epsilon(1) .OR. &
                   sum .LT. 1.0 - Epsilon(1) ) Then
            IF(PRTDBGK.EQ.1) Write(RCDBG,5541) b,s,sum
5541         Format(1x,'Market Share sum for b =',I3,' and s =',I3, &
                   ' is',F8.5,' not 1.0.',/, &
                   '  Code divides each share by sum to normalize.')
            DO t=1,CMnumTechs
              DO v=1,CMnumEqV(t)
                IF (TechbyService(s,t).EQ.1 .AND. sum .GT. 0.0 .AND. &
                    TechAvailability(b,t,v,1) .LE. CMBaseYear) Then  !regionalize tech availability - set to b for s defined by BT
                  TechShareofServiceBASE(1,b,s,t,v)= &
                    TechShareofServiceBASE(1,b,s,t,v) / sum
                ENDIF
                DO r = 2 , MNUMCR-2
                  TechShareofServiceBASE(r,b,s,t,v)= &
                    TechShareofServiceBASE(1,b,s,t,v)
                END DO    ! r
              END DO      ! v
            END DO        ! t
          END IF          ! Test value of 'sum'
         END IF           ! Test for s = 5 (cooking)
        END DO            ! s
      END DO              ! b

!
!   KTEK data set error trapping complete!
!

      IF(PRTDBGK.EQ.1) WRITE(RCDBG,550)
550   FORMAT(/,' KTEK data set error trapping complete!',/)


      ! Read baseline electricity consumption for 111(d) analysis
      !IF(EPA111D.NE.1) GO TO 178

      infile= FILE_MGR ('O','BLDBASE',.FALSE.)
      READ (infile,'(99(/))')  ! skip over header; note that Y is an index from 1 through 61
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'BLDBASE data set error trapping:'
      count = 0  ! record count during read
      NumErr = 0 ! error count during read

!      WRITE (RCDBG,*) 'TEST READ OF NEW BLDBASE with ELECTRICITY BASELINES FOR 111(D) ANALYSIS'
!      WRITE (RCDBG,*) 'Division, Year (1=1990), Baseline bkWh'

      DO r = 1,MNUMCR-2
       DO y = 1,MNUMYR

      READ(INFILE,*,ERR=175,END=176,IOSTAT=IOS) DIV, YEAR, ElTemp, BASELINEBKWHCM(r,y)            !111(d)
        BASELINEBKWHCM(r,y) = (BASELINEBKWHCM(r,y) / 3412.) * 10**3  !convert QELCM trills to bkWh
!        WRITE(RCDBG,*) r, y, BASELINEBKWHCM(r,y)                  !used to verify correct read-in of data.
       ENDDO
      ENDDO

      GO TO 177 ! EOF reached normally; skip over error reporting

 175  CONTINUE ! report read error and resume loop
       NumErr= NumErr + 1
       IF (PRTDBGK .EQ. 1) &
        WRITE (RCDBG,*) 'BLDBASE read error ',IOS,' on record ', count,'; record skipped, read continued.'

      GO TO 177 ! EOF reached normally; skip over error reporting

 176  CONTINUE  ! EOF reached unexpectedly; report
      NumErr= NumErr + 1
      IF (PRTDBGK .EQ. 1) &
       WRITE (RCDBG,*) 'BLDBASE EOF reached prematurely at y=',y, ' s=',s

 177  CONTINUE
      infile= FILE_MGR ('C','BLDBASE',.FALSE.)

      IF (PRTDBGK .EQ. 1) THEN
        WRITE (RCDBG,*) NumErr,' errors detected.'
        WRITE (RCDBG,*)
        WRITE (RCDBG,*) 'BLDBASE data set error trapping complete!'
        WRITE (RCDBG,*)
      END IF    !end of baseline kWh read-in

 178  CONTINUE

!   Adjust last year available for technologies, vintages affected by Dec 2007 energy bill standards if switch is off
      IF(NRG2007 .EQ. 0) THEN     !nrg07
       DO b = 1, CMnumBldg    !regionalize tech availability - set to b for s defined by BT
       ! Lighting  nrg07
       TechAvailability(b,24,1,2)=2050  ! Take out Incand/halogen stnd effective 01-12 if no EISA - incand 100w  nrg07
       TechAvailability(b,24,3,2)=2050  ! Take out Incand/halogen stnd effective 01-12 if no EISA - halogen 120w nrg07
       TechAvailability(b,27,2,2)=2050  ! Take out Metal halide fixture stnd effective 01-09 if no EISA - MH low bay nrg07
       TechAvailability(b,28,2,2)=2050  ! Take out Metal halide fixture stnd effective 01-09 if no EISA - MH high bay nrg07
       ! Refrigeration       
        DO t = 39, 40
         DO v = 1, 2
          TechAvailability(b,t,v,2)=2050  ! Take out Walk-in rfg/fzr stnd effective 01-09 if no EISA - base & 08 typ nrg07
         END DO      ! v   nrg07        
        END DO       ! t  nrg07
       END DO        ! b  nrg07  regionalize tech availability - set to b for s defined by BT
      END IF  !  Remove standards for technologies affected by Dec 2007 energy bill if switch is off

!
! Read 2012 Energy Use Intensities (EUIs):
!   Initialize array in case data set is incomplete
!
      DO r= 1, MNUMCR-2
       DO b= 1, CMnumBldg
        DO f= 1, CMnumMajFl
         DO s= 1, CMnumServ
          ComEUI (r,b,s,f) = 0.0
         END DO ! s
        END DO ! f
       END DO ! b
      END DO ! r
!
      infile= FILE_MGR ('O','KINTENS',.FALSE.)
      READ (infile,'(99(/))')  ! Skip over 100 line header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KINTENS data set error trapping:'
!
      count = 0
      NumErr = 0
      DO 200 r= 1, MNUMCR-2
       DO 200 b= 1, CMnumBldg
        DO 200 f= 1, CMnumMajFl
         count = count + 1
         READ (infile,*,ERR=199,END=201,IOSTAT=IOS) &
              (ComEUI (r,b,s,f),  s= 1, CMnumServ)
         ! If BaseYrPCShrofOffEqEUI .GE. 0, (from KPARM),
         ! interpret KINTENS EUI in Office Equip - PCs
         ! slot as total office equipment EUI, and split
         ! according to BaseYrPCShrofOffEqEUI
         IF (BaseYrPCShrofOffEqEUI .GE. 0.0) THEN
          ComEUI (r,b,9,f)=   & ! (NonPC)
           ComEUI (r,b,8,f) *  (1.0 - BaseYrPCShrofOffEqEUI)

          ComEUI (r,b,8,f)=   & ! (PC)
           ComEUI (r,b,8,f)  *  BaseYrPCShrofOffEqEUI
         END IF
         ! EUI units are kBtu/sqft.  On conversion to SDI,
         ! lighting efficacy units of Lumen/Watt will be used
         ! to obtain lighting SDI units of LumenYrs/sqft;
         ! Therefore, a conversion factor of .03343 wattyr/kBtu
         ! must be applied first, and this is a good time to do so:
         ComEUI (r,b,6,f)= ComEUI (r,b,6,f) * .03343
        GO TO 200
!
 199      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KINTENS read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 200    CONTINUE
        GO TO 202  ! EOF reached in KINTENS when expected
 201    CONTINUE   ! EOF reached in KINTENS prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KINTENS EOF reached prematurely at r =' &
                         ,r,' b =',b,' f =',f
 202    CONTINUE
        infile = FILE_MGR ('C','KINTENS',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,203)
203       FORMAT(/,' KINTENS data set error trapping complete!',/)
         ENDIF


! Initial adjustment of Census Division level equipment market shares
! to reflect building restrictions:
      DO r= 1, MNUMCR-2
       DO s= 1, CMnumMajServ
        DO b= 1, CMnumBldg
      !  Calculate total EUI (across fuels) with which to compute
      !  fuel shares of consumption; also calculate unadjusted average
      !  efficiency of equipment using fuel f, from KTECH, and fuel f
      !  share of service demand from regional market shares supplied
      !  by KTECH:
         ComEUI (r,b,s,CMnumMajFl+1)= 0.0
         DO f= 1, CMnumMajFl
          ComEUI (r,b,s,CMnumMajFl+1)= ComEUI (r,b,s,CMnumMajFl+1) + &
                                       ComEUI (r,b,s,f)
         END DO ! f
         BaseRestrict(b) = 1  ! Initialize holding var     bashp
        END DO  ! b
        CforStotal= 0.0 ! total consump for s, this r
        DO b= 1, CMnumBldg
         CforStotal= CforStotal + &
          ComEUI(r,b,s,CMnumMajFl+1) * &
          CMTotalFlspc (r,b,CBECSyear-(BASEYR-1))
        END DO ! b
        DO TsubS= 1, CMnumTechsforService (s)
         t= TechsforService (s,TsubS)
         DO v= 1, CMnumEqV (t)
          IF ((TechbyModel(t,v) .EQ. 1) .AND. &
              (TechAvailability (r,t,v,1) .LE. CBECSyear)) THEN       !regionalize tech availability
           CforSrestrict= 0.0 ! consump for s, this r, in bldgs this
                              ! equip (t,v) is restricted to
           DO b= 1, CMnumBldg
            IF (s .EQ. 2 .AND. t .LE. CMnumHeatPumpTechs*2) THEN     ! bashp
             BaseRestrict(b) =                                     & ! bashp
               EquipRestriction (t-CMnumHeatPumpTechs,v,b,r)         ! bashp
            ELSE                                                     ! bashp
             BaseRestrict(b)=EquipRestriction (t,v,b,r)              ! bashp
            END IF   !  Test to restore base shares for cooling hps    bashp
            IF (BaseRestrict (b) .EQ. 0)                           & ! bashp
             CforSrestrict= CforSrestrict + &
          ComEUI(r,b,s,CMnumMajFl+1) * &
          CMTotalFlspc(r,b,CBECSyear-(BASEYR-1))
           END DO ! b
           ! Now modify Census Division level shares:
           DO b= 1, CMnumBldg
            IF (BaseRestrict (b) .EQ. 0) THEN                        ! bashp
             IF (CforSrestrict .GT. 0.0) THEN
              TechShareofServiceBASE (r,b,s,t,v)= &
               TechShareofServiceBASE (r,b,s,t,v) * CforStotal &
                                                  / CforSrestrict
             ELSE ! leave as is
             END IF ! CforSrestrict>0 test
            ELSE
             TechShareofServiceBASE (r,b,s,t,v)= 0.0
            END IF ! equip restriction split
            BaseRestrict(b) = 1  ! re-initialize holding var.         bashp
           END DO  ! b
          END IF   ! v association with t
         END DO    ! v
        END DO     ! TsubS
        ! Now renormalize so market shares in b sum to 1
        DO b= 1, CMnumBldg
         denominator= 0.0
         ! find current sum
         DO TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO v= 1, CMnumEqV (t)
           IF ((TechbyModel (t,v) .EQ. 1).AND. &
              (TechAvailability (r,t,v,1) .LE. CBECSyear)) &  !regionalize tech availability
             denominator= &
              denominator + TechShareofServiceBASE (r,b,s,t,v)
          END DO ! v
         END DO  ! TsubS
         ! divide each share by sum, if nonzero
         IF (denominator .GT. 0.0) THEN
          DO TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
           DO v= 1, CMnumEqV (t)
            IF ((TechbyModel (t,v) .EQ. 1).AND. &
               (TechAvailability (r,t,v,1) .LE. CBECSyear)) &  !regionalize tech availability
              TechShareofServiceBASE (r,b,s,t,v)= &
               TechShareofServiceBASE (r,b,s,t,v) / denominator
           END DO ! v
          END DO  ! TsubS
         END IF   ! nonzero denominator
        END DO    ! b
       END DO     ! s
      END DO      ! r


!  Calculate variation of regional base year equipment market shares
!  of service demand across building types so that fuel shares
!  inherent in EUIs are honored, then convert EUIs to SDIs (See
!  July 29, 1994 documentation for a detailed description of
!   this scheme):
      DO r= 1, MNUMCR-2
       DO b= 1, CMnumBldg
        DO s= 1, CMnumServ

      !  Calculate total EUI (across fuels) with which to compute
      !  fuel shares of consumption; also calculate unadjusted average
      !  efficiency of equipment using fuel f, from KTECH, and fuel f
      !  share of service demand from regional market shares supplied
      !  by KTECH:
         ComEUI (r,b,s,CMnumMajFl+1)= 0.0
         DO f= 1, CMnumMajFl
          ComEUI (r,b,s,CMnumMajFl+1)= ComEUI (r,b,s,CMnumMajFl+1) + &
                                       ComEUI (r,b,s,f)
          FuelShareofServiceBASE (r,b,s,f)= 0.0
          AverageEfficiencyBASE (r,b,s,f)= 0.0
          denominator= 0.0
          DO TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
           IF (FuelbyTech (t,f) .EQ. 1) THEN
            DO v=1, CMnumEqV (t)
             IF (TechbyModel(t,v) .EQ. 1) THEN
               FuelShareofServiceBASE (r,b,s,f)= &
                 FuelShareofServiceBASE (r,b,s,f) + &
                 TechShareofServiceBASE (r,b,s,t,v)
               IF (TechEff (r,s,t,v) .GT. 0.0) &
                 denominator= denominator + &
                 TechShareofServiceBASE(r,b,s,t,v) / TechEff(r,s,t,v)
             ENDIF
            END DO ! v
           END IF  ! techs using this f
          END DO   ! techs this s
          IF (denominator .GT. 0.0) &
           AverageEfficiencyBASE (r,b,s,f)= &
            FuelShareofServiceBASE (r,b,s,f) / denominator
         END DO ! f

      !  Calculate building service demand market share scale factor,
      !  Kscale(f), by which to scale
      !  regional service demand market shares
      !  of equipment using fuel f so that building EUI - derived
      !  fuel shares are honored:
         DO f= 1, CMnumMajFl
          denominator= 0.0
          DO IFUEL= 1, CMnumMajFl
           if(ComEUI (r,b,s,CMnumMajFl+1) .gt. 0.0) &      ! error check for divide by zero
            denominator= denominator + &
            ComEUI (r,b,s,IFUEL) / ComEUI (r,b,s,CMnumMajFl+1) * &
            AverageEfficiencyBASE (r,b,s,IFUEL)
          END DO ! IFUEL
          Kscale(f)= 0.0
          IF (FuelShareofServiceBASE (r,b,s,f) .GT. 0.0 .AND. &
              denominator .GT. 0.0) &
            Kscale(f)= &
             ComEUI (r,b,s,f) / ComEUI (r,b,s,CMnumMajFl+1) * &
             AverageEfficiencyBASE (r,b,s,f) / &
             FuelShareofServiceBASE (r,b,s,f) / denominator
         END DO ! f

!      IF(PRTDBGK.eq.1 .and. s.le.7 .and. r.le.9) THEN                 ! test
!        write(rcdbg,*) "  Div",r,"  Bldg",b,"  Svc",s,                ! test
!    *    "  Kscale = ", Kscale                                        ! test
!      END IF   !Check for debug switch and iteration                    test

      !  scale regional service demand market shares
      !  of equipment using fuel f so that building EUI - derived
      !  fuel shares are honored:
         DO f= 1, CMnumMajFl
          DO TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
           IF (FuelbyTech (t,f) .EQ. 1) THEN
            DO v=1, CMnumEqV (t)
             IF (TechbyModel(t,v) .EQ. 1) THEN
               TechShareofServiceBASE (r,b,s,t,v)= &
                 TechShareofServiceBASE (r,b,s,t,v) * Kscale(f)
             ENDIF
            END DO ! v
           END IF  ! techs using this f for this s
          END DO   ! techs this s
         END DO    ! f

      ! Convert Energy Use Intensities (EUIs) to Service Demand
      ! Intensities (SDIs), using adjusted base year market shares,
      ! and efficiencies of equipment supplying each service as
      ! read from file KTECH.
         ! If no equipment defined for this service, it is a
         ! minor service, and the EUI is used rather than the SDI;
         ! hence, the following initialization:
         ServDmdIntenBASE (s,b,r)= ComEUI (r,b,s,CMnumMajFl+1)
         denominator= 0.0


         DO TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)         ! all t supplying s
          DO v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
            IF (TechEff (r,s,t,v) .GT. 0.0) &
              denominator= denominator + &
              TechShareofServiceBASE (r,b,s,t,v) / TechEff (r,s,t,v)
           ENDIF
          END DO ! v
         END DO  ! t
         IF (denominator .GT. 0.0) &
          ServDmdIntenBASE (s,b,r)= ComEUI (r,b,s,CMnumMajFl+1) / &
           denominator

        END DO  ! s
       END DO   ! b
      END DO    ! r

!  Initialize fuel shares to zero and compute major service fuel
!  shares for base year:
       DO 45 r= 1, MNUMCR-2
        DO 45 b= 1, CMnumBldg
         DO 45 s= 1, CMnumServ
          DO 45 f= 1, CMnumMajFl
           FuelShareofServiceBASE (r,b,s,f)= 0.0
            DO 45 TsubS= 1, CMnumTechsforService (s)
            t= TechsforService (s,TsubS)
            IF (FuelbyTech (t,f) .EQ. 1) THEN
             DO v= 1, CMnumEqV (t)
              IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
               FuelShareofServiceBASE (r,b,s,f)= &
                FuelShareofServiceBASE (r,b,s,f) &
              + TechShareofServiceBASE (r,b,s,t,v)
              ENDIF
             END DO
            END IF
   45      CONTINUE

!  Initialize minor service fuel shares and efficiencies;
!  Fuel shares are all-electric.***AEO98 - EXCEPT FOR OTHER***
!  NATURAL GAS AND DISTILLATE MISC ALSO NON-ZERO.**************
!  Efficiencies are indexed to 2003, and assigned the value of 1.
       DO r= 1, MNUMCR-2
        DO b= 1, CMnumBldg
         DO s= CMnumMajServ+1, CMnumServ-1
          AverageEfficiencyBASE (r,b,s,1)= 1.0
          FuelShareofServiceBASE (r,b,s,1)= 1.0
         END DO

        !  The following block allows for more detailed treatment
        !  of misc. end use for gas and distillate (i.e., efficiency
        !  improvements, etc).  Above DO loop used to go to CMnumServ.

         DO f= 1, CMnumMajFl
          AverageEfficiencyBASE (r,b,CMnumServ,f)= 1.0               !Misc
          FuelShareofServiceBASE (r,b,CMnumServ,f)=                & !Misc
         ComEUI(r,b,CMnumServ,f)/ComEUI(r,b,CMnumServ,CMnumMajFl+1)
         END DO     !Misc

        END DO
       END DO
!
! READ ShellHeatFactor (BLDG,IREG,2,CBECSyear-1989), 
!      ShellCoolFactor (BLDG,IREG,2,CBECSyear-1989) 
!   Note the 3rd array position indicates: 1= existing, 2= new
!   Initialize array in case data set is incomplete
!
      DO IREG= 1, MNUMCR-2
       DO IBLDTP= 1, CMnumBldg
        DO I= 1, 2
         ShellHeatFactor (IBLDTP,IREG,I,CBECSyear-1989) = 0.0   !shlfactor
         ShellCoolFactor (IBLDTP,IREG,I,CBECSyear-1989) = 0.0   !shlfactor
        END DO  ! I
       END DO  ! IBLDTP
      END DO  ! IREG
!
      RC1= FILE_MGR ('O','KSHEFF',.FALSE.)
      READ (RC1,'(99(/))')  ! Skip over 100 line header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KSHEFF data set error trapping:'
!
      READ (RC1,*,ERR=226,END=226,IOSTAT=IOS)                            & !endshel
                 ExistImprv,NewImprv                                       !endshel

!!!PH: change the shell improvement parameters ExistImprv, NewImprv, which represent
!!!annual improvements in new and existing shells
      ExistImprv = ExistImprv * 0.6
      NewImprv = NewImprv * 0.4

      count = 0
      NumErr = 0
      DO 225 IREG= 1, MNUMCR-2
       DO 225 IBLDTP= 1, CMnumBldg
        count = count + 1
        READ (RC1,*,ERR=224,END=226,IOSTAT=IOS) &
          ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989), &                 !shlfactor
          ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)                    !shlfactor

!!!PH: change the shell heating and cooling factors
          ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989) = ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989) * 0.4
          ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989) = ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989) * 0.4

        GO TO 225
!
 224      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KSHEFF read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 225    CONTINUE
        GO TO 227  ! EOF reached in KSHEFF when expected
 226    CONTINUE   ! EOF reached in KSHEFF prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KSHEFF EOF reached prematurely at r =' &
                      ,IREG,' b =',IBLDTP
 227    CONTINUE
        RCL1 = FILE_MGR ('C','KSHEFF',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,228)
228       FORMAT(/,' KSHEFF data set error trapping complete!',/)
         ENDIF
		 
!Miscellaneous Equipment Market
!Read Miscellaneous Equipment Market Penetration factors which are based on a 
!value of 1 for 2012
!	Initialize array in case data set is incomplete

			Do I=1, CNumMels				!MEls only
			Do y=1, CMLastYr
			MarketPenetrationMels(I,y)=1.0 
			MelsElQ(I)=1.0
			End Do !y
			End DO !I
			
			Do s=1,10				!data center shares
			Dcf(s)=1.0
			End Do 
			
			Do b=1,11				!Share of miscellaneous electric end uses explicitly accounted for 
			xplicitmiscshr(b)=1.0	!      in base year - currently based on TIAX August 2006 report, 2013 MELS, and 2012 CBECS.
			End Do
			
      RCL2=File_MGR('O','KMELS',.FALSE.)
		Read(RCL2,'(99(/))') !Skip over 99 lines
		IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KMELS data set error trapping:'
		NumErr = 0
	  READ (RCL2,*,ERR=624,END=625,IOSTAT=IOS) &
	 (dcf (s), s=1,10)	!Shares of increased service for Data Center
      GO TO 626
!
 624  CONTINUE    ! Report read error
      NumErr = 1
      IF(PRTDBGK.EQ.1) &
        WRITE(RCDBG,*) 'KMELS dcf read err',IOS, 'on record', &
                           s,'; skip record and continue read.'
      GO TO 626
 625  CONTINUE   ! eof of kmels prematurely; write msg
      NumErr = 1
      IF(PRTDBGK.EQ.1) &
        WRITE(RCDBG,*) 'KMELS DCF reached; NO RECORD READ'
626  CONTINUE
      IF(PRTDBGK.EQ.1) Then
        WRITE(RCDBG,*) NumErr,' errors detected.'
        WRITE(RCDBG,627)
627     FORMAT(/,' kmels dcf data set error trapping complete!',/)
      ENDIF			

		READ (RCL2,'(3(/))')  ! Skip after last read
		IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KMELS data set error trapping:'
		NumErr = 0
		 READ (RCL2,*,ERR=634,END=635,IOSTAT=IOS) &
		 (xplicitmiscshr (b), b=1,11) !Base year to last year in forecast horizon
		 Go to 636
		 
634		 Continue !Report read error and loop to next read
		 NumErr=1
		 IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'xplicitmiscshr read err',IOS,' on record', &
                            b,'; skip record and continue read.'
		Go to 636
635     CONTINUE
		NumErr = 1
		If(PRTDBGK.EQ.1) &
		Write(RCDBG,*) 'KMELS EXPLICITMISCSHR reached; no record read'
636     CONTINUE   ! EOF reached in KMELS prematurely; write msg
        IF(PRTDBGK.EQ.1) Then
        WRITE(RCDBG,*) NumErr,' errors detected.'
        WRITE(RCDBG,637)
637     FORMAT(/,' kmels explicitmiscshr data set error trapping complete!',/)
      ENDIF			
		
		READ (RCL2,'(//)')
		!Count=count+1
		READ (RCL2,*,ERR=670,END=672,IOSTAT=IOS) & FOfficePC
       ! READ (RCL2,'(//)')   ! skip line after last read               ! kk
        READ (RCL2,'(/)')   ! skip line after last read               ! kk
        READ (RCL2,*) LOfficePC		 
        READ (RCL2,'(/)')   ! skip line after last read               ! kk
        READ (RCL2,*) FOfficeNonPC  		 
        READ (RCL2,'(/)')   ! skip line after last read               ! kk
        READ (RCL2,*) LOfficeNonPC		 
        READ (RCL2,'(/)')   ! skip line after last read               ! kk
        READ (RCL2,*) FMisc  		 
        READ (RCL2,'(/)')   ! skip line after last read               ! kk
        READ (RCL2,*) LMisc		 
		 
	GO TO 671
670      CONTINUE
         NumErr = NumErr + 1
         IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'Comm_Kmel read err',IOS,' on record', &
                          count,'; skip record and continue read.'
671     CONTINUE

        GO TO 673
672     NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) Write(RCDBG,*)'Kmel reached prematurely.'

673     CONTINUE           ! EOF reached in Kmel when expected
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,674)
674       FORMAT(/,' kmel office equipment data set error trapping complete!',/)
         ENDIF 

		READ (RCL2,'(//)')  ! Skip after last read
		IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KMELS data set error trapping:'
		NumErr=0
		Do 650 I=1, CNumMels !
		 READ (RCL2,*,ERR=649,END=650,IOSTAT=IOS) &
		 (MelsElQ (I)) !CBECS year to be base year in forecast horizon
		WRITE(RCDBG,*)'MelsElQ(',I,&
		 ') =',MelsElQ(I)
		 Go to 650
		 
649		 Continue !Report read error and loop to next read
		 NumErr=NumErr+1
		 IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KMELSELQ read err',IOS,' on record', &
                            I,'; skip record and continue read.'
650		Continue							
        GO TO 652  ! MELSELQ reached in KMELS when expected
651     CONTINUE   ! MELSELQ reached in KMELS prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KMELS  reached prematurely at I =', &
            I,' not CNumMels'
652    CONTINUE
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,653)
653      FORMAT(/,' KMELS MELSELQ data set error trapping complete!',/)
        ENDIF	
			
		READ (RCL2,'(5(/))')  ! Skip after last read
		IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KMELS data set error trapping:'
		NumErr=0
		Do 660 I=1, CNumMels !
		 READ (RCL2,*,ERR=659,END=661,IOSTAT=IOS) &
		 (MarketPenetrationMels(I,y), y= 24, CMLastYr) !Base year to last year in forecast horizon
		 Write (RCDBG,*) I, &
		(MarketPenetrationMels(I,y) , y=1, CMLastYr)
		 Go to 660
		 
659		 Continue !Report read error and loop to next read
		 NumErr=NumErr+1
		 IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_MarketPenetrationMels read err',IOS,' on record', &
                            I,'; skip record and continue read.'
660		CONTINUE
        GO TO 662  ! marketpenetration reached in KMELS when expected
661    CONTINUE   ! market penetration reached in KMELS prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KMELS market penetration reached prematurely at I =', &
            I,' not CNumMels'
662    CONTINUE
        RCL2= FILE_MGR ('C','KMELS',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,663)
663      FORMAT(/,' KMELS data set error trapping complete!',/)
        ENDIF		 
		 
!  DISTRICT SERVICE DATA:

!  Read steam energy per square foot generated to provide district
!  services:
!   Initialize array in case data set is incomplete
!
      DO r= 1, MNUMCR-2
       DO b= 1, CMnumBldg
        DO s= 1, CMnumDHServ
         DistServSteamEUI (r,b,s) = 0.0
        END DO ! s
       END DO ! b
      END DO ! r
!
      infile= FILE_MGR ('O','KDSSTM',.FALSE.)
      READ (infile,'(99(/))')  ! Skip over header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KDSSTM data set error trapping:'
!
      count = 0
      NumErr = 0
      DO 255 r= 1, MNUMCR-2
       DO 255 b= 1, CMnumBldg
        count = count + 1
        READ (infile,*,ERR=254,END=256,IOSTAT=IOS) &
             (DistServSteamEUI (r,b,s), s= 1, CMnumDHServ)
        GO TO 255
!
 254      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KDSSTM read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 255    CONTINUE
        GO TO 257  ! EOF reached in KDSSTM when expected
 256    CONTINUE   ! EOF reached in KDSSTM prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KDSSTM EOF reached prematurely at r =' &
                         ,r,' b =',b
 257    CONTINUE
        infile = FILE_MGR ('C','KDSSTM',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,258)
258       FORMAT(/,' KDSSTM data set error trapping complete!',/)
         ENDIF

!!!PH: estimate steam EUI
      DO r= 1, MNUMCR-2
       DO b= 1, CMnumBldg
         DistServSteamEUI (r,b,3) = DistServSteamEUI (r,b,3) * 0.8
       END DO ! b
      END DO ! r

!  Read the efficiency of conversion of fuel f to steam energy by
!  boilers used to provide district services.
!   Initialize array in case data set is incomplete
!
      DO f= 1, CMnumMajFl
        DistServBoilerEff (f) = 0.0
      END DO
!
      infile= FILE_MGR ('O','KDSEFF',.FALSE.)
      READ (infile,'(99(/))')  ! Skip over header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KDSEFF data set error trapping:'
!
      NumErr = 0
      READ (infile,*,ERR=244,END=245,IOSTAT=IOS) &
            (DistServBoilerEff (f), f= 1, CMnumMajFl)
      GO TO 246
!
 244  CONTINUE    ! Report read error
      NumErr = 1
      IF(PRTDBGK.EQ.1) &
        WRITE(RCDBG,*) 'Comm_KDSEFF read err',IOS, &
                           '; skip only record.'
      GO TO 246
 245  CONTINUE   ! EOF reached in KDSEFF prematurely; write msg
      NumErr = 1
      IF(PRTDBGK.EQ.1) &
        WRITE(RCDBG,*) 'KDSEFF EOF reached; NO RECORD READ'
 246  CONTINUE
      infile = FILE_MGR ('C','KDSEFF',.FALSE.)

!!!PH: update district service efficiency
      DO f= 1, CMnumMajFl
        DistServBoilerEff (f) = DistServBoilerEff (f) * 0.8
      END DO
!
      IF(PRTDBGK.EQ.1) Then
        WRITE(RCDBG,*) NumErr,' errors detected.'
        WRITE(RCDBG,247)
247     FORMAT(/,' KDSEFF data set error trapping complete!',/)
      ENDIF
!
!  Read the proportions of district service steam energy generated
!  by each fuel type.  Shares in each row must add to 1.0
!  Shares read in by Census division                        ds08
!   Zero out array in case file is incomplete
!
        DO r= 1, MNUMCR-2                               !ds08
          DO f= 1, CMnumMajFl
            DistServFuelShr(r,f)=0.0                    !ds08
          END DO  ! r  ds08
        END DO  ! f
!
      infile= FILE_MGR ('O','KDSFS',.FALSE.)
      READ (infile,'(99(/))')  ! Skip over header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KDSFS data set error trapping:'
      NumErr = 0
      DO 235 r= 1, MNUMCR-2                             !ds08
        sum = 0.0
        READ (infile,*,ERR=234,END=236,IOSTAT=IOS) &
             (DistServFuelShr (r,f), f= 1, CMnumMajFl)  !ds08
          DO f= 1, CMnumMajFl
           IF(DistServFuelShr(r,f) .LT. 0.0 .OR. &
              DistServFuelShr(r,f) .GT. 1.0) Then
             NumErr = NumErr + 1
             IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'DistServFuelShr(',r,f, &
             ') =',DistServFuelShr(r,f),' not reasonable.' &
            ,'  Code sets to 0.0'
             DistServFuelShr(r,f) = 0.0                 !ds08 - also f
           END IF
           sum=sum+DistServFuelShr(r,f)
          END DO
!
!   Check for sum = 1.0 +/- Epsilon
!
          IF (sum .EQ. 0.0) Then
            NumErr = NumErr + 1
            IF(PRTDBGK.EQ.1) Write(RCDBG,*)'***Error:  ', &
            'Fuel Share sum for r =',r,' is 0.0 not 1.0.', &
            '  Update data set and resubmit job.'
          ELSEIF (sum .GT. 1.0 + Epsilon(2) .OR. &
                  sum .LT. 1.0 - Epsilon(2) ) Then
            NumErr = NumErr + 1
            IF(PRTDBGK.EQ.1) Write(RCDBG,*) &
               'Fuel Share sum for r =',r,' is',sum,' not 1.0.', &
                   '  Code divides each share by sum to normalize.'
            DO f=1,CMnumMajFl
              DistServFuelShr(r,f) = DistServFuelShr(r,f) / sum
            END DO        ! f
          END IF          ! Test value of 'sum'
!
          GO TO 235
!
 234      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KDSFS read err',IOS,' on record', &
                            b,'; skip record and continue read.'
 235    CONTINUE
        GO TO 237  ! EOF reached in KDSFS when expected
 236    CONTINUE   ! EOF reached in KDSFS prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KDSFS EOF reached prematurely at b =',b
  237   CONTINUE
        infile= FILE_MGR ('C','KDSFS',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,238)
238       FORMAT(/,' KDSFS data set error trapping complete!',/)
        ENDIF

! Read Solar Thermal forecast, by which to decrement service
! demands calculated from floorspace, SDI, mkt pen, & shell eff:
!   Initialize array in case data set is incomplete
!
      SunlightEfficacy = 0.0
      DO IREG= 1, MNUMCR-2
       DO y= 1, CMLastYr
        DO ISERV=1, CMnumSolarServ
         SolarRenewableContrib(IREG,ISERV,y) = 0.0
        END DO ! ISERV
       END DO ! y
      END DO ! IREG
!
      RC1= FILE_MGR ('O','KRENEW',.FALSE.)
      READ (RC1,'(99(/))')  ! Skip over 100 line header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KRENEW data set error trapping:'
!
      count = 1
      NumErr = 0
      READ (RC1,*,ERR=2390,END=241,IOSTAT=IOS) SunlightEfficacy
      GO TO 2400
!
2390  CONTINUE ! Report read error and go to next read
      NumErr = NumErr + 1
      IF(PRTDBGK.EQ.1) &
         WRITE(RCDBG,*) 'Comm_KRENEW read err',IOS,' on record', &
                        count,'; skip record and continue read.'
2400  CONTINUE
      DO 240 IREG= 1, MNUMCR-2
       DO 240 y= 1, MNUMYR
        count = count + 1
        READ (RC1,*,ERR=239,END=241,IOSTAT=IOS) &
             (SolarRenewableContrib(IREG,ISERV,y), &
                         ISERV=1, CMnumSolarServ)
        SolarRenewableContrib (IREG,6,y)=   & ! lighting
        SolarRenewableContrib (IREG,6,y) * SunlightEfficacy
        GO TO 240
!
 239    CONTINUE ! Report read error and loop to next read
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'Comm_KRENEW read err',IOS,' on record', &
                         count,'; skip record and continue read.'
 240    CONTINUE
        GO TO 242  ! EOF reached in KRENEW when expected
 241    CONTINUE   ! EOF reached in KRENEW prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KRENEW EOF reached prematurely at r =' &
                     ,IREG,' y =',y
 242    CONTINUE
        RCL1 = FILE_MGR ('C','KRENEW',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,243)
243       FORMAT(/,' KRENEW data set error trapping complete!',/)
         ENDIF

! Read short-run price elasticities of demand used to compute
! service demand elasticities resulting from short-term changes
! in fuel prices.  Also read elasticities used to compute
! hurdle rate elasticities resulting from rising fuel prices.
!
!   Initialize arrays in case data set is incomplete
!
      DO IREG = 1, MNUMCR-2
       DO ISERV= 1, CMnumSDElas
        DO IFUEL= 1, CMnumMajFl
         ShortRunPriceElasofDmd (IREG,ISERV,IFUEL) = 0.0
         HurdleElas (IREG,ISERV,IFUEL) = 0.0
        ENDDO ! IFUEL
       ENDDO ! ISERV
      ENDDO ! IREG

      RC1= FILE_MGR ('O','KSDELA',.FALSE.)
      READ (RC1,'(99(/))')  ! Skip over header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KSDELA data set error trapping:'
!
      count = 0
      NumErr = 0
      DO 250 IREG= 1, MNUMCR-2
       DO 250 ISERV= 1, CMnumSDElas
        count = count + 1
        READ (RC1,*,ERR=249,END=251,IOSTAT=IOS) &
         (ShortRunPriceElasofDmd (IREG,ISERV,IFUEL), &
                                  IFUEL= 1, CMnumMajFl)
          DO IFUEL=1, CMnumMajFl
           IF(ShortRunPriceElasofDmd(IREG,ISERV,IFUEL) .GT. 0.0) THEN
             NumErr = NumErr + 1
             IF(PRTDBGK.EQ.1)WRITE(RCDBG,*) &
             'ShortRunPriceElasofDmd(',IREG,ISERV,IFUEL,') =', &
              ShortRunPriceElasofDmd(IREG,ISERV,IFUEL), &
             ' not reasonable.  Code sets to 0.0'
             ShortRunPriceElasofDmd(IREG,ISERV,IFUEL) = 0.0
           END IF
          END DO    ! IFUEL
        GO TO 250
!
 249      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KSDELA read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 250    CONTINUE
        GO TO 252  ! EOF reached in KSDELA when expected
 251    CONTINUE   ! EOF reached in KSDELA prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KSDELA EOF reached prematurely at r =' &
                         ,IREG,' s =',ISERV
 252    CONTINUE
        RC1 = FILE_MGR ('C','KSDELA',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,253)
253       FORMAT(/,' KSDELA data set error trapping complete!',/)
         ENDIF

      RC1= FILE_MGR ('O','KHURELA',.FALSE.)
      READ (RC1,'(99(/))')  ! Skip over header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KHURELA data set error trapping:'
!
      count = 0
      NumErr = 0
      DO 2250 IREG= 1, MNUMCR-2
       DO 2250 ISERV= 1, CMnumSDElas
        count = count + 1
        READ (RC1,*,ERR=2249,END=2251,IOSTAT=IOS) &
         (HurdleElas (IREG,ISERV,IFUEL), &
                                  IFUEL= 1, CMnumMajFl)
          DO IFUEL=1, CMnumMajFl
           IF(HurdleElas(IREG,ISERV,IFUEL) .GT. 0.0) THEN
             NumErr = NumErr + 1
             IF(PRTDBGK.EQ.1)WRITE(RCDBG,*) &
             'HurdleElas(',IREG,ISERV,IFUEL,') =', &
              HurdleElas(IREG,ISERV,IFUEL), &
             ' not reasonable.  Code sets to 0.0'
             HurdleElas(IREG,ISERV,IFUEL) = 0.0
           END IF
          END DO    ! IFUEL
        GO TO 2250
!
 2249      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KHURELA read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 2250    CONTINUE
        GO TO 2252  ! EOF reached in KHURELA when expected
 2251    CONTINUE   ! EOF reached in KHURELA prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KHURELA EOF reached prematurely at r =' &
                         ,IREG,' s =',ISERV
 2252    CONTINUE
        RC1 = FILE_MGR ('C','KHURELA',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,2253)
 2253       FORMAT(/,' KHURELA data set error trapping complete!',/)
         ENDIF

      ! Initialize in the event of a read error:
      DO y= 1, CMLastYr                                !popDD
       DO r= 1, MNUMCR-2
        DO s= 1, 2
         DegreeDays (s,r,y)= 0.0
        END DO ! s
       END DO  ! r
      END DO   ! y

      ! Read heating and cooling degree day data:
      infile= FILE_MGR ('O','KDEGDAY',.FALSE.)
      READ (infile,'(99(/))')  ! skip over header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KDEGDAY data set error trapping:'
      count= 0  ! record count during read
      NumErr= 0 ! error count during read

      DO y= 1, CMLastYr                                !popDD
       DO s= 1, 2   ! s=1 -> heating, 2->cooling
        count= count + 1
        READ (infile,*,ERR=599,END=601,IOSTAT=IOS) &
         TMPYR(y), (DegreeDays (s,r,y), r= 1, MNUMCR-2)
        GO TO 600 ! jump over error reporting

 599    CONTINUE ! report read error and resume loop
        NumErr= NumErr + 1
        IF (PRTDBGK .EQ. 1) &
         WRITE (RCDBG,*) 'KDEGDAY read error ',IOS,' on record ', &
                          count,'; record skipped, read continued.'
 600    DegreeDays(s,11,y) = 0.0
        DO R=1,MNUMCR-2
           DegreeDays(s,11,y) = DegreeDays(s,11,y) + DegreeDays(s,r,y) * MC_NP(r,y)/MC_NP(11,y)
        ENDDO
        CONTINUE
       END DO ! s
      END DO  ! y
      GO TO 602 ! EOF reached normally; skip over error reporting

 601  CONTINUE  ! EOF reached unexpectedly; report
      NumErr= NumErr + 1
      IF (PRTDBGK .EQ. 1) &
       WRITE (RCDBG,*) 'KDEGDAY EOF reached prematurely at y=',y, &
                        ' s=',s

 602  CONTINUE
      infile= FILE_MGR ('C','KDEGDAY',.FALSE.)

      IF (PRTDBGK .EQ. 1) THEN
        WRITE (RCDBG,*) NumErr,' errors detected.'
        WRITE (RCDBG,*)
        WRITE (RCDBG,*) ' KDEGDAY data set error trapping complete!'
        WRITE (RCDBG,*)
      END IF

!  Proportion of floorspace serviced, for variable SDI calc:
      ! Initialize in the event of a read error:
      DO v= 1, CMnumVarSDICat ! for each category (1-old,2-new)
       DO s= 1, CMnumVarSDI   ! 1->htg,2-cool,...,6-lighting)
        DO b= 1, CMnumBldg
         ServicedFlrspcProp (b,s,v)= 0.0
        END DO ! b
       END DO  ! s
      END DO   ! v

      ! Read serviced floorspace proportions:
      infile= FILE_MGR ('O','KVARSDI',.FALSE.)
      READ (infile,'(99(/))')  ! skip over header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KVARSDI data set error trapping:'
      count= 0  ! record count during read
      NumErr= 0 ! error count during read

      DO s= 1, CMnumVarSDI
       DO v= 1, CMnumVarSDICat   ! v=1->old(pre1990), 2->new
        count= count + 1
        READ (infile,*,ERR=699,END=601,IOSTAT=IOS) &
         (ServicedFlrspcProp (b,s,v), b= 1,CMnumBldg)
        GO TO 700 ! jump over error reporting

 699    CONTINUE ! report read error and resume loop
        NumErr= NumErr + 1
        IF (PRTDBGK .EQ. 1) &
         WRITE (RCDBG,*) 'KVARSDI read error ',IOS,' on record ', &
                          count,'; record skipped, read continued.'

 700    CONTINUE
       END DO ! v
      END DO  ! s
      GO TO 702 ! EOF reached normally; skip over error reporting

 701  CONTINUE  ! EOF reached unexpectedly; report
      NumErr= NumErr + 1
      IF (PRTDBGK .EQ. 1) &
       WRITE (RCDBG,*) 'KVARSDI EOF reached prematurely at s=',s, &
                        ' v=',v

 702  CONTINUE
      infile= FILE_MGR ('C','KVARSDI',.FALSE.)

      IF (PRTDBGK .EQ. 1) THEN
        WRITE (RCDBG,*) NumErr,' errors detected.'
        WRITE (RCDBG,*)
        WRITE (RCDBG,*) ' KVARSDI data set error trapping complete!'
        WRITE (RCDBG,*)
      END IF


      END IF
! End of block to read input data


!  During the 1st iteration of each year, copy certain final values
!  from the previous year into holding variables that will retain
!  these values during subsequent iterations:

      IF (CURITR .EQ. 1 .OR. LOOPOP .EQ. 2) THEN

      DO 75 r= 1, MNUMCR
       DO 75 b= 1, CMnumBldg
        DO 75 s= 1, CMnumServ

         DO 72 f= 1, CMnumMajFl
          IF (CURIYR .EQ. CMFirstYr) THEN  ! prev yr is base yr
           PrevYrFuelShareofService (r,b,s,f)= &
                FuelShareofServiceBASE (r,b,s,f)
           PrevYrAverageEfficiency (r,b,s,f)= &
                AverageEfficiencyBASE (r,b,s,f)
           IF (s .EQ. CMnumServ)         & ! Initialize for Misc. to calc SD
            FuelShareofService (r,b,s,f)=                       & ! Misc
                FuelShareofServiceBASE (r,b,s,f)                  ! Misc
            AverageEfficiency (r,b,s,f) =                       & ! Misc
                AverageEfficiencyBASE (r,b,s,f)                   ! Misc

!      IF(PRTDBGK.eq.1) THEN                                          ! test
!       IF(s.eq.2 .and. r.le.9)                                       ! test
!    *   write(rcdbg,6002) r,b,s,f,                                   ! test
!    *    FuelShareofServiceBASE (r,b,s,f),                           ! test
!    *   AverageEfficiencyBASE (r,b,s,f)                              ! test
!6002  FORMAT (1X,I1,1X,I2,2(1X,I1),2(2X,F9.4))                       ! test
!      END IF   !Check for debug switch and iteration                   test

          ELSE  ! prev yr was most recently calculated
           PrevYrFuelShareofService (r,b,s,f)= &
                FuelShareofService (r,b,s,f)
           PrevYrAverageEfficiency (r,b,s,f)= &
                AverageEfficiency (r,b,s,f)
           IF (CURIYR .EQ. CMFirstYr+1 .AND. &
             AverageEfficiency (r,b,s,f) .GT. 0.0) THEN               ! Rbndfix
            AverageEfficiencyBASE (r,b,s,f) =                       & ! Rbndfix
                AverageEfficiency (r,b,s,f)                           ! Rbndfix
           END IF ! reset base year efficiencies for rebound calc       Rbndfix
          END IF  ! check for first year of forecast
   72    CONTINUE

         DO 75 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO 75 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
            IF (CURIYR .EQ. CMFirstYr) THEN  ! prev yr is base yr
             PrevYrTechShareofService (r,b,s,t,v)= &
                 TechShareofServiceBASE (r,b,s,t,v)
!      IF(PRTDBGK.eq.1 .and. r.le.9) THEN                              ! test
!       IF(s.eq.2.and.TechAvailability(r,t,v,1).le.CMBaseYear)         ! test
!    *   write(rcdbg,*) "  Tech",t,"  Vint",v,                         ! test
!    *    "Base year Tech Share of Service = ",                        ! test
!    *    TechShareofServiceBASE (r,b,s,t,v)                           ! test
!      END IF   !Check for debug switch and iteration                    test

            ELSE  ! prev yr was most recently calculated
             PrevYrTechShareofService (r,b,s,t,v)= &
                 TechShareofService (r,b,s,t,v)
            END IF
           ENDIF
   75  CONTINUE

      END IF ! 1st iteration handling of prev yr values

! Initialize Solar Thermal Contribution for Water heating end-use
!  service, each iteration of each year. Solar water heating now 
!  competes with other end-use technologies in Tech Choice Submodule.

      DO r = 1, MNUMCR - 2
         SolarRenewableContrib(r,3,CURIYR) = 0.0
        END DO  ! r Regions to initialize Solar Thermal water heating

!                                                                        endshel
! Endogenous Shell Section AEO 97                                        endshel
!                                                                        endshel
      IF (PRTDBGK.eq.1 .and. CURITR.eq.MAXITR) THEN                     !endshel
        WRITE (RCDBG,*)                                                 !endshel
        WRITE (RCDBG,*) ' Building Shell Efficiency Parameters'         !endshel
       write(rcdbg,*) "YR BT R ","SURVIV AVG ","  NEW B-YR ",         & !endshel
        " B-YR STAVG ","  ADJ-NEW  "," WGTD NEW"                        !endshel
      END IF                                                            !endshel
          YRCHNG = 20                                                   !arra09
                                                                        !endshel
        DO 13 IBLDTP= 1, CMnumBldg                                      !endshel
         DO 13 IREG= 1, MNUMCR-2                                        !endshel
                                                                        !endshel
                                                                        !endshel
! drive existing and new indices from assumed improvement params         endshel
! set target equal to ExistImprv, unless new shells are not that eff     endshel
                                                                        !endshel
      ExistShlMaxHt=MAX(ExistImprv,ShellHeatFactor(IBLDTP,IREG,2,curiyr))     !shlfactor
      ExistShlMaxCl=MAX(ExistImprv,ShellCoolFactor(IBLDTP,IREG,2,curiyr))     !shlfactor
                                                                        !endshel
      ExistShBaseStockHt= (ExistShlMaxHt**                            & !endshel-shlfactor
        (1./FLOAT(CMLastYr+(BASEYR-1)-CBECSyear)))                    & !endshel
        **FLOAT(CURIYR+(BASEYR-1)-CBECSyear)                            !endshel
      ExistShBaseStockCl= (ExistShlMaxCl**                            & !endshel-shlfactor
        (1./FLOAT(CMLastYr+(BASEYR-1)-CBECSyear)))                    & !endshel
        **FLOAT(CURIYR+(BASEYR-1)-CBECSyear)                            !endshel
                                                                        !endshel
                                                                        !endshel
! set growth target for new shell improvements by end of forecast, NewImprv endshel
      NewShAdj= (NewImprv**                                           & !endshel
        (1./FLOAT(CMLastYr+(BASEYR-1)-CBECSyear)))                    & !endshel
        **FLOAT(CURIYR+(BASEYR-1)-CBECSyear)                            !endshel
                                                                        !endshel
      NewShAdjHt= NewShAdj                                              !shlcodes 
      NewShAdjCl= NewShAdj                                              !shlcodes 
                                                                        !endshel
       NShlImprv:  select case (CURIYR)   ! phase in stricter bldg codes for new bldgs and continued shell improvement - shlcodes
           case ( :20) NShlImprv  ! use growth target for new shell improvements prior to 2010 !shlcodes
             NewShAdj= (NewImprv**                                           & !endshel
               (1./FLOAT(CMLastYr+(BASEYR-1)-CBECSyear)))                    & !endshel
               **FLOAT(CURIYR+(BASEYR-1)-CBECSyear)                            !endshel
             NewShAdjHt= NewShAdj                                              !shlcodes 
             NewShAdjCl= NewShAdj                                              !shlcodes
           case (21:23) NShlImprv  ! phase-in of ASHRAE 90.1-2007 starting in 2010 - 82.4% adoption by 2012 !shlcodes
             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             NewShAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.)))  &  !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **FLOAT(CURIYR+(BASEYR-1)-2009)                          !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             NewShAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.)))  &  !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **FLOAT(CURIYR+(BASEYR-1)-2009)                          !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
           case (24:26) NShlImprv  ! phase-in remainder of ASHRAE 90.1-2007 improvement starting in 2013 !shlcodes
             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             NewShAdjHt= TempAdjHt*((0.871/TempAdjHt)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2012)                          !shlcodes
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             NewShAdjCl= TempAdjCl*((0.871/TempAdjCl)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2012)                          !shlcodes
           case (27:35) NShlImprv  ! phase-in of ASHRAE 90.1-2013 starting in 2016 would be complete by 2024 shlcodes
             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             TempAdjHt= TempAdjHt*((0.871/TempAdjHt)**(1./(2016.-2012.)))  &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                          !shlcodes - before next code kicks in
             NewShAdjHt= TempAdjHt*((0.800/TempAdjHt)**(1./(2024.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2015)                          !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             TempAdjCl= TempAdjCl*((0.871/TempAdjCl)**(1./(2016.-2012.)))  &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                          !shlcodes - before next code kicks in
             NewShAdjCl= TempAdjCl*((0.800/TempAdjCl)**(1./(2024.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2015)                          !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
           case (36:) NShlImprv    ! shlcodes stricter bldg code for new bldgs in force by 2024, resume original rate of improvement
             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)         !shlcodes - improvement up to code phase-in
             TempAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.))) &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                         !shlcodes - before next tier kicks in
             TempAdjHt= TempAdjHt*((0.871/TempAdjHt)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                         !shlcodes - before next code kicks in
             TempAdjHt= TempAdjHt*((0.800/TempAdjHt)**(1./(2024.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016, complete adoption by 2024
                      **(2024.-2015.)                                         !shlcodes
             NewShAdjHt= TempAdjHt*(NewImprv**(1./(2040.-2003.)))         &   !shlcodes - resume original rate of improvement
                      **FLOAT(CURIYR+(BASEYR-1)-2024)                         !shlcodes
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.))) &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                         !shlcodes - before next tier kicks in
             TempAdjCl= TempAdjCl*((0.871/TempAdjCl)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                         !shlcodes - before next code kicks in
             TempAdjCl= TempAdjCl*((0.800/TempAdjCl)**(1./(2024.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016, complete adoption by 2024
                      **(2024.-2015.)                                         !shlcodes
             NewShAdjCl= TempAdjCl*(NewImprv**(1./(2040.-2003.)))         &   !shlcodes - resume original rate of improvement
                      **FLOAT(CURIYR+(BASEYR-1)-2024)                         !shlcodes
            end select NShlImprv                                        !phase in stricter bldg codes for new bldgs shlcodes
!      END IF    ! check for Stimulus switch, start year                  arra09

			!California SB32 moving up ASHRAE Compliance to 2020
			NShlImprvCA:  select case (CURIYR)
           case ( :20) NShlImprvCA  ! use growth target for new shell improvements prior to 2010 !shlcodes
             NewShAdjCA= (NewImprv**                                           & !endshel
               (1./FLOAT(CMLastYr+(BASEYR-1)-CBECSyear)))                    & !endshel
               **FLOAT(CURIYR+(BASEYR-1)-CBECSyear)                            !endshel
             NewShAdjHtCA= NewShAdj                                              !shlcodes 
             NewShAdjClCA= NewShAdj                                              !shlcodes
           case (21:23) NShlImprvCA  ! phase-in of ASHRAE 90.1-2007 starting in 2010 - 82.4% adoption by 2012 !shlcodes
             TempAdjHtCA= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             NewShAdjHtCA= TempAdjHtCA*((0.893/TempAdjHtCA)**(1./(2012.-2009.)))  &  !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **FLOAT(CURIYR+(BASEYR-1)-2009)                          !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
             TempAdjClCA= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             NewShAdjClCA= TempAdjClCA*((0.893/TempAdjClCA)**(1./(2012.-2009.)))  &  !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **FLOAT(CURIYR+(BASEYR-1)-2009)                          !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
           case (24:26) NShlImprvCA  ! phase-in remainder of ASHRAE 90.1-2007 improvement starting in 2013 !shlcodes
             TempAdjHtCA= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjHtCA= TempAdjHtCA*((0.893/TempAdjHtCA)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             NewShAdjHtCA= TempAdjHtCA*((0.871/TempAdjHtCA)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2012)                          !shlcodes
             TempAdjClCA= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjClCA= TempAdjClCA*((0.893/TempAdjClCA)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             NewShAdjClCA= TempAdjClCA*((0.871/TempAdjClCA)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2012)                          !shlcodes
           case (27:31) NShlImprvCA  ! phase-in of ASHRAE 90.1-2013 starting in 2016 would be complete by 2024 shlcodes
             TempAdjHtCA= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjHtCA= TempAdjHtCA*((0.893/TempAdjHtCA)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             TempAdjHtCA= TempAdjHtCA*((0.871/TempAdjHtCA)**(1./(2016.-2012.)))  &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                          !shlcodes - before next code kicks in
             NewShAdjHtCA= TempAdjHtCA*((0.800/TempAdjHtCA)**(1./(2020.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2015)                          !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
             TempAdjClCA= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjClCA= TempAdjClCA*((0.893/TempAdjClCA)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             TempAdjClCA= TempAdjClCA*((0.871/TempAdjClCA)**(1./(2016.-2012.)))  &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                          !shlcodes - before next code kicks in
             NewShAdjClCA= TempAdjClCA*((0.800/TempAdjClCA)**(1./(2020.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2015)                          !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
           case (32:) NShlImprvCA    ! shlcodes stricter bldg code for new bldgs in force by 2024, resume original rate of improvement
             TempAdjHtCA= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)         !shlcodes - improvement up to code phase-in
             TempAdjHtCA= TempAdjHtCA*((0.893/TempAdjHtCA)**(1./(2012.-2009.))) &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                         !shlcodes - before next tier kicks in
             TempAdjHtCA= TempAdjHtCA*((0.871/TempAdjHtCA)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                         !shlcodes - before next code kicks in
             TempAdjHtCA= TempAdjHtCA*((0.800/TempAdjHtCA)**(1./(2020.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016, complete adoption by 2024
                      **(2020.-2015.)                                         !shlcodes
             NewShAdjHtCA= TempAdjHtCA*(NewImprv**(1./(2040.-2003.)))         &   !shlcodes - resume original rate of improvement
                      **FLOAT(CURIYR+(BASEYR-1)-2020)                         !shlcodes
             TempAdjClCA= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjClCA= TempAdjClCA*((0.893/TempAdjClCA)**(1./(2012.-2009.))) &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                         !shlcodes - before next tier kicks in
             TempAdjClCA= TempAdjClCA*((0.871/TempAdjClCA)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                         !shlcodes - before next code kicks in
             TempAdjClCA= TempAdjClCA*((0.800/TempAdjClCA)**(1./(2020.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016, complete adoption by 2024
                      **(2020.-2015.)                                         !shlcodes
             NewShAdjClca= TempAdjClca*(NewImprv**(1./(2040.-2003.)))         &   !shlcodes - resume original rate of improvement
                      **FLOAT(CURIYR+(BASEYR-1)-2020)                         !shlcodes
            end select NShlImprvCA
!      END IF    ! check for Stimulus switch, start year 


! Accumulate new construction from base to year before current fcast yr endshel
!   this is to compute stock averages for all but this year's new const endshel
      TotNewFS=0.                                                       !endshel
      NewShlAvgHt=0.                                                    !endshel-shlfactor
      NewShlAvgCl=0.                                                    !endshel-shlfactor
	  NewShlAvgHtCA=0.                                                    !endshel-shlfactor
      NewShlAvgClCA=0.
                                                                        !endshel
      DO 11 IYR=CBECSyear-(BASEYR-1),CURIYR-1                           !endshel
                                                                        !endshel
      TotNewFS=TotNewFS + CMNewFloorSpace(IREG,IBLDTP,IYR)              !endshel
                                                                        !endshel

			!California SB32 moving up ASHRAE Compliance to 2020, IF THEN VERSION		
	If(IREG.EQ.9) then		
        techNShlAvgCA:  select case (IYR)                                  !shlcodes
         case ( :20) techNShlAvgCA    ! use growth target for new shell improvements prior to 2010 !shlcodes
			NewShlAvgHtCA=NewShlAvgHtCA + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !endshel-shlfactor
            ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*              & !endshel-shlfactor
            (NewImprv**(1./FLOAT(CMLastYr+(BASEYR-1)-CBECSyear)))       & !endshel
            **FLOAT(IYR+(BASEYR-1)-CBECSyear)                             !endshel
           NewShlAvgClCA=NewShlAvgClCA + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !endshel-shlfactor
            ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*              & !endshel-shlfactor
            (NewImprv**(1./FLOAT(CMLastYr+(BASEYR-1)-CBECSyear)))       & !endshel
            **FLOAT(IYR+(BASEYR-1)-CBECSyear) 
         case (21:23) techNShlAvgCA   ! phase-in of ASHRAE 90.1-2007 starting in 2010 - 82.4% adoption by 2012 !shlcodes
		             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
            NewShlAvgHtCA=NewShlAvgHtCA + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                      ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*      & !arra09-shlfactor
             (TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.)))             & !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **FLOAT(CURIYR+(BASEYR-1)-2009))                        !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
            TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
            NewShlAvgClCA=NewShlAvgClCA + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                       ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
             (TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.)))             & !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **FLOAT(CURIYR+(BASEYR-1)-2009))                        !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
		case (24:26) techNShlAvgCA  ! phase-in remainder of ASHRAE 90.1-2007 improvement starting in 2013 !shlcodes
		             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             NewShlAvgHtCA=NewShlAvgHtCA + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                        ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
              (TempAdjHt*((0.871/TempAdjHt)**(1./(2016.-2012.)))            &  !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2012))                         !shlcodes
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             NewShlAvgClCA=NewShlAvgClCA + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                        ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
              (TempAdjCl*((0.871/TempAdjCl)**(1./(2016.-2012.)))             & !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2012))                         !shlcodes
        case (27:31) techNShlAvgCA  ! phase-in of ASHRAE 90.1-2013 starting in 2016 would be complete by 2020 shlcodes
             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             TempAdjHt= TempAdjHt*((0.871/TempAdjHt)**(1./(2016.-2012.)))  &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                          !shlcodes - before next code kicks in
             NewShlAvgHtCA=NewShlAvgHtCA + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                        ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
              (TempAdjHt*((0.800/TempAdjHt)**(1./(2020.-2015.)))            &  !shlcodes - phase-in 90.1-2013 starting in 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2015))                         !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             TempAdjCl= TempAdjCl*((0.871/TempAdjCl)**(1./(2016.-2012.)))  &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                          !shlcodes - before next code kicks in
             NewShlAvgClCA=NewShlAvgClCA + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                        ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
              (TempAdjCl*((0.800/TempAdjCl)**(1./(2020.-2015.)))             & !shlcodes - phase-in 90.1-2013 starting in 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2015))    
           case (32:) techNShlAvgCA 		   !arra09 - back to "natural" improvement
		                TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)         !shlcodes - improvement up to code phase-in
             TempAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.))) &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                         !shlcodes - before next tier kicks in
             TempAdjHt= TempAdjHt*((0.871/TempAdjHt)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                         !shlcodes - before next code kicks in
             TempAdjHt= TempAdjHt*((0.800/TempAdjHt)**(1./(2020.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016, complete adoption by 2024
                      **(2020.-2015.)                                         !shlcodes
             NewShlAvgHtCA=NewShlAvgHtCA + CMNewFloorSpace(IREG,IBLDTP,IYR)* & !arra09-shlfactor
                ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*            & !arra09-shlfactor
              (TempAdjHt*(NewImprv**(1./(2040.-2003.)))                   &   !shlcodes - resume original rate of improvement
                      **FLOAT(CURIYR+(BASEYR-1)-2020))                        !shlcodes
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.))) &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                         !shlcodes - before next tier kicks in
             TempAdjCl= TempAdjCl*((0.871/TempAdjCl)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                         !shlcodes - before next code kicks in
             TempAdjCl= TempAdjCl*((0.800/TempAdjCl)**(1./(2020.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016, complete adoption by 2024
                      **(2020.-2015.)                                         !shlcodes
              NewShlAvgClCA=NewShlAvgClCA + CMNewFloorSpace(IREG,IBLDTP,IYR)* & !arra09-shlfactor
                ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*            & !arra09-shlfactor
              (TempAdjCl*(NewImprv**(1./(2040.-2003.)))                   &   !shlcodes - resume original rate of improvement
                      **FLOAT(CURIYR+(BASEYR-1)-2020))                        !shlcodes
         end select techNShlAvgCA
		 else
         techNShlAvg:  select case (IYR)                                  !shlcodes
         case ( :20) techNShlAvg    ! use growth target for new shell improvements prior to 2010 !shlcodes
           NewShlAvgHt=NewShlAvgHt + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !endshel-shlfactor
            ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*              & !endshel-shlfactor
            (NewImprv**(1./FLOAT(CMLastYr+(BASEYR-1)-CBECSyear)))       & !endshel
            **FLOAT(IYR+(BASEYR-1)-CBECSyear)                             !endshel
           NewShlAvgCl=NewShlAvgCl + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !endshel-shlfactor
            ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*              & !endshel-shlfactor
            (NewImprv**(1./FLOAT(CMLastYr+(BASEYR-1)-CBECSyear)))       & !endshel
            **FLOAT(IYR+(BASEYR-1)-CBECSyear)                             !endshel
         case (21:23) techNShlAvg   ! phase-in of ASHRAE 90.1-2007 starting in 2010 - 82.4% adoption by 2012 !shlcodes
            TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
            NewShlAvgHt=NewShlAvgHt + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                      ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*      & !arra09-shlfactor
             (TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.)))             & !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **FLOAT(CURIYR+(BASEYR-1)-2009))                        !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
            TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
            NewShlAvgCl=NewShlAvgCl + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                       ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
             (TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.)))             & !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **FLOAT(CURIYR+(BASEYR-1)-2009))                        !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
        case (24:26) techNShlAvg  ! phase-in remainder of ASHRAE 90.1-2007 improvement starting in 2013 !shlcodes
             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             NewShlAvgHt=NewShlAvgHt + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                        ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
              (TempAdjHt*((0.871/TempAdjHt)**(1./(2016.-2012.)))            &  !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2012))                         !shlcodes
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             NewShlAvgCl=NewShlAvgCl + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                        ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
              (TempAdjCl*((0.871/TempAdjCl)**(1./(2016.-2012.)))             & !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2012))                         !shlcodes
        case (27:35) techNShlAvg  ! phase-in of ASHRAE 90.1-2013 starting in 2016 would be complete by 2024 shlcodes
             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             TempAdjHt= TempAdjHt*((0.871/TempAdjHt)**(1./(2016.-2012.)))  &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                          !shlcodes - before next code kicks in
             NewShlAvgHt=NewShlAvgHt + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                        ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
              (TempAdjHt*((0.800/TempAdjHt)**(1./(2024.-2015.)))            &  !shlcodes - phase-in 90.1-2013 starting in 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2015))                         !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.)))  &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                          !shlcodes - before next tier kicks in
             TempAdjCl= TempAdjCl*((0.871/TempAdjCl)**(1./(2016.-2012.)))  &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                          !shlcodes - before next code kicks in
             NewShlAvgCl=NewShlAvgCl + CMNewFloorSpace(IREG,IBLDTP,IYR)*   & !arra09-shlfactor
                        ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*     & !arra09-shlfactor
              (TempAdjCl*((0.800/TempAdjCl)**(1./(2024.-2015.)))             & !shlcodes - phase-in 90.1-2013 starting in 2016
                      **FLOAT(CURIYR+(BASEYR-1)-2015))                         !shlcodes - envelope heat transfer improvement based on ACEEE 2014 Summer Study paper
           case (36:) techNShlAvg                                           !arra09 - back to "natural" improvement
             TempAdjHt= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)         !shlcodes - improvement up to code phase-in
             TempAdjHt= TempAdjHt*((0.893/TempAdjHt)**(1./(2012.-2009.))) &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                         !shlcodes - before next tier kicks in
             TempAdjHt= TempAdjHt*((0.871/TempAdjHt)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                         !shlcodes - before next code kicks in
             TempAdjHt= TempAdjHt*((0.800/TempAdjHt)**(1./(2024.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016, complete adoption by 2024
                      **(2024.-2015.)                                         !shlcodes
             NewShlAvgHt=NewShlAvgHt + CMNewFloorSpace(IREG,IBLDTP,IYR)* & !arra09-shlfactor
                ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*            & !arra09-shlfactor
              (TempAdjHt*(NewImprv**(1./(2040.-2003.)))                   &   !shlcodes - resume original rate of improvement
                      **FLOAT(CURIYR+(BASEYR-1)-2024))                        !shlcodes
             TempAdjCl= (NewImprv**(1./(2040.-2003.)))**(2009.-2003.)          !shlcodes - improvement up to code phase-in
             TempAdjCl= TempAdjCl*((0.893/TempAdjCl)**(1./(2012.-2009.))) &   !shlcodes - phase-in faster adoption of 90.1-2007 (10.7% of 12.9% better than 90.1-2004)
                      **(2012.-2009.)                                         !shlcodes - before next tier kicks in
             TempAdjCl= TempAdjCl*((0.871/TempAdjCl)**(1./(2016.-2012.))) &   !shlcodes - phase-in remainder of 90.1-2007 improvement by 2016
                      **(2015.-2012.)                                         !shlcodes - before next code kicks in
             TempAdjCl= TempAdjCl*((0.800/TempAdjCl)**(1./(2024.-2015.))) &   !shlcodes - phase-in 90.1-2013 starting in 2016, complete adoption by 2024
                      **(2024.-2015.)                                         !shlcodes
              NewShlAvgCl=NewShlAvgCl + CMNewFloorSpace(IREG,IBLDTP,IYR)* & !arra09-shlfactor
                ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*            & !arra09-shlfactor
              (TempAdjCl*(NewImprv**(1./(2040.-2003.)))                   &   !shlcodes - resume original rate of improvement
                      **FLOAT(CURIYR+(BASEYR-1)-2024))                        !shlcodes
         end select techNShlAvg
		 end if
                                                                        !endshel
 11   CONTINUE                                                          !endshel
                                                                        !endshel
      TempAvgHt=0.                                                      !endshel-shlfactor
      TempAvgCl=0.                                                      !endshel-shlfactor
      IF(TotNewFS.gt.0.) Then                                           !endshel-shlfactor
         TempAvgHt=NewShlAvgHt/TotNewFS                                 !endshel-shlfactor
         TempAvgCl=NewShlAvgCl/TotNewFS                                 !endshel-shlfactor
      END IF ! Check for new floorspace to calculate avg shell efficiency
                                                                        !endshel
! ShellFactor (,,1) applies to all flrspace constr'd prior to CURIYR   
    IF (IREG.EQ.9) THEN                                                                    !endshel
	      ShellHeatFactor(IBLDTP,IREG,1,curiyr)=                          & !endshel-shlfactor
       (ExistShBaseStockHt*(SurvFloorTotal(IREG,IBLDTP,CURIYR)-TotNewFS)& !endshel-shlfactor
       + NewShlAvgHtCA)/SurvFloorTotal(IREG,IBLDTP,CURIYR)                !endshel-shlfactor
      ShellCoolFactor(IBLDTP,IREG,1,curiyr)=                          & !endshel-shlfactor
       (ExistShBaseStockCl*(SurvFloorTotal(IREG,IBLDTP,CURIYR)-TotNewFS)& !endshel-shlfactor
       + NewShlAvgClCA) / SurvFloorTotal(IREG,IBLDTP,CURIYR)              !endshel-shlfactor
		else
      ShellHeatFactor(IBLDTP,IREG,1,curiyr)=                          & !endshel-shlfactor
       (ExistShBaseStockHt*(SurvFloorTotal(IREG,IBLDTP,CURIYR)-TotNewFS)& !endshel-shlfactor
       + NewShlAvgHt)/SurvFloorTotal(IREG,IBLDTP,CURIYR)                !endshel-shlfactor
      ShellCoolFactor(IBLDTP,IREG,1,curiyr)=                          & !endshel-shlfactor
       (ExistShBaseStockCl*(SurvFloorTotal(IREG,IBLDTP,CURIYR)-TotNewFS)& !endshel-shlfactor
       + NewShlAvgCl) / SurvFloorTotal(IREG,IBLDTP,CURIYR)              !endshel-shlfactor
	   end if
                                                                        !endshel
      IF(PRTDBGK.eq.1 .and. CURITR.eq.MAXITR) Then                      !endshel-shlfactor
       write(rcdbg,6003) curiyr, ibldtp, ireg,                        & !endshel
       ShellHeatFactor(IBLDTP,IREG,1,curiyr),ShellHeatFactor(IBLDTP,IREG,2,curiyr), & !endshel-shlfactor
       ExistShBaseStockHt, newshadjht, tempavght                          !endshel-shlfactor
       write(rcdbg,6004) curiyr, ibldtp, ireg,                        & !endshel
       ShellCoolFactor(IBLDTP,IREG,1,curiyr),ShellCoolFactor(IBLDTP,IREG,2,curiyr), & !endshel-shlfactor
       ExistShBaseStockCl, newshadjcl, tempavgcl                          !endshel-shlfactor
 6003  FORMAT (1X,I2,1X,I2,1X,I1,5(2X,F9.7),' heating')                 !endshel-shlfactor
 6004  FORMAT (1X,I2,1X,I2,1X,I1,5(2X,F9.7),' cooling')                 !endshel-shlfactor
      END IF  ! check for debug file to print shell values

	  	IF(IREG.EQ.9) THEN
		 ShellHeatFactor(IBLDTP,IREG,2,curiyr)=ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*newshadjhtCA             !shellkdbout !shlcodes
         ShellCoolFactor(IBLDTP,IREG,2,curiyr)=ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*newshadjclCA             !shellkdbout !shlcodes
		 else
         ShellHeatFactor(IBLDTP,IREG,2,curiyr)=ShellHeatFactor(IBLDTP,IREG,2,CBECSyear-1989)*newshadjht             !shellkdbout !shlcodes
         ShellCoolFactor(IBLDTP,IREG,2,curiyr)=ShellCoolFactor(IBLDTP,IREG,2,CBECSyear-1989)*newshadjcl             !shellkdbout !shlcodes
		end if                                                                        !endshel
 13   CONTINUE                                                          !endshel                                        
                                                                        !endshel

! Calculate data center share of large office buildings in                !dcadjust
! preparation for incremental adjustment to intensities.                  !dcadjust

       DatCtrShare=0.000002*(float(curiyr-cmfirstyr)**3.0) &              !dcadjust07 Updated equation for AEO07
                     -0.00002*(float(curiyr-cmfirstyr)**2.0) &            !dcadjust07 reset constant term for cmfirstyr
                           + 0.0006*(float(curiyr-cmfirstyr)) + 0.003043  !dcadjust07 update

! Calculate new and surviving service demands for the
! current year:


          		   ! Calculate U.S. total demand in each explicit misc. category 
           ! (sub-categories of s = 10), will share to BT by CD in service demand calculation.
			!zero out floor basesfor misc KK
            y = CURIYR
			BrewerFlrBase=0.
			LaundryFlrBase=0.
			MedFlrBase =0.
            ElevatorFlrBase =0. 
            EscalatorFlrBase =0. 
            TotFlrNoWhse =0.         
            KitchenFlrBase =0.   
            LabFlrBase =0.
          
            BrewerFlrBase = (CMSurvFloorTot(4,y) + CMSurvFloorTot(7,y)   & ! miscdetail - total food svc, and office space to share brewers
             + CMSurvFloorTot(8,y) + CMNewFlrSpace(4,y)                  & ! miscdetail
             + CMNewFlrSpace(7,y) + CMNewFlrSpace(8,y))* 1000.0            ! miscdetail
            LaundryFlrBase = (CMSurvFloorTot(5,y) + CMSurvFloorTot(6,y)  & ! miscdetail - total healthcare, lodging, and merc/service space to share laundry
             + CMSurvFloorTot(9,y) + CMNewFlrSpace(5,y)                  & ! miscdetail
             + CMNewFlrSpace(6,y) + CMNewFlrSpace(9,y))* 1000.0            ! miscdetail
            MedFlrBase = (CMSurvFloorTot(5,y) + CMSurvFloorTot(8,y)      & ! miscdetail - total healthcare and small office space to share
             + CMNewFlrSpace(5,y) + CMNewFlrSpace(8,y))* 1000.0            ! miscdetail    medical imaging equipment
            ElevatorFlrBase = (CMUSSurvFloorTot(y)-CMSurvFloorTot(3,y)   & ! miscdetail - total flsp excluding food sales to share elevators
             + CMUSNewFloorTot(y)-CMNewFlrSpace(3,y)) * 1000.0             ! miscdetail
            EscalatorFlrBase = (CMUSSurvFloorTot(y)-CMSurvFloorTot(3,y)  & ! miscdetail - total flsp excluding food sales, food service, small office,
             - CMSurvFloorTot(4,y) -CMSurvFloorTot(8,y)                  & ! miscdetail    and warehouse to share escalators
             - CMSurvFloorTot(10,y)) * 1000.0                            & ! miscdetail
             + (CMUSNewFloorTot(y)-CMNewFlrSpace(3,y)-CMSurvFloorTot(4,y)& ! miscdetail
             - CMSurvFloorTot(8,y) - CMSurvFloorTot(10,y)) * 1000.0        ! miscdetail
            TotFlrNoWhse = (CMUSSurvFloorTot(y)-CMSurvFloorTot(10,y)     & ! miscdetail - total flsp excluding warehouses to share electric vehicles
             + CMUSNewFloorTot(y)-CMNewFlrSpace(10,y)) * 1000.0            ! miscdetail
            KitchenFlrBase = (CMUSSurvFloorTot(y)-CMSurvFloorTot(7,y)    & ! miscdetail - total flsp excluding large office, small office, warehouse,
             - CMSurvFloorTot(8,y) -CMSurvFloorTot(10,y)                 & ! miscdetail    and other to share kitchen ventilation
             - CMSurvFloorTot(11,y)) * 1000.0                            & ! miscdetail
             + (CMUSNewFloorTot(y)-CMNewFlrSpace(7,y)-CMSurvFloorTot(8,y)& ! miscdetail
             - CMSurvFloorTot(10,y) - CMSurvFloorTot(11,y)) * 1000.0       ! miscdetail
            LabFlrBase = (CMSurvFloorTot(2,y) + CMSurvFloorTot(5,y)      & ! miscdetail - total education, healthcare, and other to share laboratory eqipment
             + CMSurvFloorTot(11,y) + CMNewFlrSpace(2,y)                 & ! miscdetail
             + CMNewFlrSpace(5,y) + CMNewFlrSpace(11,y))* 1000.0           ! miscdetail

!!!!KK Updated read inputs to Mels

			CoffeeBrewers = MarketPenetrationMels(8,y)*MelsELQ(8)
				CoffeeBrewers=(CoffeeBrewers/1000.0)*(BrewerFLrBase/1000.0)
			XfmrsDry = MarketPenetrationMels(1,y)*MelsELQ(1)                 	! miscdetail
				XfmrsDry = (XfmrsDry/1000.0) * (CMUSSurvFloorTot(y) + CMUSNewFloorTot(y))     ! miscdetail
            Security = MarketPenetrationMels(3,y)*MelsELQ(3)         			! miscdetail
				Security =( Security/1000.0 )* (CMUSSurvFloorTot(y) +   CMUSNewFloorTot(y)) 	! miscdetail
            ElVehicles = MarketPenetrationMels(9,y)*MelsELQ(9)        		! miscdetail
				ElVehicles = ElVehicles/1000.0 * (0.6 * (CMsurvFloorTot(10,y)+CMNewFlrSpace(10,y)) +  & ! miscdetail
             0.4 * TotFlrNoWhse/1000.0)    									! miscdetail
            KitchenVent = MarketPenetrationMels(2,y)*MelsELQ(2)        		! miscdetail
				KitchenVent = KitchenVent/1000.0 * (KitchenFlrBase/1000.0)    	! miscdetail
            LabRefFrz = MarketPenetrationMels(4,y)*MelsELQ(4)        			! miscdetail
				LabRefFrz = LabRefFrz/1000.0 * (LabFlrBase/1000.0)            	! miscdetail
            !VidDisplay = MarketPenetrationMels(6,y)*MelsELQ(6)              	! miscdetail
				!VidDisplay = VidDisplay/1000.0 * (TotFlrNoWhse/1000.0)         	! miscdetail
            LrgVidBoard = MarketPenetrationMels(7,y)*MelsELQ(7)             	! miscdetail
				LrgVidBoard = LrgVidBoard/1000.0 * (CMsurvFloorTot(1,y) +  CMNewFlrSpace(1,y)) 	! miscdetail
			FumeHoods = MarketPenetrationMels(10,y)*MelsELQ(10)         			! miscdetail
				FumeHoods = FumeHoods/1000 * (.121*(CMsurvFloorTot(11,y) + CMNewFlrSpace(11,y)))	
            Laundry = MarketPenetrationMels(11,y)*MelsELQ(11)         			! miscdetail
				Laundry = (Laundry/1000.0) * (LaundryFlrBase/1000.0)           	! miscdetail
            MedImaging = MarketPenetrationMels(5,y)*MelsELQ(5)             	! miscdetaill
				MedImaging = (MedImaging/1000.0) * (MedFlrBase/1000.0)           ! miscdetail
            Elevators = MarketPenetrationMels(12,y)*MelsELQ(12)             	! miscdetail
				Elevators = (Elevators/1000.0) * (ElevatorFlrBase/1000.0)      	! miscdetail
            Escalators = MarketPenetrationMels(13,y)*MelsELQ(13)              	! miscdetail
				Escalators = (Escalators/1000.0) * (EscalatorFlrBase/1000.0)   	! miscdetail
            
      IF (PRTDBGK.eq.1 ) THEN                                              ! misctest
        WRITE (RCDBG,*) ' year ',CURIYR+1989, ' iteration ', CURITR        ! misctest
        WRITE (RCDBG,*) ' Miscellaneous calculations for'                  ! misctest
       write(rcdbg,*) "XfmrsDry", "Security", "Elevators", "Escalators", & ! misctest
        "ElVehicles", "CoffeeBrewers", "KitchenVent", "Laundry",         & ! misctest
        "LabRefFrz", "FumeHoods", "MedImaging", "LrgVidBoard" ! misctest
                                                                           ! misctest
       write(rcdbg,*) XfmrsDry, Security, Elevators, Escalators,         & ! misctest
        ElVehicles, CoffeeBrewers, KitchenVent, Laundry, LabRefFrz,      & ! misctest
        FumeHoods, MedImaging, LrgVidBoard                     ! misctest
      END IF                                                               ! misctest

       DO 15 ISERV= 1,CMnumServ
        DO 15 IBLDTP= 1, CMnumBldg
         DO 15 IREG= 1, MNUMCR-2
          ! for brevity
          s = ISERV
          b = IBLDTP
          r = IREG
          y = CURIYR

          ! Treat major & minor services separately:
          ! For major services, the market is assumed saturated,
          ! but service demand for space heating and space
          ! cooling is sensitive to shell efficiency.  For
          ! minor services, a market penetration forecast for
          ! office equipment is used, and service demand is
          ! not sensitive to shell efficiency.

          ! New for AEO 2002, add penetration to end uses (cooling,
          ! ventilation, and other) for data center requirements.
          ! Data centers assumed to be growing percentage of
          ! large office space calculated as DatCtrShare.

          ! Major Services:
          IF ( ISERV .LE. CMnumMajServ ) THEN

           ! service demand in surviving floorspace
           ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR)= &
            ! 10**9 Btu/10**6 sq ft /1000 -> trill/10**6 sq ft;
            ! for lighting, -> 10**9 lumen yrs  / sq ft
            ServDmdIntenBASE (ISERV,IBLDTP,IREG) / 1000.0 * &
            SurvFloorTotal (IREG,IBLDTP,CURIYR)

           IF (b .eq. 7) THEN  !Add incremental intensity-data centers dcadjust
            ServDmdExBldg (s,b,r,y) = &                         !dcadjust 
             ServDmdExBldg (s,b,r,y)* (1.0 - DatCtrShare) + &   !dcadjust
             ServDmdExBldg (s,b,r,y)*dcf(s)*DatCtrShare         !dcadjust
           ENDIF ! Adjust intensity-data center share of large offices      dcadjust


            SSDnoShell(IREG,IBLDTP,ISERV)=ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR)  !Efficiency Index

           IF ( ISERV .eq. 1 )    & ! apply shell to heating directly   !endshel
             ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR)= &
             ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR) * &
             ShellHeatFactor (IBLDTP,IREG,1,curiyr)                            !endshel-shlfactor
           IF ( ISERV .eq. 2 )    & ! apply shell to cooling directly   !endshel-shlfactor
             ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR)=                & !endshel
             ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR) *               & !endshel
             ShellCoolFactor (IBLDTP,IREG,1,curiyr)                            !endshel-shlfactor

           ! service demand in new floorspace
           NewServDmd (ISERV,IBLDTP,IREG,CURIYR)= &
            ! 10**9 Btu/10**6 sq ft /1000 -> trill/10**6 sq ft;
            ! for lighting, -> 10**9 lumen yrs  / sq ft
            ServDmdIntenBASE (ISERV,IBLDTP,IREG) / 1000.0 * &
            CMNewFloorSpace(IREG,IBLDTP,CURIYR)

           IF (b .eq. 7) THEN  !Add incremental intensity-data centers dcadjust
            NewServDmd (s,b,r,y) = &                                     !dcadjust 
             NewServDmd (s,b,r,y)* (1.0 - DatCtrShare) + &               !dcadjust
             NewServDmd (s,b,r,y)*dcf(s)*DatCtrShare                     !dcadjust
           ENDIF ! Adjust intensity-data center share of large offices    dcadjust

            NSDnoShell(IREG,IBLDTP,ISERV)=NewServDmd (ISERV,IBLDTP,IREG,CURIYR)  !Efficiency Index

           ! Account for variation of SD intensity between new
           ! and existing stock of buildings, due to different
           ! proportions of serviced floorspace:
           IF (ISERV .LE. CMnumVarSDI) &
             NewServDmd (ISERV,IBLDTP,IREG,CURIYR)=    &
             NewServDmd (ISERV,IBLDTP,IREG,CURIYR) *   &
              ServicedFlrspcProp (IBLDTP,ISERV,2)  /   & !2->new
              ServicedFlrspcProp (IBLDTP,ISERV,1)        !1->existing
           ! Account for effect of shell efficiency improvements:

           IF ( ISERV .eq. 1 ) &
             NewServDmd (ISERV,IBLDTP,IREG,CURIYR)= &
             NewServDmd (ISERV,IBLDTP,IREG,CURIYR) * &
              ShellHeatFactor(IBLDTP,IREG,2,curiyr)                     !endshel-shlfactor-shellkdbout (replace calc w stored value)
           IF ( ISERV .eq. 2 )                                           & !endshel
             NewServDmd (ISERV,IBLDTP,IREG,CURIYR)=                      & !endshel
             NewServDmd (ISERV,IBLDTP,IREG,CURIYR) *                     & !endshel
              ShellCoolFactor(IBLDTP,IREG,2,curiyr)                     !endshel-shlfactor-shellkdbout (replace calc w stored value)

          ! Minor Services:
          ELSE

           IF (s .LT. CMnumServ) THEN  !PCs and Other Office Equipment   miscdetail
            ! service demand in surviving floorspace
            ServDmdExBldg (s,b,r,y)= &
             ! 10**9 Btu/10**6 sq ft /1000 -> trill/10**6 sq ft;
             ServDmdIntenBASE (s,b,r)/1000.0 * SurvFloorTotal (r,b,y)

            ! service demand in new floorspace
            NewServDmd (s,b,r,y)= &
             ! 10**9 Btu/10**6 sq ft /1000 -> trill/10**6 sq ft;
             ServDmdIntenBASE (s,b,r)/1000.0 * CMNewFloorSpace(r,b,y)

            IF (b .eq. 7) THEN  !Add incremental intensity-data centers dcadjust
             ServDmdExBldg (s,b,r,y) = &                         !dcadjust 
              ServDmdExBldg (s,b,r,y)* (1.0 - DatCtrShare) + &   !dcadjust
              ServDmdExBldg (s,b,r,y)*dcf(s)*DatCtrShare         !dcadjust

             NewServDmd (s,b,r,y) = &                            !dcadjust 
              NewServDmd (s,b,r,y)* (1.0 - DatCtrShare) + &      !dcadjust
              NewServDmd (s,b,r,y)*dcf(s)*DatCtrShare            !dcadjust
            ENDIF ! Adjust intensity-data center share of large offices      dcadjust

            ! Apply market penetration factor
            IF (s .EQ. 8) THEN  ! Office PCs

             ServDmdExBldg (s,b,r,y)= &
              ServDmdExBldg (s,b,r,y) * MarketPenetrationMels (14,y)

             NewServDmd (s,b,r,y)= &
              NewServDmd (s,b,r,y) * MarketPenetrationMels (14,y)

            END IF ! office PCs market penetration

            IF (s .EQ. 9) THEN  ! Office Non-PCs

             ServDmdExBldg (s,b,r,y)= &
              ServDmdExBldg (s,b,r,y) * MarketPenetrationMels (15,y)

             NewServDmd (s,b,r,y)= &
              NewServDmd (s,b,r,y) * MarketPenetrationMels (15,y)

            END IF ! office non-PCs market penetration

           ELSE  ! Miscellaneous (Other) End Uses   miscdetail
           ! Calculate service demand based on miscdetail categories, only apply Offpen
           ! MarketPenetration to uses not categorized, i.e. "other miscellaneous".

           ! Non-explicit (other miscellaneous) service demand
           ! service demand in surviving floorspace
           ! 10**9 Btu/10**6 sq ft /1000 -> trill/10**6 sq ft;
            ServDmdExBldg (s,b,r,y)=    &
             ServDmdIntenBASE (s,b,r) / 1000.0 *    &           
            (MarketPenetrationMels(16,y) * (1-xplicitmiscshr(b)) *         & ! miscdetail
              FuelShareofServiceBASE(r,b,s,1)                         & ! Misc
             + FuelShareofServiceBASE(r,b,s,2) +                      & ! Misc
               FuelShareofServiceBASE(r,b,s,3)) *                     & ! Misc
             SurvFloorTotal (r,b,y)

           ! service demand in new floorspace
           ! 10**9 Btu/10**6 sq ft /1000 -> trill/10**6 sq ft;
            NewServDmd (s,b,r,y)= &
             ServDmdIntenBASE (s,b,r) / 1000.0 *   &
            (MarketPenetrationMels(16,y) * (1-xplicitmiscshr(b)) *         & ! miscdetail
              FuelShareofServiceBASE(r,b,s,1)                         & ! Misc
             + FuelShareofServiceBASE(r,b,s,2) +                      & ! Misc
               FuelShareofServiceBASE(r,b,s,3)) *                     & ! Misc
             CMNewFloorSpace(r,b,y)

      IF (PRTDBGK.eq.1 .and. b.eq.1 .and. r.eq.1 ) THEN                 !misctest
        WRITE (RCDBG,*) ' year ',CURIYR+1989, ' iteration ', CURITR     !misctest
        WRITE (RCDBG,*) ' Miscellaneous demand before explicit adds'    !misctest
       write(rcdbg,*) "ServDmdExBldg ", ServDmdExBldg (s,b,r,y)         !misctest
       write(rcdbg,*) "NewServDmd ", NewServDmd (s,b,r,y)               !misctest
      END IF                                                            !misctest
           
           ! Add demand for each explicit misc. category, shared by BT and CD
           
            SurvFlrbsf = SurvFloorTotal(r,b,y) / 1000.0                    ! miscdetail - surviving flsp in billion sf for ease of sharing
            CMNewFlrbsf = CMNewFloorSpace(r,b,y) / 1000.0                  ! miscdetail - new flsp in billion sf for ease of sharing
            BrewerFlrBase = (CMSurvFloorTot(4,y) + CMSurvFloorTot(7,y)   & ! miscdetail - total food svc, and office space to share brewers
             + CMSurvFloorTot(8,y) + CMNewFlrSpace(4,y)                  & ! miscdetail
             + CMNewFlrSpace(7,y) + CMNewFlrSpace(8,y))* 1000.0            ! miscdetail
            LaundryFlrBase = (CMSurvFloorTot(5,y) + CMSurvFloorTot(6,y)  & ! miscdetail - total healthcare, lodging, and merc/service space to share laundry
             + CMSurvFloorTot(9,y) + CMNewFlrSpace(5,y)                  & ! miscdetail
             + CMNewFlrSpace(6,y) + CMNewFlrSpace(9,y))* 1000.0            ! miscdetail
            MedFlrBase = (CMSurvFloorTot(5,y) + CMSurvFloorTot(8,y)      & ! miscdetail - total healthcare and small office space to share
             + CMNewFlrSpace(5,y) + CMNewFlrSpace(8,y))* 1000.0            ! miscdetail    medical imaging equipment
            ElevatorFlrBase = (CMUSSurvFloorTot(y)-CMSurvFloorTot(3,y)   & ! miscdetail - total flsp excluding food sales to share elevators
             + CMUSNewFloorTot(y)-CMNewFlrSpace(3,y)) * 1000.0             ! miscdetail
            EscalatorFlrBase = (CMUSSurvFloorTot(y)-CMSurvFloorTot(3,y)  & ! miscdetail - total flsp excluding food sales, food service, small offices, and warehouses
             - CMSurvFloorTot(4,y) -CMSurvFloorTot(8,y)                  & ! miscdetail    to share escalators
             - CMSurvFloorTot(10,y)) * 1000.0                            & ! miscdetail
             + (CMUSNewFloorTot(y)-CMNewFlrSpace(3,y)-CMSurvFloorTot(4,y)& ! miscdetail
             - CMSurvFloorTot(8,y) - CMSurvFloorTot(10,y)) * 1000.0        ! miscdetail
            TotFlrNoWhse = (CMUSSurvFloorTot(y)-CMSurvFloorTot(10,y)     & ! miscdetail - total flsp excluding warehouses to share electric vehicles
             + CMUSNewFloorTot(y)-CMNewFlrSpace(10,y)) * 1000.0            ! miscdetail
            KitchenFlrBase = (CMUSSurvFloorTot(y)-CMSurvFloorTot(7,y)    & ! miscdetail - total flsp excluding large office, small office, warehouse,
             - CMSurvFloorTot(8,y) -CMSurvFloorTot(10,y)                 & ! miscdetail    and other to share kitchen ventilation
             - CMSurvFloorTot(11,y)) * 1000.0                            & ! miscdetail
             + (CMUSNewFloorTot(y)-CMNewFlrSpace(7,y)-CMSurvFloorTot(8,y)& ! miscdetail
             - CMSurvFloorTot(10,y) - CMSurvFloorTot(11,y)) * 1000.0       ! miscdetail
            LabFlrBase = (CMSurvFloorTot(2,y) + CMSurvFloorTot(5,y)      & ! miscdetail - total education, healthcare, and other to share laboratory eqipment
             + CMSurvFloorTot(11,y) + CMNewFlrSpace(2,y)                 & ! miscdetail
             + CMNewFlrSpace(5,y) + CMNewFlrSpace(11,y))* 1000.0           ! miscdetail

            IF (y .eq. CMFirstYr) THEN  ! miscdetail - prepare for 1st model yr xfrmr sharing - FinalEndUseCon not available
             BaseElTotR =  0.0   ! miscdetail - initialize total prior to summation
             DO nb = 1, CMnumBldg  ! sum contribution to base year electricity use over building types to share xfrmr Q within CD
              BaseElTotR =  BaseElTotR + ComEUI(r,nb,10,1)*CMTotalFlspc(r,nb,y-1)
             END DO  ! miscdetail sum base year electricity contribution over building types
            END IF   ! check for first model year for special xfrmr sharing

            miscbyBT:  select case (b)                                                 ! miscdetail

             case(1)     ! assembly includes large video boards and uses in all bldgs
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= 0.0                                             ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= KitchenVent*(CMTotalFlspc(r,b,y)/KitchenFlrBase)  ! miscdetail
              LabRefFrzElQ(r,b,y)= 0.0                                                 ! miscdetail
              !VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= LrgVidBoard*(SurvFlrbsf/CMSurvFloorTot(b,y))      ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= 0.0                                                   ! miscdetail
              MedImagingElQ(r,b,y)= 0.0                                                ! miscdetail
              ElevatorsElQ(r,b,y)= Elevators*(CMTotalFlspc(r,b,y)/ElevatorFlrBase)     ! miscdetail
              EscalatorsElQ(r,b,y)= Escalators*(CMTotalFlspc(r,b,y)/EscalatorFlrBase)  ! miscdetail
              
              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
               + LrgVidBoardElQ(r,b,y)                          ! miscdetail

              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail

              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

             case(2)     ! education includes uses in all bldgs
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= 0.0                                             ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= KitchenVent*(CMTotalFlspc(r,b,y)/KitchenFlrBase)  ! miscdetail
              LabRefFrzElQ(r,b,y)= LabRefFrz*(CMTotalFlspc(r,b,y)/LabFlrBase)          ! miscdetail
              !VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= 0.0                                                   ! miscdetail
              MedImagingElQ(r,b,y)= 0.0                                                ! miscdetail
              ElevatorsElQ(r,b,y)= Elevators*(CMTotalFlspc(r,b,y)/ElevatorFlrBase)     ! miscdetail
              EscalatorsElQ(r,b,y)= Escalators*(CMTotalFlspc(r,b,y)/EscalatorFlrBase)  ! miscdetail
              
              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
                + LrgVidBoardElQ(r,b,y)                          ! miscdetail

              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail

              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

             case(3)     ! food sales exclude elevators and escalators, includes uses in all bldgs
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= 0.0                                             ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= KitchenVent*(CMTotalFlspc(r,b,y)/KitchenFlrBase)  ! miscdetail
              LabRefFrzElQ(r,b,y)= 0.0                                                 ! miscdetail
              !VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= 0.0                                                   ! miscdetail
              MedImagingElQ(r,b,y)= 0.0                                                ! miscdetail
              ElevatorsElQ(r,b,y)= 0.0                                                 ! miscdetail
              EscalatorsElQ(r,b,y)= 0.0                                                ! miscdetail

              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
               + LrgVidBoardElQ(r,b,y)                          ! miscdetail

              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail

              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

             case(4)     ! food service includes coffee brewers, elevators, uses in all bldgs
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= CoffeeBrewers*(CMTotalFlspc(r,b,y)/BrewerFlrBase) ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= KitchenVent*(CMTotalFlspc(r,b,y)/KitchenFlrBase)  ! miscdetail
              LabRefFrzElQ(r,b,y)= 0.0                                                 ! miscdetail
              !VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= 0.0                                                   ! miscdetail
              MedImagingElQ(r,b,y)= 0.0                                                ! miscdetail
              ElevatorsElQ(r,b,y)= Elevators*(CMTotalFlspc(r,b,y)/ElevatorFlrBase)     ! miscdetail
              EscalatorsElQ(r,b,y)= 0.0                                                ! miscdetail

              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
                + LrgVidBoardElQ(r,b,y)                          ! miscdetail

              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail

              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

             case(5)       ! health includes MRI, CT Scanner, Xray, and ultrasounds (MedImaging); laundry; uses in all bldgs
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= 0.0                                             ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= KitchenVent*(CMTotalFlspc(r,b,y)/KitchenFlrBase)  ! miscdetail
              LabRefFrzElQ(r,b,y)= LabRefFrz*(CMTotalFlspc(r,b,y)/LabFlrBase)          ! miscdetail
              !VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= Laundry*(CMTotalFlspc(r,b,y)/LaundryFlrBase)          ! miscdetail
              MedImagingElQ(r,b,y)= MedImaging*(CMTotalFlspc(r,b,y)/MedFlrBase)        ! miscdetail
              ElevatorsElQ(r,b,y)= Elevators*(CMTotalFlspc(r,b,y)/ElevatorFlrBase)     ! miscdetail
              EscalatorsElQ(r,b,y)= Escalators*(CMTotalFlspc(r,b,y)/EscalatorFlrBase)  ! miscdetail

              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
                + LrgVidBoardElQ(r,b,y)                          ! miscdetail
              
              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail

              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

             case(6)       ! lodging includes laundry, uses in all bldgs
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= 0.0                                             ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= KitchenVent*(CMTotalFlspc(r,b,y)/KitchenFlrBase)  ! miscdetail
              LabRefFrzElQ(r,b,y)= 0.0                                                 ! miscdetail
              !VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= Laundry*(CMTotalFlspc(r,b,y)/LaundryFlrBase)          ! miscdetail
              MedImagingElQ(r,b,y)= 0.0                                                ! miscdetail
              ElevatorsElQ(r,b,y)= Elevators*(CMTotalFlspc(r,b,y)/ElevatorFlrBase)     ! miscdetail
              EscalatorsElQ(r,b,y)= Escalators*(CMTotalFlspc(r,b,y)/EscalatorFlrBase)  ! miscdetail

              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
                + LrgVidBoardElQ(r,b,y)                          ! miscdetail
              
              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail
               
              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

             case(7)       ! lg. off. include coffee brewers, uses in all bldgs, and data center adjustments
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= CoffeeBrewers*(CMTotalFlspc(r,b,y)/BrewerFlrBase) ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= 0.0                                               ! miscdetail
              LabRefFrzElQ(r,b,y)= 0.0                                                 ! miscdetail
              !VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= 0.0                                                   ! miscdetail
              MedImagingElQ(r,b,y)= 0.0                                                ! miscdetail
              ElevatorsElQ(r,b,y)= Elevators*(CMTotalFlspc(r,b,y)/ElevatorFlrBase)     ! miscdetail
              EscalatorsElQ(r,b,y)= Escalators*(CMTotalFlspc(r,b,y)/EscalatorFlrBase)  ! miscdetail
              
              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
                + LrgVidBoardElQ(r,b,y)                          ! miscdetail
              
              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail
               
              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

               !Add incremental intensity-data centers dcadjust
               !Apply to entire service demand to account for additional transformer requirements
              ServDmdExBldg (s,b,r,y) =                                              & !dcadjust 
               ServDmdExBldg (s,b,r,y)* (1.0 - DatCtrShare) +                        & !dcadjust
               ServDmdExBldg (s,b,r,y)*dcf(s)*DatCtrShare                              !dcadjust

              NewServDmd (s,b,r,y) =                                                 & !dcadjust 
               NewServDmd (s,b,r,y)* (1.0 - DatCtrShare) +                           & !dcadjust
               NewServDmd (s,b,r,y)*dcf(s)*DatCtrShare                                 !dcadjust

             case(8)     ! sm. off. include coffee brewers, medical imaging, and uses in all bldgs
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= CoffeeBrewers*(CMTotalFlspc(r,b,y)/BrewerFlrBase) ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= 0.0                                               ! miscdetail
              LabRefFrzElQ(r,b,y)= 0.0                                                 ! miscdetail
             ! VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= 0.0                                                   ! miscdetail
              MedImagingElQ(r,b,y)= MedImaging*(CMTotalFlspc(r,b,y)/MedFlrBase)        ! miscdetail
              ElevatorsElQ(r,b,y)= Elevators*(CMTotalFlspc(r,b,y)/ElevatorFlrBase)     ! miscdetail
              EscalatorsElQ(r,b,y)= 0.0                                                ! miscdetail
              
              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
                + LrgVidBoardElQ(r,b,y)                          ! miscdetail
              
              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail
               
              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

             case(9) ! mercantile/services include laundry, uses in all buildings
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= 0.0                                             ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= KitchenVent*(CMTotalFlspc(r,b,y)/KitchenFlrBase)  ! miscdetail
              LabRefFrzElQ(r,b,y)= 0.0                                                 ! miscdetail
              !VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= Laundry*(CMTotalFlspc(r,b,y)/LaundryFlrBase)          ! miscdetail
              MedImagingElQ(r,b,y)= 0.0                                                ! miscdetail
              ElevatorsElQ(r,b,y)= Elevators*(CMTotalFlspc(r,b,y)/ElevatorFlrBase)     ! miscdetail
              EscalatorsElQ(r,b,y)= Escalators*(CMTotalFlspc(r,b,y)/EscalatorFlrBase)  ! miscdetail
              
              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
                + LrgVidBoardElQ(r,b,y)                          ! miscdetail

              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail
               
              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

             case(10)      ! warehouses include transformers, larger share of electric vehicles
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= 0.0                                             ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.6*ElVehicles*(SurvFlrbsf/CMSurvFloorTot(b,y))    ! miscdetail
              KitchenVentElQ(r,b,y)= 0.0                                               ! miscdetail
              LabRefFrzElQ(r,b,y)= 0.0                                                 ! miscdetail
              !VidDisplayElQ(r,b,y)= 0.0                                                ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= 0.0                                                 ! miscdetail
              LaundryElQ(r,b,y)= 0.0                                                   ! miscdetail
              MedImagingElQ(r,b,y)= 0.0                                                ! miscdetail
              ElevatorsElQ(r,b,y)= 0.0                                                 ! miscdetail
              EscalatorsElQ(r,b,y)= 0.0                                                ! miscdetail
              
              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
                + LrgVidBoardElQ(r,b,y)                          ! miscdetail
              
              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail
               
              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

             case(11)       ! other includes fume hoods for laboratories, uses in all bldgs
              XfmrsDryElQ(r,b,y)=XfmrsDry*(FinalEndUseCon(1,b,r,y-1)/QELCM(11,y-1))    ! miscdetail
              IF (y .eq. CMFirstYr) THEN  ! miscdetail - 1st model yr xfrmr sharing - FinalEndUseCon not available
               XfmrsDryElQ(r,b,y)=XfmrsDry*(QELCM(r,y-1)/QELCM(11,y-1)) *            & ! miscdetail - share to regional el. demand
                 ((ComEUI(r,b,10,1)*CMTotalFlspc(r,b,y-1))/BaseElTotR)                 ! miscdetail - share to BT share of el use
              END IF
              SecurityElQ(r,b,y)= Security*(CMTotalFlspc(r,b,y)/(CMUSSurvFloorTot(y) & ! miscdetail
              + CMUSNewFloorTot(y))/1000)                                              ! miscdetail
              CoffeeBrewersElQ(r,b,y)= 0.0                                             ! miscdetail
              ElVehiclesElQ(r,b,y)= 0.4*ElVehicles*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)  ! miscdetail
              KitchenVentElQ(r,b,y)= 0.0                                               ! miscdetail
              LabRefFrzElQ(r,b,y)= LabRefFrz*(CMTotalFlspc(r,b,y)/LabFlrBase)          ! miscdetail
              !VidDisplayElQ(r,b,y)= VidDisplay*(CMTotalFlspc(r,b,y)/TotFlrNoWhse)      ! miscdetail
              LrgVidBoardElQ(r,b,y)= 0.0                                               ! miscdetail
              FumeHoodsElQ(r,b,y)= FumeHoods*(SurvFlrbsf/CMSurvFloorTot(b,y))          ! miscdetail
              LaundryElQ(r,b,y)= 0.0                                                   ! miscdetail
              MedImagingElQ(r,b,y)= 0.0                                                ! miscdetail
              ElevatorsElQ(r,b,y)= Elevators*(CMTotalFlspc(r,b,y)/ElevatorFlrBase)     ! miscdetail
              EscalatorsElQ(r,b,y)= Escalators*(CMTotalFlspc(r,b,y)/EscalatorFlrBase)  ! miscdetail

              TotExplicitMiscElQ(r,b,y) = XfmrsDryElQ(r,b,y) + SecurityElQ(r,b,y)    & ! miscdetail
               + CoffeeBrewersElQ(r,b,y) + MedImagingElQ(r,b,y) + ElVehiclesElQ(r,b,y) & ! miscdetail
               + ElevatorsElQ(r,b,y) + EscalatorsElQ(r,b,y) + FumeHoodsElQ(r,b,y)    & ! miscdetail
               + LaundryElQ(r,b,y) + KitchenVentElQ(r,b,y) + LabRefFrzElQ(r,b,y)     & ! miscdetail
               + LrgVidBoardElQ(r,b,y)                          ! miscdetail
              
              ServDmdExBldg (s,b,r,y)= ServDmdExBldg (s,b,r,y) +                     & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (SurvFlrbsf /(SurvFlrbsf+CMNewFlrbsf))      ! miscdetail

              NewServDmd (s,b,r,y)= NewServDmd (s,b,r,y) +                           & ! miscdetail
               TotExplicitMiscElQ(r,b,y) * (CMNewFlrbsf /(SurvFlrbsf+CMNewFlrbsf))     ! miscdetail

            end select  miscbyBT

      IF (PRTDBGK.eq.1 .and. b.eq.1 .and. r.eq.1 ) THEN                !misctest
       WRITE (RCDBG,*) ' year ',CURIYR+1989, ' iteration ', CURITR     !misctest
       WRITE (RCDBG,*) ' Miscellaneous demand after explicit adds'     !misctest
       write(rcdbg,*) "CD,BT Survflr ", SurvFloorTotal(r,b,y)          !misctest
       write(rcdbg,*) "Nonwhs SurvFlr ", CMUSSurvFloorTot(y)-CMSurvFloorTot(10,y) !misctest
       write(rcdbg,*) "US Floor Total ", CMUSSurvFloorTot(y)+CMUSNewFloorTot(y)   !misctest
       write(rcdbg,*) "ServDmdExBldg ", ServDmdExBldg (s,b,r,y)        !misctest
       write(rcdbg,*) "NewServDmd ", NewServDmd (s,b,r,y)              !misctest
      END IF                                                           !misctest
           END IF ! PCs, other office vs. misc. end uses penetration

          END IF ! major/minor service separation

15     CONTINUE

       ! Decrement remaining service demands by amounts contributed
       ! by solar heat and lighting:
       DO 25 IREG=1, MNUMCR-2
        DO 25 IBLDTP=1, CMnumBldg
         DO 25 ISERV= 1, CMnumSolarServ

          ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR)= &
           ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR) - &
           ((SolarRenewableContrib (IREG,ISERV,CURIYR) / CMnumBldg) * &
             SurvFloorTotal (IREG,IBLDTP,CURIYR) / &
             (SurvFloorTotal (IREG,IBLDTP,CURIYR) + &
              CMNewFloorSpace (IREG,IBLDTP,CURIYR)))
          IF (ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR) .LT. 0.0) &
              ServDmdExBldg (ISERV,IBLDTP,IREG,CURIYR)=  0.0

          NewServDmd (ISERV,IBLDTP,IREG,CURIYR)= &
           NewServDmd (ISERV,IBLDTP,IREG,CURIYR) - &
           ((SolarRenewableContrib (IREG,ISERV,CURIYR) / CMnumBldg) * &
             CMNewFloorSpace (IREG,IBLDTP,CURIYR) / &
             (SurvFloorTotal (IREG,IBLDTP,CURIYR) + &
              CMNewFloorSpace (IREG,IBLDTP,CURIYR)))
          IF (NewServDmd (ISERV,IBLDTP,IREG,CURIYR) .LT. 0.0) &
              NewServDmd (ISERV,IBLDTP,IREG,CURIYR)=  0.0

   25  CONTINUE


!   ELASTICITY CALCULATIONS MOVED TO CONSUMPTION SUBROUTINE


       ! Split surviving building service demand into components
       ! subject to replacement and retrofit candidate decisions:
       DO 22 s= 1, CMnumServ
        DO 22 b= 1, CMnumBldg
         DO 22 r= 1, MNUMCR-2
          ! Calculate proportion of this SD that is in need of
          ! being serviced by replacement equipment due to equip
          ! failure:

          ReplacementProportion= 0.0
          DO TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)         ! all t supplying s
           DO v= 1, CMnumEqV (t)                ! all v this t
            IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
             IF (TechLife (t,v) .GT. 0.0) &
              ReplacementProportion= ReplacementProportion + &
              PrevYrTechShareofService (r,b,s,t,v) / TechLife (t,v)
            ENDIF
           END DO ! v
          END DO  ! t

           RetireServDmd (s,b,r,CURIYR)= &
             ServDmdExBldg (s,b,r,CURIYR) * &
             ReplacementProportion

           ServDmdSurv (s,b,r,CURIYR)= &
             ServDmdExBldg (s,b,r,CURIYR) - &
             RetireServDmd (s,b,r,CURIYR)

            RSDnoShell(r,b,s)=SSDnoShell(r,b,s)*ReplacementProportion   !Efficiency Index
            SSDnoShell(r,b,s)=SSDnoShell(r,b,s)-RSDnoShell(r,b,s)       !Efficiency Index

22     CONTINUE

       DO 490 ISERV= 1, CMnumServ
        DO 485 IBLDTP= 1, CMnumBldg
         RetireServDmd (ISERV,IBLDTP,11,CURIYR)= 0.0
         ServDmdSurv (ISERV,IBLDTP,11,CURIYR)= 0.0
         NewServDmd (ISERV,IBLDTP,11,CURIYR)= 0.0
         DO 485 IREG= 1, MNUMCR-2
          RetireServDmd (ISERV,IBLDTP,11,CURIYR)= &
             RetireServDmd (ISERV,IBLDTP,11,CURIYR) + &
             RetireServDmd (ISERV,IBLDTP,IREG,CURIYR)
          ServDmdSurv (ISERV,IBLDTP,11,CURIYR)= &
             ServDmdSurv (ISERV,IBLDTP,11,CURIYR) + &
             ServDmdSurv (ISERV,IBLDTP,IREG,CURIYR)
          NewServDmd (ISERV,IBLDTP,11,CURIYR)= &
             NewServDmd (ISERV,IBLDTP,11,CURIYR) + &
             NewServDmd (ISERV,IBLDTP,IREG,CURIYR)
485   CONTINUE
490   CONTINUE

!  KSDOUT NOW PROVIDES SERVICE DEMAND BY BUILDING TYPE
!      IF (PRTDBGK.EQ.1) THEN
!      WRITE (RCDBG,510) (BASEYR-1)+CURIYR, CURITR
!      DO 806 ISERV= 1, CMnumServ
!       WRITE (RCDBG,508) CMServices (ISERV)
!       WRITE (RCDBG,509) (RetireServDmd (ISERV,IBLDTP,11,CURIYR), &
!                          IBLDTP= 1, CMnumBldg)
!       WRITE (RCDBG,511) (ServDmdSurv (ISERV,IBLDTP,11,CURIYR), &
!                          IBLDTP= 1, CMnumBldg)
!       WRITE (RCDBG,512) (NewServDmd (ISERV,IBLDTP,11,CURIYR), &
!                          IBLDTP= 1, CMnumBldg)
!806   CONTINUE

!      END IF

!***** FORMAT STATEMENTS
500   FORMAT ('>>>>> Subroutine COMServiceDemand called ', &
              I4,1X,'Iter ',I2)
508   FORMAT (1X,A20)
509   FORMAT (1X,'Retiring SD',/,1X,11(F8.0,1X))
510   FORMAT (1X,'National Service Demand by Building Type',/, &
              1X,I4,1X,'Iteration ',I2)
511   FORMAT (1X,'Surviving SD',/,1X,11(F8.0,1X))
512   FORMAT (1X,'New SD',/,1X,11(F8.0,1X))
  520 FORMAT (//,' 1990 District Heat Service Demand Contribution', &
            /,' (Trillion BTU Out)',/,22X,A7,1X,A7,1X,(A20),/)
  530 FORMAT (1X,A20,3(1X,F7.3))

      RETURN
      END

! *******************************************************************
! This is the Technology Choice subroutine of the Commercial Module
! of the National Energy Modeling System, NEMS.
! *******************************************************************

      SUBROUTINE COMTechnologyChoice (RCDBG)
      IMPLICIT NONE
      include'parametr'    !  system-wide parameter constants
      include'ncntrl'      !  variables set by integrating module
      include'comparm'     !  commercial module parameters
      include'comvars'     !  commercial module common variables
      include'macout'      !  for 10 yr treasury bond interest rate
      include'eusprc'      !  for electricity prices by end-use
      include'apq'         !  for fuel price expectations
      include'mxpblk'      !  for fuel price expectations
      include'commrep'     !  nems ftab report variables
      INTEGER FILE_MGR     !  File Manager routine declaration
      INTEGER*4 RCDBG      !  KDEBUG file handle
      INTEGER*4 infile     !  Reusable file handle
      INTEGER*4 IOS        !  Reusable variable for read error number
      INTEGER*4 count      !  General purpose counter variable
      INTEGER*4 NumErr     !  General purpose data set error counter
      REAL*4 numerator     !  holding variable for use in divisions
      REAL*4 denominator   !  holding variable to avoid zero divide
      REAL*4 numpurch      !  holding variable for use in divisions
      REAL*4 denpurch      !  holding variable to avoid zero divide
      REAL*4 effic         !  holding variable for TechEff
      REAL*4 MinCost       !  holding variable for minimum cost

      REAL*4 MinCostShell(2)!  holding variable for a minimum cost of   
                            !  (1) existing and (2) new building shell  
                            !  efficiencies                             
      REAL*4 MinCostShell_ST(2)!  holding variable for a minimum cost of   
                            !  (1) existing and (2) new building shell  
                            !  efficiencies                             
      REAL*4 MinCostShell_SF(CMnumMajFl,2)!  holding variable for a minimum cost of        
                            !  (1) existing and (2) new building shell  
                            !  efficiencies                                                        
      REAL*4 MinCost_SF, MinCost_ST, MinCost_AF  !temporary for retrofitting
      REAL*4 ShellEffFactor(2) ! Variable used to adjust Shell Efficiencies  
                           !  for heating and cooling. 1=existing 2=new 
      INTEGER*4 tech       !  alternate variable for subscript t
      INTEGER*4 vint       !  alternate variable for subscript v
      INTEGER*4 TsubS      !  index into TechsforService array
      INTEGER*4 TechsubS   !  alternate variable for TsubS
      REAL*4 TechnologyShareN ! sum across equipment vintage shares
      REAL*4 TechnologyShareR ! sum across equipment vintage shares
      REAL*4 ConversionFactor ! assigned as needed for units conv.
      REAL*4 ExpectPrice   !  expected price for ann fuel cost calc
      
      REAL*4 ExpectPriceel !  expected price for ann fuel cost calc
      REAL*4 ExpectPriceng !  expected price for ann fuel cost calc
      REAL*4 ExpectPriceds !  expected price for ann fuel cost calc

      REAL*4 ReplacementProportion ! RSD(y)/TSD(y-1)
      REAL*4 Sum_1         !  For testing if sum of shares = 1.0
      REAL*4 Epsilon       !  plus/minus Epsilon (equality tolerance)
      INTEGER*4 q          ! ordinal equipment index
      INTEGER*4 WthrYear
      ! Year index into DegreeDay array; necessary because
      ! after short term forecast horizon, normal year values
      ! are used.

      INTEGER*4 NRG2007    ! switch for dec 2007 energy bill provisions: 0 - no bill, 1 - bill without unfunded provisions, 2 - bill with unfunded items - nrg07
      INTEGER*4 STIMULUS   ! switch for feb 2009 stimulus package provisions: 0 - no stimulus, 1 - with stimulus; default is with stimulus - arra09     
      INTEGER*4 RTOVALUE   ! External routine to get options
      EXTERNAL RTOVALUE

      INTEGER EPA111D
	  iNTEGER AB32SW

      INTEGER*2 AEOYR      !Index corresponding to current AEO year (AEO report year - 1989)
      COMMON /AEOReportYear/ AEOYR

      INTEGER*4 svc ! Service index used when s occupied
      REAL*4 Normalizer (CMDecision) ! Non-heatpump equip ms normalizer

      INTEGER*2 STRetBehav ! Same Tech behavior during Retrofit
      COMMON /CMBehaveSwitches/ STRetBehav

      INTEGER*2 CostTrendSwitch ! Use/don't use cost trend approach
      COMMON /CMCostSwitches/ CostTrendSwitch

      REAL*4 KEqCost  ! Equipment cost trend function
      REAL*4 EffectHurdle,EffectHurdleAdj ! EffectHurdleAdj usese hurdle rate elasticity for ST and SF decision types
      REAL*4 PriceDelta (CMnumMajFl) ! Change in fuel price relative to base yr
      REAL*4 PriceDelta3(CMnumMajFl) ! 3-yr avg ch in price relative to base yr
      !REAL*4 CompositePriceDelta     ! !hurelast2
      REAL*4 InvestDmd      ! Total demand requiring investment
      REAL*4 TechInvCost    ! Installed annual capital cost required to satisfy kBtu per hour for a given technology and vintage
      REAL*4 HPCLCOST       ! Cost of cooling originally subtracted from hp cost added back to obtain investment cost for hp
      REAL*4 CostperKBtu    ! Installed cost required per unit service demand for a given technology and vintage
                            !  =TechInvCost/8760*TechLife/CapacityFactor
      REAL*4 SubsidyperKBtu ! Subsidy associated with installed equipment per unit service demand for a given technology and vintage           !investsub
      REAL*4 Subsidy111dperKBtu ! 111d subsidy associated with installed equipment per unit service demand for a diven technology and vintage  !investsub 111(d)
      REAL*4 FSnormfac      ! Normalization factor to use in computing fuel shares for the Misc end use.

      INTEGER*4 elasbaseyr    ! holding variable for base year index
      integer*4 iforward(3)   !   calculation of price effects on tech menu  ! PITC
      integer*4 ifwd          !   holding variable for applied effect        ! PITC
      integer*4 IFMAX         !   maximum forward effect                     ! PITC
      integer*4 iLastSTEOyr   !   calendar year for last STEO year           ! PITC
      common /pitc/iforward,IFMAX,iLastSTEOyr                                ! PITC
      integer iExist, iNew, iLCTech, iLCVint, iCapCost, iMaintCost, iSubCost, iSub111dCost, iFYrAvail, iLYrAvail, iFuel, isFuelNew(CMnumMajFl) !investsub 111(d)
      integer iMinCostFuel_Exist, iMinCostFuel_New, t_AF_Stage1_Exist, v_AF_Stage1_Exist,t_AF_Stage1_New, v_AF_Stage1_New 
       REAL*4     SSDnoShell(MnumCR-2,CMnumBldg,CMnumServ)                 !Efficiency Index
       REAL*4     RSDnoShell(MnumCR-2,CMnumBldg,CMnumServ)                 !Efficiency Index
       REAL*4     NSDnoShell(MnumCR-2,CMnumBldg,CMnumServ)                 !Efficiency Index
       REAL*4     SDbyFuel(CMNumMajFl,CMnumServ,CMnumBldg,MNUMCR-2,MNUMYR) !Efficiency Index
       
         REAL*4     WHSE_EFF(mnumcr-2,CMnumServ,CMnumMajFl,mnumyr)             !Average Efficiency for IDM  !indytpc
         REAL*4     WHSE_SD(mnumcr-2,CMnumServ,CMnumMajFl,mnumyr)              !Service Demand for IDM  !indytpc

         !REAL*4     WHSE_ShellIndex(4,mnumyr),WHSE_LightIndex(4,mnumyr),WHSE_VentIndex(4,mnumyr)  !indytpc - defined in comparm
         !REAL*4     WHSE_HeatIndex(4,5,mnumyr)                                                    !indytpc - defined in comparm

       COMMON /EffInd/ SSDnoShell,RSDnoShell,NSDnoShell,SDbyFuel           !Efficiency Index

! Due to the extensive use of multiple (up to seven) array indices in
! almost all formulae, single characters are used for a more compact
! notation.  These are defined as follows:

      INTEGER*4 r,       & !  Region (Census Division)
                b,       & !  Building type
                s,       & !  Service
                d,       & !  Decision type (New, Replacement, Retrofit)
                u,       & !  Behavioral rule (LC, SF, ST)
                f,       & !  Fuel
                t,       & !  Technology
                v,       & !  Vintage
                p,       & !  Consumer time preference segment
                y,       & !  Year (1 = 1990)
                c,       & !  Costs (1 -> Installed Capital, 2 -> O&M)
                a,       & !  Availability (1 -> 1st yr, 2 -> last yr)
                iy         !  Temporary year index  !indytpc


! Calculated Variables:

      ! REAL*4 TechShareofService (MNUMCR,CMnumBldg,CMnumServ,CMnumTechs,CMnumEqVint)
      ! Proportion of a given service demand that is satisfied by
      ! equipment of a particular technology & vintage within a
      ! given Census Division, building type.  This array is
      ! initialized with 2012 CBECS shares (from file KTECH),
      ! and is computed for subsequent years.  For each forecast
      ! year it represents the market shares for the previous
      ! year, until it is recalculated for the current year in
      ! the Technology Choice subroutine.

        REAL*4 PurchTechShare (MNUMCR,CMnumBldg,CMnumServ,      & ! peff
                                       CMnumTechs,CMnumEqVint)    ! peff
      ! Proportion of a purchased service demand satisfied by       peff
      ! equipment of a particular technology & vintage within a     peff
      ! given Census Division, building type.  For each forecast    peff
      ! year it represents the market shares for the previous       peff
      ! year, until it is recalculated for the current year in      peff
      ! the Technology Choice subroutine.                           peff

      ! REAL*4 FuelShareofService (MNUMCR,CMnumBldg,CMnumServ,
      !                                           CMnumMajFl)
      ! For Census Division r, building type b, service s, and
      ! fuel f, FuelShareofService (r,b,s,f) is the proportion
      ! of service demand for service s in b,r that is satisfied
      ! by fuel f in the previous year, until updated for the
      ! current year in the Technology Choice subroutine.

        REAL*4 PurchFuelShare (MNUMCR,CMnumBldg,CMnumServ,     & ! peff
                                                  CMnumMajFl)    ! peff
      ! For Census Division r, building type b, service s, and     peff
      ! fuel f, PurchFuelShare (r,b,s,f) is the proportion of      peff
      ! purchased service demand for servie s in b,r satisfied     peff
      ! by fuel f in the previous year, until updated for the      peff
      ! current year in the Technology Choice subroutine.          peff

      ! REAL*4 AverageEfficiency (MNUMCR,CMnumBldg,CMnumServ,
      !                                           CMnumMajFl)
      ! AverageEfficiency (r,b,s,f) is the effective average
      ! efficiency of the equipment mix using fuel f to satisfy
      ! service demand s in building type b of Census Division r in
      ! the current year, as calculated in the Technology Choice
      ! subroutine.

      ! REAL*4 PrevYrTechShareofService (MNUMCR,CMnumBldg,
      !                 CMnumServ,CMnumTechs,CMnumEqVint)
      ! Proportion of a given service demand that was satisfied by
      ! equipment of a particular technology & vintage within a
      ! given Census Division, and building type during the
      ! previous year.

      ! REAL*4 PrevYrFuelShareofService (MNUMCR,CMnumBldg,
      !                             CMnumServ,CMnumMajFl)
      ! For Census Division r, building type b, service s, and
      ! fuel f, PrevYrFuelShareofService (r,b,s,f) is the proportion
      ! of service demand for service s in b,r that was satisfied
      ! by fuel f during the previous year.

      ! REAL*4 PrevYrAverageEfficiency (MNUMCR,CMnumBldg,
      !                                CMnumServ,CMnumMajFl)
      ! PrevYrAverageEfficiency (r,b,s,f) is the effective average
      ! efficiency of the equipment mix using fuel f to satisfy
      ! service demand s in building type b of Census Division r
      ! during the previous year.

      ! REAL*4 MS (CMnumBldg,CMnumServ,CMDecision,CMnumTechs,
      !                                           CMnumEqVint)
      ! Equipment market shares within decision segments.
      ! MS (b,s,d,t,v) is the proportion of service demand s
      ! subject to decision type d in Census Division r and building
      ! type b in the current year that is satisfied by equipment of
      ! technology t and vintage v, as calculated in the Technology
      ! Choice subroutine.

      REAL*4 FS (MNUMCR,CMnumBldg,CMnumServ,CMDecision,CMnumMajFl)
      ! Fuel shares within decision segments.
      ! FS (r,b,s,d,f) is the proportion of service demand s
      ! subject to Decision Type d in building b and Census
      ! Division r, that is satisfied by fuel f in the current year,
      ! as calculated in the Technology Choice subroutine.

      REAL*4 FSPRt (MNUMCR,CMnumBldg,CMnumServ,CMnumMajFl)             !  peff
      ! Fuel shares for retrofit purchases.  FSPRt (r,b,s,f)              peff
      ! is the proportion of purchased service demand s                   peff
      ! in building b and Census Division r, that is satisfied            peff
      ! by fuel f in the current year, as calculated in the               peff
      ! Technology Choice subroutine.                                     peff

      REAL*4 AE (MNUMCR,CMnumBldg,CMnumServ,CMDecision,CMnumMajFl)
      ! Average efficiencies within decision segments.
      ! AE (r,b,s,d,f) is the effective average efficiency
      ! of the equipment using fuel f to satisfy the Decision Type d
      ! segment of service demand s in building type b of Census
      ! Division r in the current year, as calculated in the
      ! Technology Choice subroutine.

      REAL*4 RegionFuelShare (MNUMCR,CMnumServ,CMnumMajFl)
      ! For Census Division r, service s, and fuel f,
      ! RegionFuelShare (r,s,f) is the proportion of service
      ! demand for service s in region r that is satisfied by fuel f
      ! in the current year.  It is obtained by consolidating
      ! FuelShareofService (r,b,s,f) across building types in the
      ! Technology Choice subroutine.

      REAL*4 RegionAvgEff (MNUMCR,CMnumServ,CMnumMajFl)
      ! RegionAvgEff (r,s,f) is the effective average
      ! efficiency of the equipment mix using fuel f to satisfy
      ! service demand s in Census Division r in the current year.
      ! It is obtained by consolidating AverageEfficiency (r,b,s,f)
      ! across building types in the Technology Choice subroutine.

      ! REAL*4 DecFuelShare (MNUMCR,CMnumServ,CMDecision,CMnumMajFl,
      !                                                      MNUMYR)
      ! For Census Division r, service s, and fuel f,
      ! DecFuelShare (r,s,d,f,y) is the proportion of service
      ! demand for service s in region r subject to decision type d
      ! that is satisfied by fuel f in year y.

      ! REAL*4 DecAvgEff (MNUMCR,CMnumServ,CMDecision,CMnumMajFl,
      !                                                  MNUMYR)
      ! DecAvgEff (r,s,d,f,y) is the effective average
      ! efficiency of the equipment mix selected in decision segment
      ! d using fuel f to satisfy service demand s in Census Division
      ! r in year y.

      REAL*4 AnnualCostTech (CMnumPref,CMnumTechs,CMnumEqVint,2)
      ! The annualized unit cost of a given vintage of a given
      ! technology per unit service demand (SD), incorporating
      ! annualized cost of capital per unit SD, annualized operating
      ! and maintenance costs per unit SD, annualized fuel costs
      ! per unit service demand, and consumer time preference
      ! premiums.  The last dimension allows different heating and
      ! cooling fuel costs for existing (1) and new (2) shells.    
      ! This array is re-computed for each value of r,b, and s in 
      ! order to support technology purchase decisions, and is not saved.

      REAL*4 AnnualCostTechAdj (CMnumPref,CMnumTechs,CMnumEqVint,2)    !hurelast2
      ! Same as above, but using adjusted hurdle rate elasticities for 
      ! same fuel and same tech choices.  All fuel choices which involve
      ! fuel switching use the undadjusted hurdle rates.

      REAL*4 ACE (CMnumTechs,CMnumEqVint,CMnumPref)
      ! The annualized cost of existing equipment.  Similar to the
      ! variable AnnualCostTech described above, but with a capital
      ! investment component due solely to the cost of retrofitting.
      ! This is for cost comparisons with new equipment priced in
      ! the AnnualCostTech array.

      INTEGER*4 LCTNR_AF (CMnumPref,2,2)                            
      ! A table generated for each r,b, and s that identifies the       
      ! least cost (LC) heating and cooling equipment (T)               
      ! as perceived by consumers in each time preference category      
      ! faced with a new or replacement (NR) decision type and          
      ! considering equipment across all fuels (AF).  LCTNR_AF 
      ! (p,1,*) holds the least cost technology subscript, t, under     
      ! those conditions, and  LCTNR_AF (p,2,*) holds the           
      ! corresponding vintage  subscript, v. LCTNR_AF (p,*,1)       
      ! holds the least cost for existing building shells               
      ! efficiencies. LCTNRAF (p,*,2)  holds the least cost for         
      ! with new building shell efficiencies.                           

      INTEGER*4 LCTRetAF (CMnumPref,CMnumTechs,CMnumEqVint,2)
      ! A table generated for each r,b, and s that identifies the
      ! least cost (LC) equipment (T) as perceived by consumers in
      ! each time preference category faced with a potential retrofit
      ! (Ret) decision involving surviving equipment, but considering
      ! equipment across all all fuels (AF).  The least cost choice
      ! will vary with the current equipment as well as the time
      ! preference. LCTRetAF (p,t,v,1) holds the least cost technology
      ! subscript under those conditions, and LCTRetAF (p,t,v,2) holds
      ! the corresponding vintage subscript.

      INTEGER*4 LCTNR_SF (CMnumPref,CMnumMajFl,2,2)                 
      ! A table generated for each r,b, and s that identifies the       
      ! least cost (LC) equipment (T) as perceived by consumers in      
      ! each time preference category faced with a new or replacement   
      ! (NR) decision type and considering equipment using the same     
      ! fuel (SF).  LCTNR_SF (p,f,1,*) holds the least cost         
      ! technology subscript, t, among all equipment using fuel f under 
      ! those conditions, and LCTNR_SF (p,f,2,*) holds the          
      ! corresponding vintage subscript. LCTNR_SF (p,f,*,1) holds   
      ! the value for existing shell efficiency improvements, and       
      ! LCTNR_SF (p,f,*,2) holds the value for new shell            
      ! efficiency improvements.                                        

      INTEGER*4 LCTRetSF (CMnumPref,CMnumTechs,CMnumEqVint,2)
      ! A table generated for each r,b, and s that identifies the
      ! least cost (LC) equipment (T) as perceived by consumers in
      ! each time preference category faced with a retrofit (Ret)
      ! decision type and considering equipment using the same
      ! fuel (SF).  For consumers with current equipment t,v,
      ! LCTRetSF (p,t,v,1) holds the least cost technology
      ! subscript among all equipment using the same fuel as t under
      ! those conditions, and LCTRetSF (p,t,v,2) holds the
      ! corresponding vintage subscript.

      INTEGER*4 LCVNR_ST (CMnumPref,CMnumTechs,2)                   
      ! A table generated for each r,b, and s that identifies the       
      ! least cost (LC) equipment (T) as perceived by consumers in      
      ! each time preference category faced with a new or replacement   
      ! (NR) decision type and considering equipment using the same     
      ! technology (ST).  LCVNR_ST (p,t) holds the least cost       
      ! vintage subscript, v, for each technology, t under those        
      ! conditions.  LCVNR_ST (p,t,1) holds the value when EXISTING 
      ! shell efficiency improvements are considered, and               
      ! LCVNR_ST (p,t,2) holds the value when NEW shell efficiency  
      ! improvements are considered.                                    

      INTEGER*4 LCVRetST (CMnumPref,CMnumTechs,CMnumEqVint)             
      ! A table generated for each r, b, and s, that identifies         
      ! the least cost (LC) vintage/model (V) of equipment in           
      ! each technology class, as perceived by purchasers in            
      ! each time preference segment faced with the retrofit            
      ! decision (Ret), and following the Same Technology behavior      
      ! rule (ST).                                                      

      REAL*4 LCMSN (CMnumTechs,CMnumEqVint)                             
      ! LCMSN (t,v) contains the market share (MS) of equipment of      
      ! technology t and vintage v within that segment of service       
      ! demand controlled by consumers following the least cost         
      ! behavior rule (LC) in making new or replacement decisions       
      ! (NR).  Here, 'market share' is defined as the ratio of the      
      ! amount of service demand satisfied by a particular type of      
      ! equipment to the total service demand within the given          
      ! segment. It replaces LCMSNR for new.                            

      REAL*4 LCMSR (CMnumTechs,CMnumEqVint)                             
      ! LCMSR (t,v) contains the market share (MS) of equipment of      
      ! technology t and vintage v within that segment of service       
      ! demand controlled by consumers following the least cost         
      ! behavior rule (LC) in making new or replacement decisions       
      ! (NR).  Here, 'market share' is defined as the ratio of the      
      ! amount of service demand satisfied by a particular type of      
      ! equipment to the total service demand within the given          
      ! segment. It replaces LCMSNR for replacement.                    

      REAL*4 LCMSRet (CMnumTechs,CMnumEqVint)
      ! LCMSRet (t,v) contains the market share (MS) of equipment of
      ! technology t and vintage v within that segment of service
      ! demand controlled by consumers following the least cost
      ! behavior rule (LC) in making retrofit decisions (Ret).
      ! Here, 'market share' is defined as the ratio of the amount
      ! of service demand satisfied by a particular type of
      ! equipment to the total service demand within the given
      ! segment.

      REAL*4 SFMSN (CMnumTechs,CMnumEqVint)
      ! SFMSN (t,v) contains the market share (MS) of equipment of
      ! technology t and vintage v within that segment of service
      ! demand controlled by consumers following the same fuel
      ! behavior rule (SF) in making the New purchase decision.
      ! (N).  Here, 'market share' is defined as the ratio of the
      ! amount of service demand satisfied by a particular type of
      ! equipment to the total service demand within the given
      ! segment.

      REAL*4 SFMSR (CMnumTechs,CMnumEqVint)
      ! SFMSR (t,v) contains the market share (MS) of equipment of
      ! technology t and vintage v within that segment of service
      ! demand controlled by consumers following the same fuel
      ! behavior rule (SF) in making Replacement purchase decisions
      ! (R).  Here, 'market share' is defined as the ratio of the
      ! amount of service demand satisfied by a particular type of
      ! equipment to the total service demand within the given
      ! segment.

      REAL*4 SFMSRet (CMnumTechs,CMnumEqVint)
      ! SFMSRet (t,v) contains the market share (MS) of equipment of
      ! technology t and vintage v within that segment of service
      ! demand controlled by consumers following the same fuel
      ! behavior rule (SF) in making retrofit decisions (Ret).
      ! Here, 'market share' is defined as the ratio of the
      ! amount of service demand satisfied by a particular type of
      ! equipment to the total service demand within the given
      ! segment.

      REAL*4 STMSN (CMnumTechs,CMnumEqVint)
      ! STMSN (t,v) contains the market share (MS) of equipment of
      ! technology t and vintage v within that segment of service
      ! demand controlled by consumers following the same technology
      ! behavior rule (ST) in making the New purchase decision
      ! (N).  Here, 'market share' is defined as the ratio of the
      ! amount of service demand satisfied by a particular type of
      ! equipment to the total service demand within the given
      ! segment.

      REAL*4 STMSR (CMnumTechs,CMnumEqVint)
      ! STMSR (t,v) contains the market share (MS) of equipment of
      ! technology t and vintage v within that segment of service
      ! demand controlled by consumers following the same technology
      ! behavior rule (ST) in making Replacement purchase decisions
      ! (R).  Here, 'market share' is defined as the ratio of the
      ! amount of service demand satisfied by a particular type of
      ! equipment to the total service demand within the given
      ! segment.

      REAL*4 STMSRet (CMnumTechs,CMnumEqVint)
      ! STMSRet (t,v) contains the market share (MS) of equipment of
      ! technology t and vintage v within that segment of service
      ! demand controlled by consumers following the same technology
      ! behavior rule (ST) in making retrofit decisions (Ret).
      ! Here, 'market share' is defined as the ratio of the amount
      ! of service demand satisfied by a particular type of
      ! equipment to the total service demand within the given
      ! segment.

      ! REAL*4 NSD (MNUMCR,CMnumBldg,CMnumServ,MNUMYR)
      ! New service demand associated with new floor space.
      ! Will replace NewServDmd in CMServDmdNw common block in
      ! Compar2 include file.

      ! REAL*4 RSD (MNUMCR,CMnumBldg,CMnumServ,MNUMYR)
      ! Replacement service demand associated with retiring equipment.
      ! Will replace RetireServDmd in CMServDmdNw common block in
      ! Compar2 include file.

      ! REAL*4 SSD (MNUMCR,CMnumBldg,CMnumServ,MNUMYR)
      ! Surviving service demand subject to retrofit decisions.
      ! Will replace ServDmdSurv in CMServDmdNw common block in
      ! Compar2 include file.

       REAL*4 PRtSD (MNUMCR,CMnumBldg,CMnumServ)                       ! peff
      ! Retrofit service demand involving equipment purchase. This       peff
      ! quantity is the portion of SSD involving retrofit purchases.     peff

       REAL*4 PurchSD (MNUMCR,CMnumBldg,CMnumServ)                     ! peff
      ! Total service demand involving equipment purchase.  This         peff
      ! quantity includes NSD, RSD and the portion of SSD involving      peff
      ! retrofit purchases.                                              peff

       REAL*4 InvestRetroDmd(CMnumTechs,CMnumEqVint)                   ! invest
      ! Retrofit demand requiring investment. InvestRetroDmd(t,v)        invest
      ! is the quantity of retrofit demand for which technology t,       invest
      ! vintage v is purchased to replace an existing technology.        invest

      ! REAL*4 InvestCost (MNUMCR,CMnumMajServ,14:MNUMYR)
      ! InvestCost (r,s,y) is the estimated investment in equipment required in region r tp satisfy service s in year y.
      ! Regions 1-9 are Census divisions, 11 is national.

      ! REAL*4 SubsidyInvestCost (MNUMCR,CMnumMajServ,14:MNUMYR)       !investsub
      ! SubsidyInvestCost (r,s,y) is the estimated investment in federal, local, or utility equipment subsidies for a given
      !  technology in region r to satisfy service s in year y.
      ! Regions 1-9 are census divisions, 11 is national.

      ! REAL*4 Subsidy111dInvestCost (MNUMCR,CMnumMajServ,14:MNUMYR)   !investsub 111(d)
      ! Subsidy111dInvestCost (r,s,y) is the estimated investment in utility equipment subsidy (for 111d analysis) for a given
      !  technology in region r to satisfy service s in year y.
      ! Regions 1-9 are census divisions, 11 is national.

! Variables read or set from Technology Characterization Table, KTECH:

      ! INTEGER*4 FuelbyTech (CMnumTechs,CMnumMajFl)
      ! Set to 1 if tech (row) uses fuel (col); 0 otherwise

      ! INTEGER*4 TechbyService (CMnumServ,CMnumTechs)
      ! Set to 1 if service (row) provided by tech (col); 0 otherwise

      ! INTEGER*4 CMnumTechsforService (CMnumServ)
      ! For service s, CMnumTecsforService (s) is the number of
      ! technologies input from the technology characterization
      ! file, KTECH, that provide that service.

      ! INTEGER*4 TechsforService (CMnumServ,CMnumTechs)
      ! For service s, TechsforService (s,TsubS) contains the
      ! technology subscript, t, of a technology providing
      ! service s, for each value of TsubS between 1 and
      ! CMnumTechsforService (s).  The value of t is that assigned
      ! in KTECH, and will be between 1 and CMnumTechs.

      ! REAL*4 TechShareofService (MNUMCR,CMnumBldg,CMnumServ,
      !                                 CMnumTechs,CMnumEqVint)
      ! Declared above under the heading, 'Calculated Variables.' The
      ! proportion of a given service demand that is satisfied by
      ! equipment of a particular technology & vintage within a given
      ! Census Division and building type for the previous year,
      ! until recalculated for the current year in the Technology
      ! Choice subroutine. This array is initialized for 2012
      ! using CBECS shares input from KTECH.

      ! REAL*4 TechEff (MNUMCR,CMnumServ,CMnumTechs,CMnumEqVint)
      ! Efficiencies of specific equipment, with allowance for
      ! regional variation, and equipment use for multiple services

      ! REAL*4 TechCost (MNUMCR,CMnumTechs,CMnumEqVint,4) - in comparm
      ! For equipment of technology t and vintage v for region r:
      ! TechCost(r,t,v,1) = installed capital cost per unit service demand
      ! TechCost(r,t,v,2) = annual O&M cost per unit service demand
      ! TechCost(r,t,v,3) = subsidy cost per unit service demand             !investsub
      ! TechCost(r,t,v,4) = 111d subsidy cost per unit service demand        !investsub 111(d)

      ! REAL*4 TechLife (CMnumTechs,CMnumEqVint)
      ! Average life expectancy of equipment, in years

      ! INTEGER*4 TechAvailability (MNUMCR,CMnumTechs,CMnumEqVint,2)
      ! For equipment of technology t and vintage v:
      ! TechAvailability(r,t,v,1) = calendar year first available to buy
      ! TechAvailability(r,t,v,2) = last year of equipment availability

! Other input variables:
      ! REAL*4 BehaviorShare (CMnumServ,CmnumBldg,CMDecision,CMnumRule)
      ! From file KBEHAV;
      ! For technology choice decision type d in building type b,
      ! and service type s,
      ! BehaviorShare (s,b,d,r) is the proportion of consumers following
      ! behavior rule r during the equipment selection process.

      ! REAL*4 FuelShareofService (MNUMCR,CMnumBldg,CMnumServ,
      !                                             CMNumMajFl)
      ! Declared above under the heading, 'Calculated Variables.'
      ! For Census Division r, building type b, service s, and
      ! fuel f, FuelShareofService (r,b,s,f) is the proportion
      ! of service demand for service s in b,r that is satisfied
      ! by fuel f in the previous year, until updated for the
      ! current year by the Technology Choice subroutine.

      ! REAL*4 CapacityFactor (MNUMCR-2,CMnumBldg,CMnumServ)
      ! From file KCAPFAC
      ! Capacity factors by region, building type, and service

      ! REAL*4 TimePrefProp (CMnumMajServ,CMnumPref,MNUMYR)
      ! From file KPREF.  For service s in year y:
      ! For consumer time preference level p, TimePrefProp (s,p,y) is
      ! the proportion of consumers who fall into that category.
      ! Referred to as P sub P in the model documentation.

      ! REAL*4 TimePrefPrem (CMnumMajServ,CMnumPref,MNUMYR)
      ! From file KPREF.  For service s in year y:
      ! For consumer time preference level p, TimePrefPrem (s,p,y) is
      ! the consumer time preference interest rate premium which is
      ! applicable to a proportion of the population given by
      ! TimePrefProp (s,p,y).
      ! Referred to as R sub CP in the model documentation.

      DATA Epsilon /.001/

      NRG2007  = RTOVALUE("NRG2007  ",0)  ! Get dec 2007 energy bill switch value nrg07
      STIMULUS = RTOVALUE("STIMULUS  ",0) ! Get stimulus package switch value arra09
      EPA111D = RTOVALUE("EPA111D ",0)  ! SCEDES switch to activate subsidies for 111(d) analysis
	AB32SW = RTOVALUE("AB32SW  ", 0)

      ! These are used as array subscripts to enhance the readability of the code
         iExist=1
         iNew=2
         iLCTech=1
         iLCVint=2
         iCapCost=1
         iMaintCost=2
         iSubCost=3      !investsub
         iSub111dCost=4  !investsub 111(d)
         iFYrAvail=1
         iLYrAvail=2

      IF (CURIYR.EQ.CMFirstYr .AND. CURITR.EQ.1) THEN
          !
          !  Initialize variables from input files if this is the first time
          !  COMTechnologyChoice has been called during this run:
          !   Zero out array in case file is incomplete
          !
                  DO s= 1, CMnumMajServ   ! Major Services only
                    DO b= 1, CMnumBldg
                      DO d= 1, CMDecision
                        DO u= 1, CMnumRule
                          BehaviorShare(s,b,d,u)=0.0
                        END DO  ! u
                      END DO  ! d
                    END DO  ! b
                  END DO  ! s
          !
          !       Read Behavioral Rule Shares:
          !
                  IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KBEHAV data set error trapping:'
                  infile= FILE_MGR ('O','KBEHAV',.FALSE.)
                  READ (infile,'(99(/))')  ! skip 100 line header
                  count = 0
                  NumErr = 0
                  DO 10 s= 1, CMnumMajServ   ! Major Services only
                    DO 10 b= 1, CMnumBldg
                     count = count + 1
                     READ (infile,*,ERR=8,END=11,IOSTAT=IOS) &
                         ((BehaviorShare (s,b,d,u), u= 1,CMnumRule), &
                                      d= 1,CMDecision)
                     DO u= 1, CMnumRule
                      DO d= 1, CMDecision
                       IF(BehaviorShare(s,b,d,u) .LT. 0.0 .OR. &
                        BehaviorShare(s,b,d,u) .GT. 1.0) Then
                       NumErr = NumErr + 1
                       IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'BehaviorShare(',s,b,d,u, &
                       ') =',BehaviorShare(s,b,d,u),' not reasonable.' &
                      ,'  Code sets to 0.0'
                       BehaviorShare(s,b,d,u) = 0.0
                       END IF
                      END DO ! d
                     END DO  ! u
                     GO TO 10
          !
            8       CONTINUE ! Report read error and loop to next read
                    NumErr = NumErr + 1
                    IF(PRTDBGK.EQ.1) &
                     WRITE(RCDBG,*) 'Comm_KBEHAV read err',IOS,' on record', &
                                      count,'; skip record and continue read.'
             10   CONTINUE
                  GO TO 12   ! EOF reached in KBEHAV when expected
             11   CONTINUE   ! EOF reached in KBEHAV prematurely; write msg
                  NumErr = NumErr + 1
                  IF(PRTDBGK.EQ.1) &
                    WRITE(RCDBG,*) 'KBEHAV EOF reached prematurely at s =' &
                        ,s,' b =',b,' d =',d,' u =',u
             12   CONTINUE
                  infile= FILE_MGR ('C','KBEHAV',.FALSE.)
          !
          !   Within a service, for a given b,d sum over u's should = 1.0
          !
                  DO s= 1, CMnumMajServ   ! Major Services only
                    DO b= 1, CMnumBldg
                      DO d= 1, CMDecision
                        sum_1 = 0.0
                        DO u= 1, CMnumRule
                          sum_1=sum_1+BehaviorShare(s,b,d,u)
                        END DO  ! u
                        IF(sum_1 .EQ. 0.0) Then
                          NumErr = NumErr + 1
                          BehaviorShare(s,b,d,3)=1.0
                          IF(PRTDBGK.EQ.1) Then
                            WRITE(RCDBG,*)'Sum of Behavior Shares for s =' &
                            ,s,' b =',b,' d =',d,' is 0.0'
                            WRITE(RCDBG,*)'Code sets Behavior Share for Same' &
                             ,' Technology =1.0 and continues.'
                          ENDIF
                        ELSEIF(sum_1.GT.1.0+Epsilon .OR. sum_1.LT.1.0-Epsilon) Then
                          NumErr = NumErr + 1
                          IF(PRTDBGK.EQ.1) &
                            WRITE(RCDBG,*)'Sum of Behavior Shares =',sum_1, &
                            ' not 1.0 for s =',s,' b =',b,' d =',d, &
                            '.  Code divides each by sum to normalize.'
                          DO u= 1, CMnumRule
                            BehaviorShare(s,b,d,u)=BehaviorShare(s,b,d,u)/sum_1
                          END DO  ! u
                        ENDIF
                      END DO      ! d
                    END DO        ! b
                  END DO          ! s
          !
                  IF(PRTDBGK.EQ.1) Then
                    WRITE(RCDBG,*) NumErr,' errors detected.'
                    WRITE(RCDBG,15)
          15        FORMAT(/,' KBEHAV data set error trapping complete!',/)
                  ENDIF
          !
          ! Technology data is read in the ServiceDemand subroutine in order
          ! to support the computation of base year service demand intensities
          ! using the technology efficiencies and the input EUIs.


          !    Read annual growth rates for minor service efficiencies:
          !      First, initialize array in case data is incomplete
          !
                  DO s= 1, CMnumServ
                      EffGrowthRate (s) = 0.0
                  END DO
          !
                  infile= FILE_MGR ('O','KDELEFF',.FALSE.)
                  READ (infile,'(99(/))')  ! skip over header
                IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KDELEFF data set error trapping:'
                  NumErr = 0
                  READ (infile,*,ERR=59,END=60,IOSTAT=IOS) &
                    (EffGrowthRate (s), s= 1, CMnumServ)
                      DO s= 1, CMnumServ
                       CEffGrowth(s)=EffGrowthRate(s)      !Efficiency Index
                      END DO 
                  GO TO 61
            59    CONTINUE
                  NumErr = 1
                  IF(PRTDBGK.EQ.1) &
                    WRITE(RCDBG,*) 'Comm_KDELEFF read err',IOS,' on record', &
                                      '; skip record.'
                  GO TO 61
            60    CONTINUE   ! EOF reached in KDELEFF prematurely; write msg
                  NumErr = 1
                  IF(PRTDBGK.EQ.1) &
                    WRITE(RCDBG,*) 'KDELEFF EOF reached prematurely.'
            61    CONTINUE
                  infile= FILE_MGR ('C','KDELEFF',.FALSE.)
          !
                  IF(PRTDBGK.EQ.1) Then
                    WRITE(RCDBG,*) NumErr,' errors detected.'
                    WRITE(RCDBG,62)
            62      FORMAT(/,' KDELEFF data set error trapping complete!',/)
                  ENDIF
          !
          !   Read Capacity Factors
          !    First, initialize capacity factors in case data is incomplete
          !
                  DO r= 1, MNUMCR-2
                    DO b= 1, CMnumBldg
                      DO s= 1,CMnumServ
                        CapacityFactor (r,b,s) = 0.0
                      ENDDO  ! s
                    ENDDO  ! b
                  ENDDO  ! r
          !
                  infile= FILE_MGR ('O','KCAPFAC',.FALSE.)
                  READ (infile,'(99(/))')  ! Skip over 100 line header
                 IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KCAPFAC data set error trapping:'
          !
                  count = 0
                  NumErr = 0
                  DO 70 r= 1, MNUMCR-2
                    DO 70 b= 1, CMnumBldg
                     count = count + 1
                     READ (infile,*,ERR=69,END=71,IOSTAT=IOS) &
                       (CapacityFactor (r,b,s), s= 1,CMnumServ)
                      DO s= 1,CMnumServ
                       IF(CapacityFactor(r,b,s) .LT. 0.0 .OR. &
                          CapacityFactor(r,b,s) .GT. 1.0) Then
                        NumErr = NumErr + 1
                        IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'CapacityFactor(',r,b,s, &
                         ') =',CapacityFactor(r,b,s),' not reasonable.' &
                         ,'  Code sets to 0.0'
                       CapacityFactor(r,b,s) = 0.0
                       END IF
                      ENDDO  ! s
                      GO TO 70
          !
            69      CONTINUE ! Report read error and loop to next read
                    NumErr = NumErr + 1
                    IF(PRTDBGK.EQ.1) &
                      WRITE(RCDBG,*) 'Comm_KCAPFAC read err',IOS,' on record', &
                                      count,'; skip record and continue read.'
            70    CONTINUE
                  GO TO 72   ! EOF reached in KCAPFAC when expected
            71    CONTINUE   ! EOF reached in KCAPFAC prematurely; write msg
                  NumErr = NumErr + 1
                  IF(PRTDBGK.EQ.1) &
                    WRITE(RCDBG,*) 'KCAPFAC EOF reached prematurely at r =' &
                        ,r,'and b =',b
            72    CONTINUE
                  infile= FILE_MGR ('C','KCAPFAC',.FALSE.)
          !
                  IF(PRTDBGK.EQ.1) Then
                    WRITE(RCDBG,*) NumErr,' errors detected.'
                    WRITE(RCDBG,73)
            73      FORMAT(/,' KCAPFAC data set error trapping complete!',/)
                  ENDIF
          !
          !    Read Consumer Time Preference Data; Check for Errors
          !     First, initialize arrays in case data is incomplete
          !
                 DO y= 1, LASTYR
                  DO s= 1, CMnumMajServ
                    DO p= 1, CMnumPref
                      TimePrefProp (s,p,y) = 0.0
                      TimePrefPrem (s,p,y) = 0.0
                    END DO  ! p
                  END DO  ! s
                 END DO  ! y
          !
                  infile= FILE_MGR ('O','KPREM',.FALSE.)
                  READ (infile,'(99(/))')  ! Skip over header
                  IF(PRTDBGK.EQ.1) WRITE(RCDBG,*)'KPREM data set error trapping:'
          !
                  count = 0
                  NumErr = 0
                  DO 80 y= 1, LASTYR
                  DO 80 s= 1, CMnumMajServ
                    DO 80 p= 1, CMnumPref
                      count = count + 1
                      READ (infile,*,ERR=79,END=81,IOSTAT=IOS) &
                          TimePrefProp (s,p,y), TimePrefPrem (s,p,y)
                    GO TO 80
          !
            79        CONTINUE ! Report read error and loop to next read
                      NumErr = NumErr + 1
                      IF(PRTDBGK.EQ.1) &
                        WRITE(RCDBG,*) 'Comm_KPREM read err',IOS,' on record', &
                                       count,'; skip record and continue read.'
            80    CONTINUE
                  GO TO 82    ! EOF reached in KPREM when expected
            81    CONTINUE    ! EOF reached in KPREM prematurely; write message
          !
                  NumErr = NumErr + 1
                  IF(PRTDBGK.EQ.1) &
                    WRITE(RCDBG,*) 'KPREM EOF reached prematurely at s =' &
                                   ,s,' p =',p,' y =',y+(BASEYR-1)
            82    CONTINUE
                  infile= FILE_MGR ('C','KPREM',.FALSE.)
          !
          !
          !   Check if shares are reasonable.
          !    Premium for lighting in public buildings was adjusted based on Dec 07 energy bill EISA07 - nrg07
          !    Fed buildings make up 2.59% of 2003 CBECS floorspace, use 2.5% in model, change is       - nrg07
          !    effective one year after enactment => 2009                                               - nrg07
          !    Premium for all uses in all government buildings was adjusted for 2010 and 2011 based    - arra09
          !    on Feb 2009 stimulus package funding for Federal, state and local construction and       - arra09
          !    energy efficiency projects.  Govt flsp % of new and replacement service demand used      - arra09
          !    as proxy for proportion to move to 10-yr T-bill rate.                                    - arra09
          !
                 DO y= CMFirstYr, LASTYR
                  DO s= 1, CMnumMajServ
                     sum_1=0.0
                    IF(STIMULUS .EQ. 0 ) THEN                                      !arra09 - for runs without stimulus package,
                     IF (y .GT. 20 .AND. y .LT. 25) THEN                           !arra09 - remove 2010 through 2013 changes for 
                       DO p= 1, CMnumPref                                          !arra09 - govt project funding
                        TimePrefProp(s,p,y)=TimePrefProp(s,p,y-1)                  !arra09
                       END DO  ! P - no stimulus spending on govt projects          arra09
                     END IF    ! check for 2010 through 2013 - no stimulus spending arra09
                     IF(NRG2007 .EQ. 0 ) THEN                                      !nrg07 - for runs without EISA07, remove
                      IF (s .EQ. 6 .AND. y .GT. 19) THEN                           !nrg07 - provision affecting lighting purchases
                       TimePrefProp(s,5,y)=0.100                                   !nrg07 - for all (not just new) GSA managed buildings
                       TimePrefProp(s,6,y)=0.008                                   !nrg07 - to use energy efficient lighting to the
                       TimePrefProp(s,7,y)=0.002                                   !nrg07 - maximum extent possible, 2009 on
                      END IF ! check for lighting end use and year                  nrg07
                     END IF  ! check for NRG2007 switch                             nrg07
                    ELSE     ! account for improbable situation: with stimulus package, no EISA07
                      IF (s .EQ. 6 .AND. y .GT. 19) THEN                           !nrg07 - provision affecting lighting purchases
                       IF (NRG2007 .EQ. 0) THEN                                    !nrg07 - for runs without EISA07
                        IF (y .EQ. 20 .OR. y .GT. 24) THEN                         !arra09 - remove EISA adjustments without affecting stimulus years
                         TimePrefProp(s,5,y)=0.100                                 !nrg07 - for all (not just new) GSA managed buildings
                         TimePrefProp(s,6,y)=0.008                                 !nrg07 - to use energy efficient lighting to the
                         TimePrefProp(s,7,y)=0.002                                 !nrg07 - maximum extent possible, 2009 on
                        END IF ! check to remove EISA but keep stimulus changes        
                       END IF  ! check for NRG2007 switch                           nrg07
                      END IF ! check for lighting end use and year                  nrg07
                    END IF  ! check for STIMULUS switch                             arra09
                     
                    
                    DO p= 1, CMnumPref
                      IF(TimePrefProp(s,p,y).LT.0.0 .OR. TimePrefProp(s,p,y) &
                        .GT. 1.0) Then
                       NumErr = NumErr + 1
                       IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'Proportion of Consumers =' &
                      ,TimePrefProp(s,p,y),' not reasonable.  Set to 0.0 for s =' &
                      ,s,' p =',p,' and y =',y+(BASEYR-1)
                       TimePrefProp(s,p,y) = 0.0
                      END IF
                      sum_1=sum_1+TimePrefProp(s,p,y)
                    END DO   ! P
                    IF(sum_1 .EQ. 0.0) Then
                      NumErr = NumErr + 1
                      TimePrefProp(s,1,y)=1.0
                      TimePrefPrem(s,1,y)=0.0
                      IF(PRTDBGK.EQ.1) Then
                        WRITE(RCDBG,*)'Proportion of Consumers for s =' &
                        ,s,' sums to 0.0 in year ',y+(BASEYR-1)
                        WRITE(RCDBG,*)'Code sets Proportion of Consumers(',s, &
                        ',1) to 1.0 and the corresponding premium to 0.0'
                      END IF
                    ELSEIF(sum_1.GT.1.0+Epsilon .OR. sum_1.LT.1.0-Epsilon) Then
                      NumErr = NumErr + 1
                      IF(PRTDBGK.EQ.1) &
                        WRITE(RCDBG,*)'Sum of Proportion of Consumers =',sum_1, &
                  ' not 1.0 for s =',s,' y = ',y+(BASEYR-1),'.  Code divides ', &
                         'each by sum_1 to normalize.'
                      DO p= 1, CMnumPref
                        TimePrefProp(s,p,y)=TimePrefProp(s,p,y) / sum_1
                      END DO  ! p
                    ENDIF     ! Test of sum_1
                  END DO    ! s
                 END DO  ! y

                  IF(PRTDBGK.EQ.1) Then
                    WRITE(RCDBG,*) NumErr,' errors detected.'
                    WRITE(RCDBG,83)
          83        FORMAT(/,' KPREM data set error trapping complete!',/)
                  ENDIF

          !  Modify heat pump capital cost to reflect incremental cost to provide
          !  heating, over and above cost to provide cooling of comparable cooling
          !  technology (specified by KTECH index CoolingTechIndexHP)
                DO t= 1, CMnumHeatPumpTechs ! reserved htg heat pump indices
                 IF (TechbyService(1,t) .EQ. 1) THEN ! for defined heat pumps
                  DO v= 1, CMnumEqV (t)
                   DO r= 1, MNUMCR-2  ! regionalize techcost
                    IF (TechbyModel(t,v) .EQ. 1) THEN ! for defined models
                     TechCost (r,t,v,iCapCost)= TechCost (r,t,v,iCapCost) - TechCost (r,CoolingTechIndexHP,v,iCapCost)
                    END IF  ! defined models
                   END DO   ! r regionalize techcost
                  END DO   ! v
                 END IF    ! defined heat pumps
                END DO     ! t

                END IF       ! End of first-call initialization

!       IF(CURIYR .EQ. AEOYR .AND. CURITR .EQ.1) STRetBehav=0   !turn off retrofit behavior with 7% discount rate (used primarily for technology side-case runs)

      !  The remainder of this subroutine is executed for each iteration
      !  of each forecast year:

      ! Initialize equipment service demand totals for this year:
       DO r= 1, MNUMCR                                
        Do b=1,  CMnumBldg                            
         DO t= 1, CMnumTechs
          DO v= 1, CMnumEqVint
           DO d= 1, CMDecision
            EquipSD (r,b,t,v,d,CURIYR)= 0.0           
           END DO ! d decision types
          END DO  ! v vintages
         END DO   ! t technoloiges
        END DO   ! building types                     
       END DO    ! r regions                          

      ! invest
      ! Initialize investment cost totals for this year
      DO s= 1, CMnumMajServ                                          ! invest
        DO r= 1, MNUMCR                                              ! invest
          InvestCost (r,s,CURIYR) = 0.0                              ! invest
          SubsidyInvestCost(r,s,CURIYR) = 0.0                        ! invest  !investsub
          Subsidy111dInvestCost(r,s,CURIYR) = 0.0                    ! invest  !investsub 111(d)
        END DO  ! r                                                  ! invest
      END DO  ! s                                                    ! invest

      ! Code to print debugging diagnostics for investment (additional code farther down)
!      IF (PRTDBGK.eq.1 .and. CURCALYR.ge.2017 .and. CURITR.eq.MAXITR+1) THEN
!              WRITE (RCDBG,*)
!              WRITE (RCDBG,*) ' Investment Information (equipment)'
!              WRITE (RCDBG,*) "Y  T V R  B ","InvRetrDmd ", &
!              "InvestDmd ","TInvCst ","CostperKBtu ","SubsidyperKBtu ","Subsidy111dperKBtu ", &
!              "InvestCost(r,s,y) ","SubsidyInvestCost(r,s,y) ","Subsidy111dInvestCost(r,s,y)"
!            END IF

      ! PITC
      PriceDelta3(1:3)= 0.0  
      !Electricity
      IF(curiyr.gt.ksteoyr) PriceDelta3(1)= & 
        ((pelcm(11,curiyr)+pelcm(11,curiyr-1)+pelcm(11,curiyr-2))/3.) & 
         /  pelcm(11,elasbaseyr) 
      !Natural Gas
      IF(curiyr.gt.ksteoyr) PriceDelta3(2)= & 
        ((pngcm(11,curiyr)+pngcm(11,curiyr-1)+pngcm(11,curiyr-2))/3.) &
          /  pngcm(11,elasbaseyr)                                      
      !Distillate Oil 
      IF(curiyr.gt.ksteoyr) PriceDelta3(3)= & 
        ((pdscm(11,curiyr)+pdscm(11,curiyr-1)+pdscm(11,curiyr-2))/3.) & 
          /  pdscm(11,elasbaseyr)                                       
      ! Set Advancment Years by Fuel for Price-Induced Technical Change aka "PITC"
      iLastSTEOyr=ksteoyr+baseyr-1   !used for setting max advance
      IF(curiyr .gt. ksteoyr) THEN
         DO f= 1, CMnumMajFl                                      
           if(curitr.eq.1)  ifwd=iforward(f)   !set to last year's
         !     Set minimum shift to what it was last year or to a greater shift
         Iforward(f) = min(Ifwd,-ifix( ((PriceDelta3(f)-1.0)/.10) ))         
         !     Set maximum shift to IFMAX from Kparm file         
         Iforward(f) = max(IFMAX,iforward(f))                     
         ENDDO                                                    
      ENDIF

      elasbaseyr = CBECSyear - (BASEYR-1)  !   del
      DO 9000 r= 1, MNUMCR-2           !  For each Census Division
       DO 9000 s= 1,CMnumMajServ       !  For each Major Service

         ! General factor for converting NEMS fuel prices to dollars per unit of service demand
         !  Note service demand in kBtu, NEMS prices in mmBtu (million Btu) --> factor is 8760/1000
         ConversionFactor= 8.76

         ! del -- discount rate elasticity (hurdle rate adjustments)
         ! Fuel specific price differences are computed relative to      
         ! the base year for the current census division.  This is  
         ! accomplished in preparation of an adjustment to discount 
         ! rates to account for the effects of changing fuel prices 
         PriceDelta (1:3)= 0.0  !the subscript represents major fuel
         !Electricity
         IF (PELCMOUT(r,elasbaseyr,s).GT.0.0) & 
           PriceDelta(1) = PELCMOUT(r,CURIYR,s)/ PELCMOUT(r,elasbaseyr,s)
         !Natural Gas
         IF (PNGCM(r,elasbaseyr).GT.0.0) & 
           PriceDelta(2) = PNGCM(r,CURIYR) / PNGCM(r,elasbaseyr)
         !Distillate Oil 
         IF (PDSCM(r,elasbaseyr).GT.0.0) & 
           PriceDelta(3) = PDSCM(r,CURIYR) / PDSCM(r,elasbaseyr)

         ! Composite price "delta" considered for use in adjusting hurdle rate                 
         !CompositePriceDelta=(PriceDelta(1)*qelcm(mnumcr,curiyr-1) &         
         !  + PriceDelta(2)*qngcm(mnumcr,curiyr-1)+PriceDelta(3)*qdscm(mnumcr,curiyr-1))/ &
         !  (qelcm(mnumcr,curiyr-1)+qngcm(mnumcr,curiyr-1)+qdscm(mnumcr,curiyr-1))                                                          

        DO 8000 b= 1, CMnumBldg        !  For each Building Type

         ! Compute shell factors for later use, these only vary by r and b, not equipment types        
          IF (s.LT.3) THEN                           
           ! Store Adjust Heat&Cool Shell Factors             
           IF (s.EQ.1) THEN
               ! Heating
               ShellEffFactor(iExist) = ShellHeatFactor(b,r,iExist,curiyr)  
               ShellEffFactor(iNew) =   ShellHeatFactor(b,r,iNew,curiyr)
           ELSE  
               ! Cooling
               ShellEffFactor(iExist) =  ShellCoolFactor(b,r,iExist,curiyr)
               ShellEffFactor(iNew) =    ShellCoolFactor(b,r,iNew,curiyr)
           ENDIF  
          ELSE ! If not heating or cooling, set shell index to 1.0
            ShellEffFactor(iExist) = 1.          
            ShellEffFactor(iNew)   = 1.
          ENDIF

         !  Initialize service demand variables for purchased equipment
         PRtSD(r,b,s)   = 0.0                                         !  peff
         PurchSD(r,b,s) = 0.0                                         !  peff

         !  Calculate equipment market shares of surviving service demand
         !  and replacement service demand, together with corresponding
         !  fuel shares, initialize variables for purchased efficiency calc:

         ReplacementProportion= 0.0
         DO TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
            IF (TechLife (t,v) .GT. 0.0) &
             ReplacementProportion= ReplacementProportion + &
              PrevYrTechShareofService (r,b,s,t,v) / TechLife (t,v)
           ENDIF
          END DO
         END DO

         DO f= 1, CMnumMajFl
          SurvivingFuelShareofService (f)= 0.0
          ReplacementFuelShareofService (f)= 0.0
         END DO

         DO TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
            SurvivingShareofService (t,v)= 0.0
            ReplacementShareofService (t,v)= 0.0

            IF (TechLife (t,v) .GT. 0.0 .AND. &
               (1.0 - ReplacementProportion) .GT. 0.0) THEN
             SurvivingShareofService (t,v)= &
              PrevYrTechShareofService (r,b,s,t,v) * &
              (1.0 - (1.0 / TechLife (t,v))) / &
              (1.0 - ReplacementProportion)
             DO f= 1, CMnumMajFl
              SurvivingFuelShareofService (f)= &
               SurvivingFuelShareofService (f) + &
               SurvivingShareofService (t,v) * FuelbyTech (t,f)
             END DO
            END IF

            IF (TechLife (t,v) .GT. 0.0 .AND. &
                ReplacementProportion .GT. 0.0) THEN
             ReplacementShareofService (t,v)= &
              PrevYrTechShareofService (r,b,s,t,v) * &
              (1.0 / TechLife (t,v)) &
             / ReplacementProportion
             DO f= 1, CMnumMajFl
              ReplacementFuelShareofService (f)= &
               ReplacementFuelShareofService (f) + &
               ReplacementShareofService (t,v) * FuelbyTech (t,f)
             END DO
            END IF
           ENDIF
          END DO
         END DO

         
         !  Compute Annualized Cost of New Equipment for each Time Preference

         ! Identify fuel used by this technology:
         !DO 90 p= 1, CMnumPref !Move down in loop for greater efficiency, no need to continually identify techs and fuels, etc
          DO 90 TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
           ! Identify fuel used by this technology:
           DO f= 1, CMnumMajFl
            IF (FuelbyTech (t,f) .EQ. 1) iFuel=f
           ENDDO          
           f=iFuel

          DO 90 v= 1, CMnumEqV (t)
            
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! 1 == this v belongs to tech t

            ! PITC
            ! Brief digression to limit years of advance for specific vintages from the menu 
            !  if the technologies are deemed to be "close in"
            !    Check for "close-in" vintages and trim advancement years if so
                 If(TechAvailability(r,t,v,iFYrAvail).le.iLastSTEOyr+50)ifwd=iforward(f)   !regionalize tech availability
                 If(TechAvailability(r,t,v,iFYrAvail).le.iLastSTEOyr+10)ifwd=max(-5,iforward(f))   !regionalize tech availability
                 If(TechAvailability(r,t,v,iFYrAvail).le.iLastSTEOyr+ 5)ifwd=max(-3,iforward(f))  !regionalize tech availability
                 If(TechAvailability(r,t,v,iFYrAvail).le.iLastSTEOyr   )ifwd=0                !regionalize tech availability
            ! End of PITC check to limit advancement of close in technologies.
 
            !For vintages that belong to this technology set the time preference slice and compute costs 
            !  Some vintages are not available and their selection is eliminated by setting costs = MAXCOST
            DO p= 1, CMnumPref  
            !Check for availability of this vintage and compute costs for those available
            IF ( (TechCost (r,t,v,iCapCost) .EQ. MAXCOST) .OR. &
                      (BASEYR-1+CURIYR .LT. TechAvailability (r,t,v,iFYrAvail)+ifwd).OR. &  !regionalize tech availability
                      (BASEYR-1+CURIYR .GT. TechAvailability (r,t,v,iLYrAvail))  .OR. &  !regionalize tech availability
                      (EquipRestriction (t,v,b,r) .EQ. 1)              .OR. &
                      (TechbyService (s,t) .EQ. 0) )          THEN

                 ! This vintage IS NOT available and set tech cost to
                 !  to "MAXCOST" so that it has no chance of being selected
                      AnnualCostTech (p,t,v,iExist)= MAXCOST
                      AnnualCostTech (p,t,v,iNew)= MAXCOST                     
                      AnnualCostTechAdj (p,t,v,iExist)= MAXCOST         
                      AnnualCostTechAdj (p,t,v,iNew)= MAXCOST             
                        ! Debugging code to print diagnositics for effects of shell on ACT
                        !    IF(PRTDBGK.eq.1 .and. CURITR.eq.MAXITR+1 .and. s.eq.1)   &  
                        !       write(rcdbg,*) t,v,p,BASEYR,CURIYR,AnnualCostTech(p,t,v,1), & 
                        !       AnnualCostTech(p,t,v),TechAvailability(r,t,v,1)
            ELSE
            !BEGINNING OF CALCULATIONS FOR AVAILABLE VINTAGES

            ! This vintage IS available therefore compute annualized tech cost
                        !   Debugging code to print PITC diagnositic/status variables
                        !      IF(PRTDBGK.eq.1 .and. CURITR.eq.MAXITR+1 .and. s.eq.1.and.b.eq.7   &
                        !      .and.r.eq.1.and.p.eq.1)write(rcdbg,*) 'TC1 ',t,v,f,                &
                        !             techavailability(r,t,v,1),                                  &
                        !             CURIYR,iforward(f),ifwd,ilastSTEOyr,EquipName(t,v)

                 !First step, compute the implicit discount rate based on 10-yr Treasury interest rate
                 ! plus the time preference premium
                 EffectHurdle = MC_RMGBLUSREAL(CURIYR)/100. + TimePrefPrem (s,p,CURIYR)

            IF (EffectHurdle.LT.0.03) THEN  !Set 3% floor for effective hurdle rate per FEMP title 10 CFR 436.14
                EffectHurdle = (0.03 - EffectHurdle) + EffectHurdle
            ENDIF

                    !Digression to adjust EffectHurdle based on fuel price inflation & hurdle rate elasticity
                    ! -- see khurela.txt values
                    EffectHurdleAdj=EffectHurdle    
                    IF (PriceDelta(f).GT.1.0) THEN  !First only do this if prices have increased
                       IF (EffectHurdle.GT.0.15) &  !Next, adjust "high" hurdle rates only 
                           EffectHurdleAdj = (EffectHurdle - 0.15)*PriceDelta(f)**HurdleElas(r,s,f) +0.15 !hurelast2
                    END IF  ! Check for rising prices and rate over 15 percent.
                 !Hurdle rate now calculated for this preference premium

                 !Compute annualized cost of the capital component of annualized cost of equipment
                 !   Note added an r dimension to ktek input file variables to fully regionalize techcost 
                 !First check to see if cost trends apply to this technology
                 IF (CostTrendSwitch .EQ. 1) THEN
                    !Invoke the cost trend function for the capital component of ACT
                    AnnualCostTech(p,t,v,iExist)= KEqCost(r,t,v,CURIYR+(BASEYR-1),"CAP")  
                    ELSE
                    !No cost trend, just use the capital cost from the input file
                    AnnualCostTech(p,t,v,iExist)= TechCost(r,t,v,iCapCost)
                 ENDIF

                 !Compute the annualized capital cost of the technology
                 !  Two separate costs are computed -- AnnualCostTech(...) and AnnualCostTechAdj(...):
                 !  AnnualCostTech uses the "standard" hurdle rate defined by the input file (kprem) and the 10-year Treasury rate.
                 !  AnnualCostTechAdj uses the "adjusted" hurdle rate which reduces the non-financial component (in excess of 15%)
                 !    when real energy costs increase.
                 !  AnnualCostTechAdj applies to "Same Technology" and "Same Fuel" decision types, mirroring the behavior of previous
                 !    technology choice code.  AnnualCostTech applies to the Stage 1 of the "All Fuel" decision type technology
                 !    choice.  In the Stage 2 of the "All Fuel" choice, AnnualCostTechAdj is used to potentially increase the 
                 !    efficiency of the choice, either by switching to a more efficient technology using the selected fuel, or by 
                 !    purchasing a more efficient version of the selected technology.  The use of two stages refines the previous
                 !    methodology and prevents switching into a fuel experiencing rising real prices that would otherwise not have
                 !    been selected.  A similar technique is applied to the retrofit equipment purchases later in the code.
                 AnnualCostTechAdj(p,t,v,iExist)= AnnualCostTech(p,t,v,iExist) / CapacityFactor (r,b,s) * EffectHurdleAdj  /   & 
                   ( 1. - (1. + EffectHurdleAdj )**( -1.*TechLife (t,v)) )                                              
                 AnnualCostTech(p,t,v,iExist)= AnnualCostTech(p,t,v,iExist) / CapacityFactor (r,b,s) * EffectHurdle  /   & 
                   ( 1. - (1. + EffectHurdle )**( -1.*TechLife (t,v)) )
                 
                 !Now add O&M costs of equipment per unit service demand
                 AnnualCostTechAdj (p,t,v,iExist)= AnnualCostTechAdj (p,t,v,iExist) + TechCost (r,t,v,iMaintCost)  
                 AnnualCostTech (p,t,v,iExist)= AnnualCostTech (p,t,v,iExist) + TechCost (r,t,v,iMaintCost)  

                 !Now add annualized fuel costs per unit service demand
                 !  Use price expectation variables calculated by the NEMS Integrating Module (Common Block "mxpblk") 
                 !  to determine avg fuel price over equip lifetime.
                 !  Solve only for the myopic foresight case if foresight control is "local"
                 IF (I4SCNT.EQ.1 .AND. LOOPOP.EQ.1) THEN             
                 ! Nonperfect myopic foresight specified, average expected prices over technology equip life
                   ExpectPrice= 0.0 ! initialize
                   DO y= 1, TechLife(t,v)
                    FuelPricetoUse: SELECT CASE (f)
                      CASE (1) FuelPricetoUse !Electricity
                        ExpectPrice= ExpectPrice + XPELCM(r,CURIYR+y-1)
                      CASE (2) FuelPricetoUse !Natural Gas
                        ExpectPrice= ExpectPrice + XPNGCM (r,CURIYR+y-1) 
                      CASE (3) FuelPricetoUse !Distillate Oil
                        ExpectPrice= ExpectPrice + XPDSCM (r,CURIYR+y-1)  
                     END SELECT FuelPricetoUse
                   ENDDO               
                   ExpectPrice= ExpectPrice/Techlife(t,v)
                 ELSE  
                 ! Local foresight, just use current year prices
                    LocalFuelPrice: SELECT CASE (f)
                      CASE (1) LocalFuelPrice !Electricity
                        ExpectPrice= PELCMOUT(r,CURIYR,s) !use electricity end use prices 
                      CASE (2) LocalFuelPrice !Natural Gas
                        ExpectPrice= PNGCM(r,CURIYR)
                      CASE (3) LocalFuelPrice !Distillate Oil
                        ExpectPrice= PDSCM (r,CURIYR)
                     END SELECT LocalFuelPrice
                 END IF ! Set fuel prices based on foresight control

                 ! Fuel prices from NEMS are in 1987 Dollars, therefore convert to same dollar year as ktek input file costs
                 ExpectPrice= ExpectPrice * MC_JPGDP(iKtechCostYr) / MC_JPGDP(-2)  !year 0 of the MC_JPGDP array is 1989

                 ! Special factor for lighting since service demand in in kilo-lumens (instead of kBtu)
                 !  also adjust for color rendering of technology vintage so recompute for each vintage here
                 IF (s .EQ. 6) ConversionFactor= (1.0/.03343)/TechCRI(r,s,t,v) 

                 ! Temporarily store the Capital and Maintenace Costs in the New Shell variable prior to adding operating costs
                 AnnualCostTech(p,t,v,iNew) =  AnnualCostTech(p,t,v,iExist)
                 AnnualCostTechAdj(p,t,v,iNew) =  AnnualCostTechAdj(p,t,v,iExist)  
                 !Fuel costs for existing buildings account for building shell for heating and cooling, for other end uses
                 !  the value of ShellEffFactor == 1 reflecting no shell effect
                 AnnualCostTechAdj(p,t,v,iExist) = AnnualCostTechAdj(p,t,v,iExist) + ExpectPrice / TechEff (r,s,t,v) * ConversionFactor &
                       * ShellEffFactor(iExist) 
                 AnnualCostTech(p,t,v,iExist) = AnnualCostTech(p,t,v,iExist) + ExpectPrice / TechEff (r,s,t,v) * ConversionFactor &
                       * ShellEffFactor(iExist)             
                 
                 IF (s .LT. 3) THEN                          
                  ! For heating and cooling technologies also computed fuel costs accounting for shell efficiency in new construction
                  AnnualCostTechAdj(p,t,v,iNew) = AnnualCostTechAdj(p,t,v,iNew) + ExpectPrice / TechEff (r,s,t,v) * ConversionFactor &
                       * ShellEffFactor(iNew)             
                  AnnualCostTech(p,t,v,iNew) = AnnualCostTech(p,t,v,iNew) + ExpectPrice / TechEff (r,s,t,v) * ConversionFactor &
                       * ShellEffFactor(iNew)             
                 ENDIF                                                  
                 
            !END OF CALCULATIONS FOR AVAILABLE VINTAGES
            END IF           ! cost by equipment availability              !Check for availability of this vintage
            ENDDO            ! Time preferences for this vintage
           ENDIF             ! TechbyModel(t,v).EQ.1                       !Vintage belongs to this technology
   90     CONTINUE           ! Annualized cost of New Equipment

!  Technology selection for the New and Replacement (NR) decisions
!  is very similar, apart from weighting by behavior proportions.
!  First, perform calculations the two types have in common:
!  Identify the least cost (LC) technologies and vintages (T) over
!  all fuels (AF) for each time preference:

          DO 100 p= 1, CMnumPref

           !Initialize Same Fuel Technology and Vintage Variables to 0 in Case No Techs of a Fuel Type Provide This Service
           DO f= 1, CMnumMajFl
            isFuelNew(f)=0
            MinCostShell_SF(f,iExist)=MAXCOST
            MinCostShell_SF(f,iNew)=MAXCOST
            LCTNR_SF (p,f,iLCTech,iExist)= 0  
            LCTNR_SF (p,f,iLCVint,iExist)= 0  
            IF (s .LT. 3) THEN  
              LCTNR_SF (p,f,iLCTech,iNew)= 0  
              LCTNR_SF (p,f,iLCVint,iNew)= 0  
            ENDIF 
           ENDDO

           ! Initialize All Fuel (AF) MinCost Searches to vintage 1 of first tech providing this service
           t= TechsforService (s,1) !technology number for first technology providing this service
           v=1        
           MinCostShell(iExist) = AnnualCostTech(p,t,v,iExist) 
           LCTNR_AF(p,iLCTech,iExist) = t            
           LCTNR_AF(p,iLCVint,iExist) = v            
            IF (s .LT. 3) THEN                      
             MinCostShell(iNew) = AnnualCostTech(p,t,v,iNew)     
             LCTNR_AF(p,iLCTech,iNew) = t              
             LCTNR_AF(p,iLCVint,iNew) = v              
            ENDIF                                           

          ! Check for lower-cost equipment in this time preference across appropriate technologies
          DO 99 TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
                     
           ! Initialize Same Tech (ST) MinCost Searches to vintage 1 of each tech providing this service
           v=1
           MinCostShell_ST(iExist) = AnnualCostTechAdj (p,t,v,iExist)
           LCVNR_ST (p,t,iExist) = v            
            IF (s .LT. 3) THEN                    
              MinCostShell_ST(iNew) = AnnualCostTechAdj (p,t,v,iNew)  
              LCVNR_ST (p,t,iNew) = v            
            ENDIF           

           ! Identify fuel used by this technology:
           DO f= 1, CMnumMajFl
            IF (FuelbyTech (t,f) .EQ. 1) iFuel=f
           ENDDO          
           f=iFuel
           
           ! Initialize Same Fuel (SF) MinCost Searches to vintage 1 of each tech providing this service
           ! NOTE: Only Initialize if this is the First Tech Found Using this Fuel
           IF(isFuelNew(f) .EQ. 0) THEN
             v=1
             isFuelNew=1
             MinCostShell_SF(f,iExist) = AnnualCostTechAdj (p,t,v,iExist) 
             LCTNR_SF (p,f,iLCTech,iExist)= t
             LCTNR_SF (p,f,iLCVint,iExist)= v
             IF (s .LT. 3) THEN
               MinCostShell_SF(f,iNew) = AnnualCostTechAdj (p,t,v,iNew)  
               LCTNR_SF (p,f,iLCTech,iNew)= t 
               LCTNR_SF (p,f,iLCVint,iNew)= v    
             ENDIF                          
           ENDIF
           
           DO 98 v= 1, CMnumEqV (t)           
            IF (TechbyModel(t,v) .EQ. 1) THEN   ! all v this t
             ! Existing Building Shells                               
             ! Search for Lower Cost Equipment for AF           
             IF ( AnnualCostTech (p,t,v,iExist) .LT. MinCostShell(iExist)) THEN 
                 MinCostShell(iExist) = AnnualCostTech (p,t,v,iExist)             
                 LCTNR_AF (p,iLCTech,iExist)= t                         
                 LCTNR_AF (p,iLCVint,iExist)= v                         
             ENDIF   
             ! Search for Lower Cost Equipment for ST
             IF ( AnnualCostTechAdj(p,t,v,iExist) .LT. MinCostShell_ST(iExist)) THEN  
                MinCostShell_ST(iExist) = AnnualCostTechAdj (p,t,v,iExist)            
                LCVNR_ST (p,t,iExist) = v 
             END IF 
             ! Search for Lower Cost Equipment for SF
             IF ( AnnualCostTechAdj (p,t,v,iExist) .LT. MinCostShell_SF(f,iExist)) THEN  
                   MinCostShell_SF(f,iExist) = AnnualCostTechAdj (p,t,v,iExist)          
                   LCTNR_SF (p,f,iLCTech,iExist)= t 
                   LCTNR_SF (p,f,iLCVint,iExist)= v 
             ENDIF                       

             ! New Building Shells    
             IF (s .LT. 3) THEN                                 
               ! Search for Lower Cost Equipment for Stage 1 of AF           
               IF ( AnnualCostTech (p,t,v,iNew) .LT. MinCostShell(iNew)) THEN  
                 MinCostShell(iNew) = AnnualCostTech (p,t,v,iNew)              
                 LCTNR_AF (p,iLCTech,iNew)= t                        
                 LCTNR_AF (p,iLCVint,iNew)= v                        
               ENDIF   
               ! Search for Lower Cost Equipment for ST
               IF ( AnnualCostTechAdj(p,t,v,iNew) .LT. MinCostShell_ST(iNew)) THEN   
                 MinCostShell_ST(iNew) = AnnualCostTechAdj (p,t,v,iNew)              
                 LCVNR_ST (p,t,iNew) = v                        
               END IF 
               IF ( AnnualCostTechAdj (p,t,v,iNew) .LT. MinCostShell_SF(f,iNew)) THEN  
                 MinCostShell_SF(f,iNew) = AnnualCostTechAdj (p,t,v,iNew)              
                 LCTNR_SF (p,f,iLCTech,iNew)= t 
                 LCTNR_SF (p,f,iLCVint,iNew)= v 
               ENDIF             
             ENDIF                        
             
            ENDIF   ! vintage belongs to this technology
   98     Continue  ! next vintage   
   
                ! Same Technology General Choices                
                ! Check if no vintages of this technology were available (i.e., mincost=maxcost)
                !   and set v == 0 if so
                IF (MinCostShell_ST(iExist) .EQ. MAXCOST) LCVNR_ST (p,t,iExist)= 0 
                IF (s .LT. 3) THEN 
                  IF (MinCostShell_ST(iNew) .EQ. MAXCOST) LCVNR_ST (p,t,iNew)= 0 
                ENDIF                                                       
                                
   99     Continue  ! next technology (and fuel implicitly)   

                ! Same Fuel General Choices 
                DO f= 1, CMnumMajFl

                  ! In case no technologies using this fuel are currently available:
                  IF (MinCostShell_SF(f,iExist) .EQ. MAXCOST) THEN
                     LCTNR_SF (p,f,iLCTech,iExist) = 0
                     LCTNR_SF (p,f,iLCVint,iExist) = 0
                  ENDIF   
                  IF (s .LT. 3) THEN      
                    IF (MinCostShell_SF(f,iNew) .EQ. MAXCOST) THEN
                      LCTNR_SF (p,f,iLCTech,iNew) = 0
                      LCTNR_SF (p,f,iLCVint,iNew) = 0
                    ENDIF    
                  ENDIF
                  
                ENDDO
                
           ! Stage 2 of the All Fuel retrofit choice potentially allows increased efficiency choice 
           !   using reduced hurdle rate -- either a more efficient technology or a more
           !   efficient vintage of the stage 1 technology selected.  
           t= LCTNR_AF(p,iLCTech,iExist) 
           ! Identify fuel used by this technology:
           DO f= 1, CMnumMajFl
            IF (FuelbyTech (t,f) .EQ. 1) iFuel=f
           ENDDO          
           iMinCostFuel_Exist=iFuel
           IF(s.lt.3) THEN
            t= LCTNR_AF(p,iLCTech,iNew) 
            ! Identify fuel used by this technology:
            DO f= 1, CMnumMajFl
             IF (FuelbyTech (t,f) .EQ. 1) iFuel=f
            ENDDO          
            iMinCostFuel_New=iFuel
           ENDIF 
           
           !Store Stage 1 choices
           t_AF_Stage1_Exist=LCTNR_AF(p,iLCTech,iExist)            
           v_AF_Stage1_Exist=LCTNR_AF(p,iLCVint,iExist)
           t_AF_Stage1_New=LCTNR_AF(p,iLCTech,iNew)            
           v_AF_Stage1_New=LCTNR_AF(p,iLCVint,iNew)
                
          ! Check for lower-cost equipment in this time preference across appropriate technologies
          DO 93  TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
           ! Consider only technologies for the All Fuel choice using iMinCostFuel
           IF (FuelbyTech (t,iMinCostFuel_Exist) .EQ. 1) THEN
            DO 91 v= 1, CMnumEqV (t)           
             IF (TechbyModel(t,v) .EQ. 1) THEN   ! all v this t
              ! Use Adjusted Hurdle Rate for the Stage 2 choice
              IF ( AnnualCostTechAdj (p,t,v,iExist) .LT. MinCostShell(iExist)) THEN 
                 MinCostShell(iExist) = AnnualCostTechAdj (p,t,v,iExist)             
                 LCTNR_AF (p,iLCTech,iExist)= t                         
                 LCTNR_AF (p,iLCVint,iExist)= v                         
              ENDIF ! found lower cost t  
             ENDIF ! all v this t
  91        CONTINUE !loop v
           ENDIF !Consider only technologies for the All Fuel choice using iMinCostFuel

           IF (s.lt.3) THEN
            IF (FuelbyTech (t,iMinCostFuel_New) .EQ. 1) THEN
             DO 92 v= 1, CMnumEqV (t)           
              IF (TechbyModel(t,v) .EQ. 1) THEN   ! all v this t
               ! Use Adjusted Hurdle Rate for the Stage 2 choice
               ! Search for Lower Cost Equipment for AF           
               IF ( AnnualCostTechAdj (p,t,v,iNew) .LT. MinCostShell(iNew)) THEN  
                 MinCostShell(iNew) = AnnualCostTechAdj (p,t,v,iNew)              
                 LCTNR_AF (p,iLCTech,iNew)= t                        
                 LCTNR_AF (p,iLCVint,iNew)= v                        
               ENDIF ! found lower cost t  
              ENDIF ! all v this t
   92        CONTINUE !loop v
            ENDIF !Consider only technologies for the All Fuel choice using iMinCostFuel
           ENDIF !s.lt.3
                           
   93     CONTINUE             
           
           ! Debugging code to print diagnositcs for effects of shell on ACT
!           IF(PRTDBGK.eq.1 .and. CURITR.eq.MAXITR+1) THEN
!             IF(LCTNR_AF (p,iLCTech,iExist).ne.t_AF_Stage1_Exist .or. LCTNR_AF (p,iLCVint,iExist).ne.v_AF_Stage1_Exist) &
!               write(rcdbg,*) "Stage2 AF_Exist Change Selected r:",r," s:",s," p:",p," b:",b," t2e:",  &
!                LCTNR_AF (p,iLCTech,iExist)," v2e:",LCTNR_AF (p,iLCVint,iExist)," t1e:",t_AF_Stage1_Exist," v1e:",v_AF_Stage1_Exist 
!             IF(s.lt.3 .and. (LCTNR_AF (p,iLCTech,iNew).ne.t_AF_Stage1_New .or. LCTNR_AF (p,iLCVint,iNew).ne.v_AF_Stage1_New)) &
!               write(rcdbg,*) "Stage2 AF_New Change Selected r:",r," s:",s," p:",p," b:",b," t2n:",    &
!                LCTNR_AF (p,iLCTech,iNew)," v2n:",LCTNR_AF (p,iLCVint,iNew)," t1n:",t_AF_Stage1_New," v1n:",v_AF_Stage1_New 
!           ENDIF !debugging prints

                                
  100     CONTINUE  ! next time preference        

!  For the Least Cost (LC) behavior segment, compute equipment market
!  shares (MS).  Here, market share is defined as the proportion of
!  service demand in a given segment (LC behaviour of NR decision)
!  that is satisfied by the given equipment (a particular t,v pair).
!  It is obtained by summing the proportions of consumers in each
!  time preference category that selected the given equipment.

        DO 130 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO 130 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this t
            LCMSN (t,v)= 0.0                                            
            LCMSR (t,v)= 0.0                                            
             DO p= 1, CMnumPref                                         
              IF ( (LCTNR_AF (p,1,1) .EQ. t) .AND. (LCTNR_AF (p,2,1) .EQ. v))    & 
               LCMSR (t,v)= LCMSR (t,v) + TimePrefProp (s,p,CURIYR)     
              IF (s.LT.3) THEN                                            
                IF ( (LCTNR_AF (p,1,2) .EQ. t) .AND. (LCTNR_AF (p,2,2) .EQ. v) ) & 
                 LCMSN (t,v)= LCMSN (t,v) + TimePrefProp (s,p,CURIYR)     
              ELSE
                IF ( (LCTNR_AF (p,1,1) .EQ. t) .AND. (LCTNR_AF (p,2,1) .EQ. v) )  & 
                 LCMSN (t,v)= LCMSN (t,v) + TimePrefProp (s,p,CURIYR)               
              END IF
             END DO    ! p                                              
            END IF
  130    CONTINUE

!  For the Same Fuel (SF) behavior segment, compute equipment market
!  shares by summing proportions over time preference selections:

         DO 140 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO 140 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this t
            SFMSN (t,v)= 0.0
            SFMSR (t,v)= 0.0
            DO f= 1, CMnumMajFl
             DO p= 1, CMnumPref
              IF (s .LT. 3) THEN
               IF ( LCTNR_SF (p,f,1,1) .EQ. t .AND. LCTNR_SF (p,f,2,1) .EQ. v )  & 
                      SFMSR (t,v)= SFMSR (t,v) + (TimePrefProp (s,p,CURIYR) *    & 
                      ReplacementFuelShareofService (f))                   
               IF ( LCTNR_SF (p,f,1,2) .EQ. t .AND. LCTNR_SF (p,f,2,2) .EQ. v )  & 
                      SFMSN (t,v)= SFMSN (t,v) + (TimePrefProp (s,p,CURIYR) *    & 
                      PrevYrFuelShareofService (r,b,s,f))                  
              ELSE                                                         
               IF ( LCTNR_SF (p,f,1,1) .EQ. t .AND. LCTNR_SF (p,f,2,1) .EQ. v ) THEN
                SFMSN (t,v)= SFMSN (t,v) + (TimePrefProp (s,p,CURIYR) * &
                    PrevYrFuelShareofService (r,b,s,f))
                SFMSR (t,v)= SFMSR (t,v) + (TimePrefProp (s,p,CURIYR) * &
                    ReplacementFuelShareofService (f))
               END IF
              END IF                                                  

             END DO     ! p
            END DO      ! f
           END IF
  140    CONTINUE

!  For the Same Technology (ST) behavior segment, compute equipment
!  market shares:

         DO 150 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          TechnologyShareN= 0.0
          TechnologyShareR= 0.0
          DO 142 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
            TechnologyShareN= TechnologyShareN + PrevYrTechShareofService (r,b,s,t,v)
            TechnologyShareR= TechnologyShareR + ReplacementShareofService (t,v)
           ENDIF
  142     CONTINUE
          DO 150 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this t
            STMSN (t,v)= 0.0
            STMSR (t,v)= 0.0
            DO 145 p= 1, CMnumPref
             IF (s.LT.3) THEN                                           
              IF ( LCVNR_ST (p,t,1) .EQ. v ) & 
                STMSR (t,v)= STMSR (t,v) + TimePrefProp (s,p,CURIYR)                            
              IF ( LCVNR_ST (p,t,2) .EQ. v ) & 
                STMSN (t,v)= STMSN (t,v) + TimePrefProp (s,p,CURIYR)                            
             ELSE                                                       
              IF ( LCVNR_ST (p,t,1) .EQ. v ) THEN
               STMSN (t,v)= STMSN (t,v)  + TimePrefProp (s,p,CURIYR)
               STMSR (t,v)= STMSR (t,v)  + TimePrefProp (s,p,CURIYR)
              END IF
             END IF                                                   
  145       CONTINUE
            STMSN (t,v)= STMSN (t,v) * TechnologyShareN
            STMSR (t,v)= STMSR (t,v) * TechnologyShareR
           ENDIF
  150    CONTINUE

!  From the results above, compute composite market shares for the
!  New decision type by consolidating across all behavioral rules:

         DO 160 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO 160 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this t
            MS (b,s,1,t,v)= &
              BehaviorShare (s,b,1,1) * LCMSN (t,v) & 
            + BehaviorShare (s,b,1,2) * SFMSN (t,v) &
            + BehaviorShare (s,b,1,3) * STMSN (t,v)
           ENDIF
  160    CONTINUE

!  Compute composite market shares for the Replacement decision type
!  by consolidating across all behavioral rules:

         DO 170 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO 170 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this t
            MS (b,s,2,t,v)= &
               BehaviorShare (s,b,2,1) * LCMSR (t,v) & 
             + BehaviorShare (s,b,2,2) * SFMSR (t,v) &
             + BehaviorShare (s,b,2,3) * STMSR (t,v)
           ENDIF
  170    CONTINUE

!  Computations for the Retrofit (Ret) decision type involve
!  additional considerations relative to those for the New and
!  Replacement decisions, and must be handled separately:

!  Compute the Annualized cost of Existing Technology, ACE :

        DO 180 p= 1, CMnumPref
         DO 180 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO 180 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech

           ! If equipment is not yet available, assign the maximum
           ! (prohibitive) cost.  Note that the equipment may be no
           ! longer available for purchase, yet still be part of the
           ! existing stock.
            IF ( (BASEYR-1+CURIYR.LT.TechAvailability(r,t,v,1)+ifwd) .OR.  &  !9-16-99 PITC  regionalize tech availability
                (TechCost (r,t,v,iCapCost) .EQ. MAXCOST) ) THEN 
                  ACE (t,v,p)= MAXCOST
            ELSE

             ConversionFactor= 8.76  ! general case for service demand conversion, all end uses other than lighting
             
             ! Identify fuel used by this technology:
             DO f= 1, CMnumMajFl
              IF (FuelbyTech (t,f) .EQ. 1) iFuel=f
             ENDDO          
             f=iFuel

                 !First step, compute the implicit discount rate based on 10-yr Treasury interest rate
                 ! plus the time preference premium
                 EffectHurdle = MC_RMGBLUSREAL(CURIYR)/100. + TimePrefPrem (s,p,CURIYR)                  
                    ! del
                      IF (PriceDelta(f).GT.1.0) THEN  !First only do this if prices have increased
                       IF (EffectHurdle.GT.0.15) &  !Next, adjust "high" hurdle rates only 
                           EffectHurdle = (EffectHurdle - 0.15)*PriceDelta(f)**HurdleElas(r,s,f) +0.15
                    END IF  ! Check for rising prices and rate over 15 percent.
                 !Hurdle rate now calculated for this preference premium

             ! Consider O&M costs of equipment per unit service demand:
             ACE (t,v,p)= TechCost (r,t,v,iMaintCost)
             ! Consider the cost of removing this equipment for retrofitting.  Conceptually, this cost would be
             ! added to the cost of equipment considered for purchase when making the retrofit decision;
             ! however, the equivalent least-cost decision is made with greater programming economy by
             ! subtracting the retrofit cost from the cost of retaining the existing equipment.  The only problem
             ! with this approach is that the retrofit cost is then annualized over the life of the existing equipment
             ! rather than the life of the equipment considered for purchase.  The advantage of this approach is that the
             ! number of cost tables that must be built is reduced by a factor equal to the square of the number of
             ! different equipment choices available.

             ACE (t,v,p)= ACE (t,v,p) - RetroCostFract (t,v) * TechCost (r,t,v,iCapCost) / CapacityFactor (r,b,s) * & 
                       (EffectHurdle ) / ( 1. - (1. + EffectHurdle )**( -1.*TechLife (t,v)) )

                 !Now add annualized fuel costs per unit service demand
                 !  Use price expectation variables calculated by the NEMS Integrating Module (Common Block "mxpblk") 
                 !  to determine avg fuel price over equip lifetime.
                 !  Solve only for the myopic foresight case if foresight control is "local"
                 IF (I4SCNT.EQ.1 .AND. LOOPOP.EQ.1) THEN             
                 ! Nonperfect myopic foresight specified, average expected prices over technology equip life
                   ExpectPrice= 0.0 ! initialize
                   DO y= 1, TechLife(t,v)/2
                    FuelPricetoUse2: SELECT CASE (f)
                      CASE (1) FuelPricetoUse2 !Electricity
                        ExpectPrice= ExpectPrice + XPELCM(r,CURIYR+y-1)
                      CASE (2) FuelPricetoUse2 !Natural Gas
                        ExpectPrice= ExpectPrice + XPNGCM (r,CURIYR+y-1) 
                      CASE (3) FuelPricetoUse2 !Distillate Oil
                        ExpectPrice= ExpectPrice + XPDSCM (r,CURIYR+y-1)  
                     END SELECT FuelPricetoUse2
                   ENDDO               
                   ExpectPrice= ExpectPrice/(Techlife(t,v)/2)
                 ELSE  
                 ! Local foresight, just use current year prices
                    LocalFuelPrice2: SELECT CASE (f)
                      CASE (1) LocalFuelPrice2 !Electricity
                        ExpectPrice= PELCMOUT(r,CURIYR,s) !use electricity end use prices 
                      CASE (2) LocalFuelPrice2 !Natural Gas
                        ExpectPrice= PNGCM(r,CURIYR)
                      CASE (3) LocalFuelPrice2 !Distillate Oil
                        ExpectPrice= PDSCM (r,CURIYR)
                     END SELECT LocalFuelPrice2
                 END IF ! Set fuel prices based on foresight control

                 ! Fuel prices from NEMS are in 1987 Dollars, therefore convert to same dollar year as ktek input file costs
                 ExpectPrice= ExpectPrice * MC_JPGDP(iKtechCostYr) / MC_JPGDP(-2)  !year 0 of the MC_JPGDP array is 1989

                 ! Special factor for lighting since service demand in in kilo-lumens (instead of kBtu)
                 !  also adjust for color rendering of technology vintage so recompute for each vintage here
                 IF (s .EQ. 6) ConversionFactor= (1.0/.03343)/TechCRI(r,s,t,v) 
                
                 ACE (t,v,p)= ACE (t,v,p) + ExpectPrice / TechEff (r,s,t,v) * ConversionFactor * ShellEffFactor(iExist)

             END IF           ! cost by equipment availability
           ENDIF              ! TechbyModel(t,v).EQ.1 test
  180    CONTINUE             ! Annualized cost of Existing Equipment

!  Identify the least cost equipment over all fuels for each time
!  preference, by comparing the Annualized Cost of Existing Equipment
!  against the Annualized Cost of New Equipment.  In addition to
!  varying with time preference, the least cost equipment choice
!  varies with original equipment, which serves as a standard for
!  comparison.

         DO 190 p= 1, CMnumPref
          DO 190 TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS) ! existing technology
           DO 190 v= 1, CMnumEqV (t)     ! existing vintage
            IF (TechbyModel(t,v) .EQ. 1) THEN               ! all v this t

             ! Initialize Same Fuel choice to this tech and vint
             ! If t,v represents unavailable equipment, do not
             ! choose replacement equipment:
             IF ( ACE (t,v,p) .EQ. MAXCOST ) THEN           ! retrofitting only existing equipment
              LCTRetSF (p,t,v,iLCTech)= 0
              LCTRetSF (p,t,v,iLCVint)= 0
              LCTRetAF (p,t,v,iLCTech)= 0
              LCTRetAF (p,t,v,iLCVint)= 0
              LCVRetST (p,t,v)= 0
             
             ELSE        ! this equipment exists; consider retrofit

              ! Identify fuel used by this technology:
              DO f= 1, CMnumMajFl
               IF (FuelbyTech (t,f) .EQ. 1) iFuel=f
              ENDDO          
              f=iFuel

              ! Use existing technology, vintage as initial standard
              MinCost_SF= ACE (t,v,p)
              LCTRetSF (p,t,v,iLCTech)= t
              LCTRetSF (p,t,v,iLCVint)= v
              
              MinCost_AF= ACE (t,v,p)
              LCTRetAF (p,t,v,iLCTech)= t
              LCTRetAF (p,t,v,iLCVint)= v

              MinCost_ST= ACE (t,v,p)
              LCVRetST (p,t,v)= v

              ! Check for lower-cost equipment in this time preference

              ! Same Technology Retrofitting Behavior
                 ! If the user has set STRetBehav to 1 in KPARM, then the
                 ! same tech behavior for retrofit will consider all vintages;
                 ! otherwise, that behavior is to take no action.
                 IF (STRetBehav .EQ. 1) THEN ! only do the work if needed
                  DO vint= 1, CMnumEqV (t)
                   IF (TechbyModel(t,vint).EQ.1) THEN
                    IF ( AnnualCostTechAdj(p,t,vint,iExist) .LT. MinCost_ST ) THEN  
                     MinCost_ST = AnnualCostTechAdj(p,t,vint,iExist)                
                     LCVRetST (p,t,v)= vint
                    END IF           
                   ENDIF             ! TechbyModel(t,vint)==1 test
                  ENDDO
                 ENDIF       ! STRetBehav==1 test

              DO 189 TechsubS= 1, CMnumTechsforService (s)
               tech= TechsforService (s,TechsubS)
               DO 189 vint= 1, CMnumEqV (tech)
                IF (TechbyModel(tech,vint) .EQ. 1) THEN
                 IF ( (AnnualCostTechAdj(p,tech,vint,iExist) .LT. MinCost_SF ) .AND. &  
                               (FuelbyTech (tech,f) .EQ. 1) )      THEN
                  MinCost_SF = AnnualCostTechAdj(p,tech,vint,iExist)                    
                  LCTRetSF (p,t,v,iLCTech)= tech
                  LCTRetSF (p,t,v,iLCVint)= vint
                 ENDIF

                 IF ( AnnualCostTech(p,tech,vint,iExist) .LT. MinCost_AF ) THEN
                  MinCost_AF= AnnualCostTech(p,tech,vint,iExist)                    
                  !note retrofitting using unadjusted hurdle rates in "stage 1"
                  LCTRetAF (p,t,v,iLCTech)= tech
                  LCTRetAF (p,t,v,iLCVint)= vint
                 ENDIF                                 
                END IF            
  189         CONTINUE

             END IF    ! retrofitting only existing equipment
            END IF    ! all v this t                        
             
  190     Continue

          ! Stage 2 of the All Fuel retrofit choice potentially allows increased efficiency choice 
          !   using reduced hurdle rate -- either a more efficient technology or a more
          !   efficient vintage of the stage 1 technology selected.  
         DO 209 p= 1, CMnumPref
          DO 200  TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
            DO 205 v= 1, CMnumEqV (t)           
             IF ( ACE (t,v,p) .lt. MAXCOST ) THEN           ! only consider existing equipment
              IF (TechbyModel(t,v) .EQ. 1) THEN   ! all v this t

               ! Identify fuel used by this technology:
                DO f= 1, CMnumMajFl
                 IF (FuelbyTech (LCTRetAF(p,t,v,iLCTech),f) .EQ. 1) iFuel=f
                ENDDO          
                iMinCostFuel_Exist=iFuel
                
                 !Store Stage 1 retrofit choices
                 t_AF_Stage1_Exist=LCTRetAF(p,t,v,iLCTech)            
                 v_AF_Stage1_Exist=LCTREtAF(p,t,v,iLCVint)                      
             
                !Stage 2 search for lower cost equipment using adjusted hurdle rates                 
                DO 202 TechsubS= 1, CMnumTechsforService (s)
                 tech= TechsforService (s,TechsubS)
                 IF (FuelbyTech (tech,iMinCostFuel_Exist) .EQ. 1) THEN  !Consider only technologies for the Stage 2 All Fuel choice using iMinCostFuel
  
                    DO 201 vint= 1, CMnumEqV (tech)

                     IF (TechbyModel(tech,vint) .EQ. 1) THEN  !all vint this tech                
                      IF ( AnnualCostTechAdj (p,tech,vint,iExist) .LT. MinCost_AF) THEN 
                       MinCost_AF = AnnualCostTechAdj (p,tech,vint,iExist)             
                       LCTRetAF (p,t,v,iLCTech)= tech                         
                       LCTRetAF (p,t,v,iLCVint)= vint                         
                      ENDIF ! found lower cost tech  
                     ENDIF !all vint this tech                       

 201                CONTINUE              

                 ENDIF !Consider only technologies for the Stage 2 All Fuel choice using iMinCostFuel
 202            CONTINUE              

                ! Debugging code to print diagnositcs for effects of shell on ACT
                  IF(PRTDBGK.eq.1 .and. CURITR.eq.MAXITR+1) THEN
                   IF(LCTRetAF (p,t,v,iLCTech).ne.t_AF_Stage1_Exist .or. LCTRetAF (p,t,v,iLCVint).ne.v_AF_Stage1_Exist) &
                    write(rcdbg,*) "Stage2 AF_Exist Retrofit Change r:",r," s:",s," p:",p," b:",b," t2e:",  &
                    LCTRetAF (p,t,v,iLCTech)," v2e:",LCTRetAF (p,t,v,iLCVint)," t1e:",t_AF_Stage1_Exist," v1e:",v_AF_Stage1_Exist 
                  ENDIF !Debug print

              ENDIF ! all v this t
             ENDIF ! only consider existing equipment
             
  205        CONTINUE !loop v
  
                           
  200     CONTINUE             
  209     Continue


!  Compute equipment market shares for the LC behavioral segment.
!  Here, market share is defined as the proportion of service demand
!  in a given segment (LC behaviour of Retrofit decision) that is
!  satisfied by the given equipment (a particular t,v pair).
!  It is obtained by summing weighted proportions of consumers in
!  each time preference category that selected the given equipment.
!  The weights are given by the market shares of the equipment
!  originally used by the consumers who choose the given equipment.

         DO 210 TsubS= 1, CMnumTechsforService (s) ! For each tech,
          t= TechsforService (s,TsubS)
          DO 210 v= 1, CMnumEqV (t)      ! For each vintage,
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
             LCMSRet (t,v)= 0.0           ! Initialize market share.
              DO TechsubS= 1, CMnumTechsforService (s)
               tech= TechsforService (s,TechsubS)! Check orig techs,
               DO vint= 1, CMnumEqV (tech)   !  and vintages,
                IF (TechbyModel(tech,vint) .EQ. 1) THEN
                 DO p= 1, CMnumPref       !  and time preferences.
                 ! If consumers switched from original equipment
                 ! to the equipment whose market share is being
                 ! calculated, compute the contribution to the
                 ! calculated market share.  The term 'switched'
                 ! includes the case where the consumers happen to
                 ! choose the original equipment.
                   IF ( (LCTRetAF (p,tech,vint,iLCTech) .EQ. t )   .AND. &
                        (LCTRetAF (p,tech,vint,iLCVint) .EQ. v ) )       &
                    LCMSRet (t,v)= LCMSRet (t,v) + &
                    (SurvivingShareofService (tech,vint) * TimePrefProp (s,p,CURIYR))                  
                 END DO  ! p
                END IF   ! TechbyModel(tech,vint).EQ.1 test
               END DO    ! vint
              END DO     ! TechsubS
           END IF        ! TechbyModel(t,v).EQ.1 test
  210    CONTINUE

!  Compute equipment market shares for the SF behavioral segment:
!  Here, market share is defined as the proportion of service demand
!  in a given segment (SF behaviour of Retrofit decision) that is
!  satisfied by the given equipment (a particular t,v pair).
!  It is obtained by summing weighted proportions of consumers in
!  each time preference category that selected the given equipment.
!  The weights are given by the market shares of the equipment
!  originally used by the consumers who choose the given equipment.

         DO 220 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS) ! For each technology, and
          DO 220 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
            SFMSRet (t,v)= 0.0          ! Initialize market share.
             DO TechsubS= 1, CMnumTechsforService (s)
              tech= TechsforService (s,TechsubS) ! Check orig techs,
              DO vint= 1, CMnumEqV (tech)   !  and vintages
               IF (TechbyModel(tech,vint) .EQ. 1) THEN
                DO p= 1, CMnumPref       !  and time preferences.
                 ! If consumers switched from original equipment
                 ! to the equipment whose market share is being
                 ! calculated, compute the contribution to the
                 ! calculated market share.  The term 'switched'
                 ! includes the case where the consumers happen to
                 ! choose the original equipment.  The equipment
                 ! whose market share is being calculated can only
                 ! have been chosen if the original equipment used
                 ! the same fuel,so this additional test is not made.
                  IF ( (LCTRetSF (p,tech,vint,iLCTech) .EQ. t )   .AND. &
                       (LCTRetSF (p,tech,vint,iLCVint) .EQ. v ) ) &
                   SFMSRet (t,v)= SFMSRet (t,v) + &
                   (SurvivingShareofService (tech,vint) * TimePrefProp (s,p,CURIYR))
                ENDDO   ! p
               ENDIF    ! TechbyModel(t,v).EQ.1 test
              ENDDO     ! vint
            ENDDO       ! TechsubS
          ENDIF         ! TechbyModel(t,v).EQ.1 test
  220    CONTINUE



!  Compute equipment market shares for the ST behavioral segment:
!  Method used depends on setting of STRetBehav switch in KPARM
!  Here, market share is defined as the proportion of service demand
!  in a given segment (ST behaviour of Retrofit decision) that is
!  satisfied by the given equipment (a particular t,v pair).
!  It is obtained by summing weighted proportions of consumers in
!  each time preference category that selected the given equipment.
!  The weights are given by the market shares of the equipment
!  originally used by the consumers who choose the given equipment.

        IF (STRetBehav .EQ. 1) THEN ! purchase of better vintage possible

         DO 420 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS) ! For each technology, and
          DO 420 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v).EQ.1) THEN     ! all v this tech
            STMSRet (t,v)= 0.0          ! Initialize market share.
              DO vint= 1, CMnumEqV (t)   !  Check original vintages
               IF (TechbyModel(t,vint).EQ.1) THEN
                DO p= 1, CMnumPref       !  and time preferences.
                 ! If consumers switched from original equipment
                 ! to the equipment whose market share is being
                 ! calculated, compute the contribution to the
                 ! calculated market share.  The term 'switched'
                 ! includes the case where the consumers happen to
                 ! choose the original equipment.
                  IF ( LCVRetST (p,t,vint).EQ.v ) &
                   STMSRet (t,v)= STMSRet (t,v) + &
                   (SurvivingShareofService (t,vint) * TimePrefProp (s,p,CURIYR))
                ENDDO   ! p
               ENDIF    ! TechbyModel(t,vint)==1 test
              ENDDO     ! vint
          ENDIF         ! TechbyModel(t,v)==1 test
  420    CONTINUE

        ELSE ! Same Tech behavior interpreted as retrofit inaction

         DO 230 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO 230 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN ! all v this tech
            STMSRet (t,v)= SurvivingShareofService (t,v)
           ENDIF
  230    CONTINUE

        ENDIF ! STRetBehav split

!  Compute composite market shares for the Retrofit decision type by
!  consolidating across all behavioral rules:

         DO 240 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO 240 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
            MS (b,s,3,t,v)= &
              BehaviorShare (s,b,3,1) * LCMSRet (t,v) &
            + BehaviorShare (s,b,3,2) * SFMSRet (t,v) &
            + BehaviorShare (s,b,3,3) * STMSRet (t,v)
           ENDIF
  240    CONTINUE


         y= CURIYR      ! for more concise notation
         ! Substitute new nomenclature for that existing in other
         ! subroutines, until permanent changes are made.
         NSD (r,b,s,y)= NewServDmd (s,b,r,y)
         RSD (r,b,s,y)= RetireServDmd (s,b,r,y)
         SSD (r,b,s,y)= ServDmdSurv (s,b,r,y)

      IF (s.EQ.2) THEN ! Heat pumps receive special processing:

!  The heat pump purchase decisions for heating and cooling are
!  partially integrated.  This is done by comparing the cost of
!  purchasing a particular heat pump for heating and cooling against
!  the purchase of a specified cooling technology combined with
!  each of the other heating equipment choices.  Of course, heat pumps
!  are also compared with each other.  Briefly, this approach has
!  been implemented by subtracting the cost of a user-specified cooling
!  equip from the heat pump capital cost in ktech.  What remains is the
!  heat pump's incremental capital cost to provide heating.  This is
!  compared with other heating equipment and purchase decisions are
!  made.  Cooling equipment purchase decisions are made without regard
!  to heat pumps, and market shares are calculated which require
!  adjustment to account for the fact that some cooling is provided
!  by the heat pumps purchased during the heating technology choice
!  cycle.  The adjustment is performed by assuming that the ratio of
!  the amount of cooling service demand to heating service demand
!  satisfied by the heat pump is given by the ratio of the number of
!  cooling degree days to heating degree days in the year and Census
!  division under consideration.  Beyond the historical and short-
!  term forecast horizons, average degree days are used.  When
!  combined with the cooling service demand forecast, this permits
!  the market shares of the heat pumps for cooling to be computed.
!  The total amount of cooling satisfied by all heat pumps is used to
!  calculate the renormalization of remaining cooling equipment market
!  shares.

!  To simplify notation below, load heating and cooling service
!  demands for the current year, Census Division, and building into
!  a work array:
      DO svc= 1,2 ! for heating and cooling only
       SD (svc,1)= NSD (r,b,svc,CURIYR)
       SD (svc,2)= RSD (r,b,svc,CURIYR)
       SD (svc,3)= SSD (r,b,svc,CURIYR)
      END DO  ! svc

!  For years within the STEO horizon, heating and cooling degree
!  days are available; beyond that horizon normal values are used with
!  projected population shifts:
      WthrYear= CURIYR

!  At this point, heat pumps have been selected in the heating
!  decision portion of TechChoice;  it remains to calculate their
!  implied shares of cooling, and adjust shares of remaining cooling
!  equipment:
      DO t= 1, CMnumHeatPumpTechs ! reserved htg heat pump indices
       IF (TechbyService(1,t) .EQ. 1) THEN ! for defined heat pumps
        DO v= 1, CMnumEqV (t)
         IF (TechbyModel(t,v) .EQ. 1) THEN ! for defined models
          DO d= 1, CMDecision ! new, replacement, & retrofit
           IF (SD(2,d) .GT. 0.0) THEN
            MS (b,2,d,t+CMnumHeatPumpTechs,v)=  & ! cooling market share
              MS (b,1,d,t,v) * SD (1,d)  & ! amount of heating svc dmd
                    !* ratio of cooling to heating degree days= amt of cooling
            * DegreeDays (2,r,WthrYear) / DegreeDays (1,r,WthrYear) &
            / SD (2,d)  ! / cooling svc dmd
           ELSE
            MS (b,2,d,t+CMnumHeatPumpTechs,v)= 0.0
           END IF ! SD=0 split
          END DO ! d
         END IF  ! defined models
        END DO   ! v
       END IF    ! defined heat pumps
      END DO     ! t

      ! Amount of cooling service demand satisfied by heat pumps:
      DO d= 1, CMDecision
       HeatPumpCoolingSD (d)= 0.0
       DO t= 1, CMnumHeatPumpTechs ! reserved htg heat pump indices
        IF (TechbyService(1,t) .EQ. 1) THEN ! for defined heat pumps
         DO v= 1, CMnumEqV (t)
          IF (TechbyModel(t,v) .EQ. 1) THEN ! for defined models
           HeatPumpCoolingSD (d)= HeatPumpCoolingSD (d) + &
            MS (b,2,d,t+CMnumHeatPumpTechs,v) * SD (2,d)
          END IF  ! defined models
         END DO   ! v
        END IF    ! defined heat pumps
       END DO     ! t
      END DO      ! d

      ! Sum non-heatpump cooling equipment market shares
      ! by decision type for subsequent renormalization
      DO d= 1, CMDecision
       Normalizer (d)= 0.0
       DO TsubS= 1, CMnumTechsforService (2)
        t= TechsforService (2,TsubS)
        IF (t .GT. 2*CMnumHeatPumpTechs) THEN ! for other than heat pumps
         DO v= 1, CMnumEqV (t)
          IF (TechbyModel(t,v) .EQ. 1) THEN ! for defined models
           Normalizer (d)= Normalizer (d) + MS (b,2,d,t,v)
          END IF  ! defined models
         END DO   ! v
        END IF    ! non heat pump cooling equipment
       END DO     ! TsubS
      END DO      ! d


      ! Renormalize cooling equipment market shares:
      DO TsubS= 1, CMnumTechsforService (2)
       t= TechsforService (2,TsubS)
       IF (t .GT. 2*CMnumHeatPumpTechs) THEN ! for other than heat pumps
        DO v= 1, CMnumEqV (t)
         IF (TechbyModel(t,v) .EQ. 1) THEN ! for defined models
          DO d= 1, CMDecision ! new, replacement, & retrofit
           IF (Normalizer(d).GT.0.0) &
            MS (b,2,d,t,v) = MS (b,2,d,t,v) / Normalizer (d)
           IF (SD(2,d) .GT. 0.0) &
            MS (b,2,d,t,v) = MS (b,2,d,t,v) * (SD(2,d) - HeatPumpCoolingSD(d)) / SD(2,d)  ! cooling market share
          END DO ! d
         END IF  ! defined models
        END DO   ! v
       END IF    ! non heat pump cooling equipment
      END DO     ! TsubS

      END IF ! Special processing of heat pumps for cooling


      ! Update equipment investment cost total for this r,b,s,y:
      DO TsubS= 1, CMnumTechsforService (s)
       t= TechsforService (s,TsubS)
       DO v= 1, CMnumEqV (t)                                           ! invest

!       Initialize variables for this t,v
          InvestRetroDmd(t,v) = 0.0                                    ! invest
          InvestDmd = 0.0                                              ! invest
          TechInvCost = 0.0                                            ! invest
          CostperKBtu = 0.0                                            ! invest
          SubsidyperKBtu = 0.0                                         ! invest  !investsub
          Subsidy111dperKBtu = 0.0                                     ! invest  !investsub 111(d)

         IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech       ! invest

           ! Identify fuel used by this technology:
           DO f= 1, CMnumMajFl
            IF (FuelbyTech (t,f) .EQ. 1) iFuel=f
           ENDDO          
           f=iFuel

!    Check for "close-in" technologies and trim advancement years if so   ! PITC
        If(TechAvailability(r,t,v,1).le.iLastSTEOyr+50)ifwd=iforward(f)   ! PITC  !regionalize tech availability
        If(TechAvailability(r,t,v,1).le.iLastSTEOyr+10)                 & ! PITC  !regionalize tech availability
                                            ifwd=max(-5,iforward(f))      ! PITC
        If(TechAvailability(r,t,v,1).le.iLastSTEOyr+ 5)                 & ! PITC  !regionalize tech availability
                                            ifwd=max(-3,iforward(f))      ! PITC
        If(TechAvailability(r,t,v,1).le.iLastSTEOyr)ifwd=0                ! PITC  !regionalize tech availability

        IF ( (TechCost (r,t,v,iCapCost) .EQ. MAXCOST) .OR. &                      !regionalize techcost
            (BASEYR-1+CURIYR .LT. TechAvailability (r,t,v,1) &                    !regionalize tech availability
                            + ifwd)                       .OR.          & ! PITC
                  (BASEYR-1+CURIYR .GT. TechAvailability (r,t,v,2)) .OR. &        !regionalize tech availability
                  (EquipRestriction (t,v,b,r) .EQ. 1)             .OR. &
                  (TechbyService (s,t) .EQ. 0) )                   THEN
                  ! Unavailable or does not provide this service
                  GO TO 314

              ELSE

!  Uncomment these lines to print diagnostic/status info
!      IF(PRTDBGK.eq.1 .and. CURITR.eq.MAXITR+1 .and. s.eq.1.and.b.eq.7   & ! PITC
!      .and.r.eq.1)write(rcdbg,*) 'TC2 ',t,v,f,techavailability(r,t,v,1),   & ! PITC
!      CURIYR,iforward(f),ifwd,EquipName(t,v)                               ! PITC

      !  Compute retrofit demand requiring investment

          InvestRetroDmd(t,v)= SSD(r,b,s,y) *                        & ! invest
                    ( MS(b,s,3,t,v) - SurvivingShareofService(t,v))    ! invest
          IF (InvestRetroDmd(t,v) .LT. 0.0) InvestRetroDmd(t,v)= 0.0   ! invest

      !  Compute total demand requiring investment for this technology
      !  and vintage within this division, service and building type.

          InvestDmd= MS (b,s,1,t,v) * NSD (r,b,s,y) +                & ! invest
                MS(b,s,2,t,v) * RSD(r,b,s,y) + InvestRetroDmd(t,v)     ! invest

          IF (CostTrendSwitch .EQ. 1) THEN                             ! invest

      !  Restore full cost for heatpumps, use techcost as is for
      !  all other technologies

           IF (t .LE. CMnumHeatPumpTechs) THEN                         ! invest
             IF (s .EQ. 2) GO TO 314     !Avoids double-counting         invest
       HPCLCOST=KEqCost(r,CoolingTechIndexHP,v,CURIYR+(BASEYR-1),"CAP")! invest regionalize techcost
             IF (HPCLCOST .EQ. MAXCOST) &
               HPCLCOST=TechCost(r,CoolingTechIndexHP,v,iCapCost)      ! invest regionalize techcost

             TechInvCost= KEqCost(r,t,v,CURIYR+(BASEYR-1),"CAP") +   & ! invest regionalize techcost
              HPCLCOST                                                 ! invest

           ELSE

           !  All technologies other than heatpumps
             TechInvCost= KEqCost(r,t,v,CURIYR+(BASEYR-1),"CAP")       ! invest regionalize techcost
           ENDIF     ! Check for heatpumps                               invest

         ELSE

      !  Restore full cost for heatpumps, use techcost as is for
      !  all other technologies

           IF (t .LE. CMnumHeatPumpTechs) THEN                         ! invest
             IF (s .EQ. 2) GO TO 314     !Avoids double-counting         invest
             TechInvCost= TechCost(r,t,v,iCapCost) +                 & ! invest regionalize techcost
              TechCost(r,CoolingTechIndexHP,v,iCapCost)                ! invest regionalize techcost

           ELSE

           !  All technologies other than heatpumps
             TechInvCost= TechCost(r,t,v,iCapCost)                     ! invest regionalize techcost
           ENDIF     ! Check for heatpumps                               invest
          ENDIF     !Check for cost trend switch                         invest

      !  Increment investment cost for this region, service and year.
      !  Divide TechInvCost by 8760 to change $/kBtu/hr to $/kBtu/yr,
      !  multiply by techlife (assume entire capital cost is paid in
      !  year of purchase), and divide by capacity factor to account
      !  for the fact that equipment purchased will be in place for
      !  entire year, not just the time used to meet demand.

          CostperKBtu = (TechInvCost/8760.0) / CapacityFactor(r,b,s)   ! invest
          SubsidyperKBtu = (TechCost(r,t,v,iSubCost)/8760.0) / CapacityFactor(r,b,s)          !investsub
          Subsidy111dperKBtu = (TechCost(r,t,v,iSub111dCost)/8760.0) / CapacityFactor(r,b,s)  !investsub 111(d)

          InvestCost(r,s,y)= InvestCost(r,s,y) +                     & ! invest
            (InvestDmd * CostperKBtu)                                  ! invest

          SubsidyInvestCost(r,s,y) = SubsidyInvestCost(r,s,y) +      & ! invest               !investsub
            (InvestDmd * SubsidyperKBtu)                               ! invest               !investsub 111(d)

			
          Subsidy111dInvestCost(r,s,y) = Subsidy111dInvestCost(r,s,y) + & ! invest            !investsub
            (InvestDmd * Subsidy111dperKBtu)              ! invest            !investsub 111(d)



          PurchSD(r,b,s)= PurchSD (r,b,s) + InvestDmd                  ! peff
          PRtSD(r,b,s)= PRtSD(r,b,s) + InvestRetroDmd(t,v)             ! peff

          ENDIF     !Check to see if tech available                      invest

  314    CONTINUE

          ENDIF  ! TechbyModel=1

      ! Code to print debugging diagnostics for investment (additional code farther up)
!      IF(PRTDBGK.eq.1 .and. CURCALYR.ge.2017 .and. CURITR.eq.MAXITR+1) THEN         ! invest
!       IF(s.le.2 .and. TechInvCost.gt.0.0 .and. CURIYR.le.7)           & ! invest
!         WRITE (RCDBG,6002) y,t,v,r,b,InvestRetroDmd(t,v),InvestDmd,   & ! invest
!         TechInvCost, CostperKBtu, SubsidyperkBtu, Subsidy111dperkBtu, & ! invest
!         InvestCost(r,s,y), SubsidyInvestCost(r,s,y), Subsidy111dInvestCost(r,s,y)  ! invest
! 6002  FORMAT (1X,I1,1X,I2,2(1X,I1),1X,I2,2(2X,E9.4),2X,F6.2,2(2x,E9.4)) ! invest
!      END IF   !Check for debug switch and iteration                       invest

        END DO ! v
       END DO  ! t


!  Now that composite equipment market shares for each decision type
!  have been obtained, calculate the resulting fuel shares and
!  effective average efficiencies for each decision type.
!  Here, 'fuel share' is defined as the proportion of a given segment
!  (eg, decision segment) of service demand that is satisfied by a
!  given fuel.  'Effective average efficiency' is defined as the
!  ratio of service demand to consumption for a given segment and
!  fuel, and is calculated as the reciprocal sum of weighted
!  reciprocal equipment efficiencies, with the weights given by the
!  equipment market shares of service.

         DO 270 d= 1, CMDecision        ! For each decision type
          DO 270 f= 1, CMnumMajFl       ! For each major fuel
           FS (r,b,s,d,f) = 0.0         ! Initialize fuel share
           AE (r,b,s,d,f) = 0.0         !  average efficiency and
           FSPRt(r,b,s,f) = 0.0         ! purchased retro fuel share peff

           ! Sum market shares over equipment using this fuel
           ! to obtain fuel shares for this decision type
           DO 250 TsubS= 1, CMnumTechsforService (s)
            t= TechsforService (s,TsubS)
            DO 250 v= 1, CMnumEqV (t)
             IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
              FS (r,b,s,d,f) = FS (r,b,s,d,f) + &
               MS (b,s,d,t,v) * FuelbyTech (t,f)
              IF (d.EQ.3 .AND. SSD(r,b,s,y).GT.0.0) FSPRt(r,b,s,f) = &    ! peff-error check divide by zero
               FSPRt(r,b,s,f) +(InvestRetroDmd(t,v) /SSD(r,b,s,y) &       ! peff
                   * FuelbyTech(t,f))  ! peff
             ENDIF
  250      CONTINUE
         ! [Error check: sum over f of FS should = 1 for given r,etc]

           ! Divide fuel share by sum of (market share / efficiency)
           ! over all equipment using given fuel, to obtain effective
           ! average efficiency for that fuel use.
           denominator= 0.0
           DO 260 TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
            DO 260 v= 1, CMnumEqV (t)
             IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
              IF ( TechEff (r,s,t,v) .GT. 0.0 ) &
               denominator= denominator + &
               ( MS (b,s,d,t,v) * FuelbyTech (t,f) / &
                 TechEff (r,s,t,v) )
             ENDIF
  260      CONTINUE
           IF ( denominator .GT. 0.0 ) &
           AE (r,b,s,d,f)= FS (r,b,s,d,f) / denominator

  270    CONTINUE
!
!  Consolidate across decision types to compute fuel shares of this
!  service for each fuel, and market shares for each equipment type.
!  Here, 'market share' is defined as the proportion of service
!  demand in each Census Division and building type that is satisfied
!  by equipment of a given vintage and technology.

         y= CURIYR      ! for more concise notation
         ! Substitute new nomenclature for that existing in other
         ! subroutines, until permanent changes are made.
         NSD (r,b,s,y)= NewServDmd (s,b,r,y)
         RSD (r,b,s,y)= RetireServDmd (s,b,r,y)
         SSD (r,b,s,y)= ServDmdSurv (s,b,r,y)

         ! Compute fuel shares by service across decision types:
         DO 280 f= 1, CMnumMajFl
          FuelShareofService (r,b,s,f)= 0.0      ! Initialize
          PurchFuelShare (r,b,s,f)= 0.0          ! Initialize                 peff
          denominator= NSD (r,b,s,y) + RSD (r,b,s,y) + SSD (r,b,s,y)
             SDbyFuel(f,s,b,r,y)=0.
             IF (denominator .GT. 0.0)  THEN 
               FuelShareofService (r,b,s,f)= &
               (   NSD (r,b,s,y) * FS (r,b,s,1,f) &
                 + RSD (r,b,s,y) * FS (r,b,s,2,f) &
                 + SSD (r,b,s,y) * FS (r,b,s,3,f) ) / denominator
               SDbyFuel(f,s,b,r,y)=NSDnoShell(r,b,s) * FS (r,b,s,1,f) &    !Efficiency Index SDbyFuel
                                 + RSDnoShell(r,b,s) * FS (r,b,s,2,f) &    !Efficiency Index
                                 + SSDnoShell(r,b,s) * FS (r,b,s,3,f)      !Efficiency Index
             ENDIF

          IF (PurchSD (r,b,s) .GT. 0.0)                                  & !  peff
           PurchFuelShare (r,b,s,f)=                                     & !  peff
            (   NSD (r,b,s,y) * FS (r,b,s,1,f)                           & !  peff
              + RSD (r,b,s,y) * FS (r,b,s,2,f)                           & !  peff
              + PRtSD (r,b,s) * FSPRt (r,b,s,f) ) / PurchSD (r,b,s)        !  peff
  280    CONTINUE

         ! Compute market shares of equipment across decision types:
         DO 290 TsubS= 1, CMnumTechsforService (s)
          t= TechsforService (s,TsubS)
          DO 290 v= 1, CMnumEqV (t)
           IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
            TechShareofService (r,b,s,t,v)= 0.0       ! Initialize
            PurchTechShare (r,b,s,t,v)= 0.0           ! Initialize            peff

            IF (denominator .GT. 0.0)             & ! same denominator as above
             TechShareofService (r,b,s,t,v) =     &
             (   NSD (r,b,s,y) * MS (b,s,1,t,v)   &
               + RSD (r,b,s,y) * MS (b,s,2,t,v)   &
               + SSD (r,b,s,y) * MS (b,s,3,t,v) ) / denominator            

            IF (PurchSD (r,b,s) .GT. 0.0)                                & !  peff
             PurchTechShare (r,b,s,t,v) =                                & !  peff
             (   NSD (r,b,s,y) * MS (b,s,1,t,v)                          & !  peff
               + RSD (r,b,s,y) * MS (b,s,2,t,v)                          & !  peff
               + PRtSD (r,b,s) * (InvestRetroDmd(t,v)/SSD (r,b,s,y)))    & !  peff
               / PurchSD (r,b,s)                                           !  peff
           ENDIF
  290    CONTINUE

         ! Compute effective average efficiency of equipment mix
         ! for each fuel used to satisfy this service demand in this
         ! Census Division, building type, and year.

         DO 310 f= 1, CMnumMajFl
          ! Divide fuel share by sum of (market share / efficiency)
          ! over all equipment using given fuel, to obtain effective
          ! average efficiency for that fuel use.
          AverageEfficiency (r,b,s,f)= 0.0
          denominator= 0.0
          DO 300 TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
           DO 300 v= 1, CMnumEqV (t)
            IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
             IF ( TechEff (r,s,t,v) .GT. 0.0 ) &
              denominator= denominator + &
              ( TechShareofService (r,b,s,t,v) * FuelbyTech (t,f) / &
                TechEff (r,s,t,v) )
            ENDIF
  300     CONTINUE

          IF ( denominator .GT. 0.0 ) THEN                             !Rbndfix
           AverageEfficiency (r,b,s,f)= &
            FuelShareofService (r,b,s,f) / denominator
          ELSE                                                         !Rbndfix
           AverageEfficiency (r,b,s,f)=                                 & !Rbndfix
            PrevYrAverageEfficiency (r,b,s,f)                          !Rbndfix
          ENDIF   !  Check for 0 Avg. efficiency - for rebound effect   Rbndfix

!          IF (PRTDBGK .EQ. 1) THEN
!          IF (CURITR .EQ. 1 .AND. s .EQ. 2 .AND. f .EQ. 2
!     &        .AND. y.EQ.7)
!     &      write(rcdbg,*) 'r',r,' b',b,' s',s,' f',f,
!     &      ' Avg. Eff',AverageEfficiency(r,b,s,f),
!     &      ' Fuel Shr',FuelShareofService(r,b,s,f)
!          END IF !Check for debug file

!*****************************************************************

        IF (b.EQ.10) THEN                                             !indytpc - capture warehouse info for IDM
       WHSE_EFF(r,s,f,y) = AverageEfficiency(r,10,s,f)                !indytpc - Average Efficiency in Warehouses for IDM
         WHSE_SD(r,s,f,y)  = SDbyFuel(f,s,10,r,y)                     !indytpc - Service Demand in Warehouses for IDM
!         IF (CURITR.EQ.1) THEN
!         WRITE(RCDBG,5551) y,r,b,s,f,AverageEfficiency(r,b,s,f),SDbyFuel(f,s,b,r,y)
!         ENDIF
        ENDIF   !  Check for warehouse bldg type to capture information for IDM

!5551   FORMAT(5(i5,2x),2(f10.5),'y,r,b,s,f,AverageEfficiency(r,b,s,f),SDbyFuel(f,s,b,r,y)')

!*****************************************************************

  310    CONTINUE

      ! Update equip service demand total for this r,b,s,y:
      DO TsubS= 1, CMnumTechsforService (s)
       t= TechsforService (s,TsubS)
       DO v= 1, CMnumEqV (t)
!        IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
! add b to ksdout
          EquipSD (r,b,t,v,1,y)= EquipSD (r,b,t,v,1,y)                      & 
          + MS (b,s,1,t,v) * NSD (r,b,s,y)
          EquipSD (r,b,t,v,2,y)= EquipSD (r,b,t,v,2,y)                      & 
                           + MS (b,s,2,t,v) * RSD (r,b,s,y)
          EquipSD (r,b,t,v,3,y)= EquipSD (r,b,t,v,3,y)                      & 
                           + MS (b,s,3,t,v) * SSD (r,b,s,y)
!      TOTAL d=4 REMOVED FROM EquipSD ARRAY TO CONSERVE MEMORY                  
!        ebe07-03
!          EquipSD (r,b,t,v,4,y)= EquipSD (r,b,t,v,1,y)                      &
!                           + EquipSD (r,b,t,v,2,y) + EquipSD (r,b,t,v,3,y)   
          EquipSD (11,b,t,v,1,y)= EquipSD (11,b,t,v,1,y)                    & 
                           + MS (b,s,1,t,v) * NSD (r,b,s,y)                   
          EquipSD (11,b,t,v,2,y)= EquipSD (11,b,t,v,2,y)                    & 
                           + MS (b,s,2,t,v) * RSD (r,b,s,y)                   
          EquipSD (11,b,t,v,3,y)= EquipSD (11,b,t,v,3,y)                    & 
                           + MS (b,s,3,t,v) * SSD (r,b,s,y)                   
!      TOTAL d=4 REMOVED FROM EquipSD ARRAY TO CONSERVE MEMORY                  
!        ebe07-03
!          EquipSD (11,b,t,v,4,y)= EquipSD (11,b,t,v,1,y)                    &
!                           + EquipSD(11,b,t,v,2,y) + EquipSD(11,b,t,v,3,y)   
!         ENDIF
        END DO ! v
       END DO  ! t

 8000   CONTINUE  ! End of pass through all building types for
                  ! this service and region


! Compute fuel shares of this service for this region, by
! consolidating across building types.

         DO 320 f= 1, CMnumMajFl
          RegionFuelShare (r,s,f)= 0.0      ! Initialize
          numerator= 0.0
          denominator= 0.0
          DO 315 b= 1, CMnumBldg
           numerator= numerator + FuelShareofService (r,b,s,f) * &
            ( NSD (r,b,s,y) + RSD (r,b,s,y) + SSD (r,b,s,y) )
           denominator= denominator + &
              NSD (r,b,s,y) + RSD (r,b,s,y) + SSD (r,b,s,y)
  315     CONTINUE

          IF (denominator .GT. 0.0) &
           RegionFuelShare (r,s,f)= numerator / denominator

  320    CONTINUE


!  Compute effective average efficiency of equipment mix for each
!  fuel used to satisfy this service demand in this Census Division
!  and year.  Essentially, this is obtained by computing the ratio
!  of service demand satisfied by a particular fuel to consumption
!  of that fuel by equipment providing that service.

         DO 330 f= 1, CMnumMajFl
          RegionAvgEff (r,s,f)= 0.0   !  Initialize
          numerator= 0.0
          denominator= 0.0

          DO 325 b= 1,CMnumBldg
           numerator= numerator + FuelShareofService (r,b,s,f) * &
            ( NSD (r,b,s,y) + RSD (r,b,s,y) + SSD (r,b,s,y) )

           DO 325 TsubS= 1, CMnumTechsforService (s)
            t= TechsforService (s,TsubS)
            DO 325 v= 1, CMnumEqV (t)
             IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
              IF ( TechEff (r,s,t,v) .GT. 0.0 ) &
               denominator= denominator + &
               ( TechShareofService (r,b,s,t,v) * FuelbyTech (t,f) / &
                 TechEff (r,s,t,v) * &
                 ( NSD (r,b,s,y) + RSD (r,b,s,y) + SSD (r,b,s,y) )  )
             ENDIF
  325     CONTINUE

          IF ( denominator .GT. 0.0 ) &
          RegionAvgEff (r,s,f)= numerator / denominator
  330    CONTINUE


! Compute fuel shares of this service for each decision type in
! this region, by consolidating across building types.

         DO 340 f= 1, CMnumMajFl
          DO 340 d= 1, CMDecision
           DecFuelShare (r,s,d,f,y)= 0.0      ! Initialize
           numerator= 0.0
           denominator= 0.0

           DO 335 b= 1, CMnumBldg
            IF (d .EQ. 1) THEN
             numerator= numerator + FS (r,b,s,d,f) * NSD (r,b,s,y)
             denominator= denominator + NSD (r,b,s,y)
            ELSE IF (d .EQ. 2) THEN
             numerator= numerator + FS (r,b,s,d,f) * RSD (r,b,s,y)
             denominator= denominator + RSD (r,b,s,y)
            ELSE IF (d .EQ. 3) THEN
             numerator= numerator + FS (r,b,s,d,f) * SSD (r,b,s,y)
             denominator= denominator + SSD (r,b,s,y)
            END IF
  335      CONTINUE

           IF (denominator .GT. 0.0) &
            DecFuelShare (r,s,d,f,y)= numerator / denominator

  340    CONTINUE


!  Compute effective average efficiency of equipment mix in each
!  decision segment, for each fuel used to satisfy this service
!  demand in this Census Division and year.

         DO 350 f= 1, CMnumMajFl
          DO 350 d= 1, CMDecision
           DecAvgEff (r,s,d,f,y)= 0.0   !  Initialize
           numerator= 0.0
           denominator= 0.0

           DO 345 b= 1,CMnumBldg
            IF (d .EQ. 1 .AND. AE (r,b,s,d,f) .GT. 0.0) THEN
             numerator= numerator + FS (r,b,s,d,f) * NSD (r,b,s,y)
             denominator= denominator + &
               ( FS (r,b,s,d,f) * NSD (r,b,s,y)  / &
                 AE (r,b,s,d,f) )
            ELSE IF (d .EQ. 2 .AND. AE (r,b,s,d,f) .GT. 0.0) THEN
             numerator= numerator + FS (r,b,s,d,f) * RSD (r,b,s,y)
             denominator= denominator + &
              ( FS (r,b,s,d,f) * RSD (r,b,s,y)  / &
                 AE (r,b,s,d,f) )
            ELSE IF (d .EQ. 3 .AND. AE (r,b,s,d,f) .GT. 0.0) THEN
             numerator= numerator + FS (r,b,s,d,f) * SSD (r,b,s,y)
             denominator= denominator + &
               ( FS (r,b,s,d,f) * SSD (r,b,s,y)  / &
                 AE (r,b,s,d,f) )
            END IF
  345      CONTINUE

           IF ( denominator .GT. 0.0 ) &
            DecAvgEff (r,s,d,f,y)= numerator / denominator
  350    CONTINUE


!  Increment National investment cost for this service and year

         InvestCost(11,s,y)= InvestCost(11,s,y) + InvestCost (r,s,y)
         SubsidyInvestCost(11,s,y) = SubsidyInvestCost(11,s,y) + SubsidyInvestCost(r,s,y)  		 !investsub
		 Subsidy111dInvestCost(11,s,y) = Subsidy111dInvestCost(11,s,y) + Subsidy111dInvestCost(r,s,y)

		
 9000 CONTINUE  ! End of segment executed for major services each
                ! iteration of each forecast year.

!*****************************************************************
!     indytpc block starts here
!       Calculate weighted mappings to Census Regions for use as TPC drivers in Industrial Model
!       Applies only to Heating, Lighting, Ventilation, and Building Shell, for Warehouses

!       Region      Division
!       1           1,2
!       2           3,4
!       3           5,6,7
!       4           8,9

!       Building Shell Index:  Simple Averages across divisions

        WHSE_ShellIndex(1,y) = sum(ShellHeatFactor(10,1:2,1,y))/2  !indytpc
        WHSE_ShellIndex(2,y) = sum(ShellHeatFactor(10,3:4,1,y))/2  !indytpc
        WHSE_ShellIndex(3,y) = sum(ShellHeatFactor(10,5:7,1,y))/3  !indytpc
        WHSE_ShellIndex(4,y) = sum(ShellHeatFactor(10,8:9,1,y))/2  !indytpc

!       Energy Intensity Index:  Weighted by service demand

!       Heating (s=1)

      DO 355 f = 1, 3    ! By major fuel:  EL, NG, DS

!         Region 1
          numerator   = sum(WHSE_SD(1:2,1,f,y))
          denominator = sum(WHSE_SD(1:2,1,f,y)*WHSE_EFF(1:2,1,f,y))
          IF (denominator.ne.0.0) WHSE_HeatIndex(1,f,y) = numerator/denominator

!         Region 2
          numerator   = sum(WHSE_SD(3:4,1,f,y))
          denominator = sum(WHSE_SD(3:4,1,f,y)*WHSE_EFF(3:4,1,f,y))
          IF (denominator.ne.0.0) WHSE_HeatIndex(2,f,y) = numerator/denominator

!         Region 3
          numerator   = sum(WHSE_SD(5:7,1,f,y))
          denominator = sum(WHSE_SD(5:7,1,f,y)*WHSE_EFF(5:7,1,f,y))
          IF (denominator.ne.0.0) WHSE_HeatIndex(3,f,y) = numerator/denominator

!         Region 4
          numerator   = sum(WHSE_SD(8:9,1,f,y))
          denominator = sum(WHSE_SD(8:9,1,f,y)*WHSE_EFF(8:9,1,f,y))
          IF (denominator.ne.0.0) WHSE_HeatIndex(4,f,y) = numerator/denominator

355       CONTINUE

        DO r=1,4                                      ! Add dummmy values for LG and GS, for IDM model
          WHSE_HeatIndex(r,4,y) = WHSE_HeatIndex(r,3,y)
          WHSE_HeatIndex(r,5,y) = WHSE_HeatIndex(r,3,y)
        ENDDO

!       Ventilation (s=4,f=1)

!         Region 1
          numerator   = sum(WHSE_SD(1:2,4,1,y))
          denominator = sum(WHSE_SD(1:2,4,1,y)*WHSE_EFF(1:2,4,1,y))
          IF (denominator.ne.0.0) WHSE_VentIndex(1,y) = numerator/denominator

!         Region 2
          numerator   = sum(WHSE_SD(3:4,4,1,y))
          denominator = sum(WHSE_SD(3:4,4,1,y)*WHSE_EFF(3:4,4,1,y))
          IF (denominator.ne.0.0) WHSE_VentIndex(2,y) = numerator/denominator

!         Region 3
          numerator   = sum(WHSE_SD(5:7,4,1,y))
          denominator = sum(WHSE_SD(5:7,4,1,y)*WHSE_EFF(5:7,4,1,y))
          IF (denominator.ne.0.0) WHSE_VentIndex(3,y) = numerator/denominator

!         Region 4
          numerator   = sum(WHSE_SD(8:9,4,1,y))
          denominator = sum(WHSE_SD(8:9,4,1,y)*WHSE_EFF(8:9,4,1,y))
          IF (denominator.ne.0.0) WHSE_VentIndex(4,y) = numerator/denominator

!       Lighting (s=6,f=1)

!         Region 1
          numerator   = sum(WHSE_SD(1:2,6,1,y))
          denominator = sum(WHSE_SD(1:2,6,1,y)*WHSE_EFF(1:2,6,1,y))
          IF (denominator.ne.0.0) WHSE_LightIndex(1,y) = numerator/denominator

!         Region 2
          numerator   = sum(WHSE_SD(3:4,6,1,y))
          denominator = sum(WHSE_SD(3:4,6,1,y)*WHSE_EFF(3:4,6,1,y))
          IF (denominator.ne.0.0) WHSE_LightIndex(2,y) = numerator/denominator

!         Region 3
          numerator   = sum(WHSE_SD(5:7,6,1,y))
          denominator = sum(WHSE_SD(5:7,6,1,y)*WHSE_EFF(5:7,6,1,y))
          IF (denominator.ne.0.0) WHSE_LightIndex(3,y) = numerator/denominator

!         Region 4
          numerator   = sum(WHSE_SD(8:9,6,1,y))
          denominator = sum(WHSE_SD(8:9,6,1,y)*WHSE_EFF(8:9,6,1,y))
          IF (denominator.ne.0.0) WHSE_LightIndex(4,y) = numerator/denominator

!*****************************************************************

!       Check to see if the calculations went as expected

        IF (CURIYR.EQ.LASTYR.AND.CURITR.EQ.MAXITR) THEN
         IF(PRTDBGK.EQ.1) Write(RCDBG,*) " r b s ", r, b ,s                           !misctest
      write(RCDBG,*) ' Warehouse Shell Efficiency'
        do iy=15,lastyr
        write(RCDBG,5531) iy,(WHSE_ShellIndex(r,iy),r=1,4)
      enddo
        write(RCDBG,*)
        write(RCDBG,*) '  Warehouse Heat Index '
        do iy=15,lastyr
          write(RCDBG,5532) iy,(WHSE_HeatIndex(r,1,iy),r=1,4),(WHSE_HeatIndex(r,2,iy),r=1,4),(WHSE_HeatIndex(r,3,iy),r=1,4)
        enddo
        write(RCDBG,*)
        write(RCDBG,*) '  Warehouse Ventilation Index '
        do iy=15,lastyr
          write(RCDBG,5531) iy,(WHSE_VentIndex(r,iy),r=1,4)
        enddo
        write(RCDBG,*)
        write(RCDBG,*) '  Warehouse Lighting Index '
        do iy=15,lastyr
          write(RCDBG,5531) iy,(WHSE_LightIndex(r,iy),r=1,4)
        enddo

        ENDIF

5531    Format(i4,2x,4(f8.4,2x))
5532    Format(i4,2x,12(f8.4,2x))
!     indytpc block ends here
!******************************************************************************


!  Compute US Average Efficiencies for Major Services and Fuels:

      DO 360 s= 1, CMnumMajServ        ! for FTAB report
       DO 360 f= 1, CMnumMajFl

        CMUSAvgEff (s,f,y)= 0.0
        CMUSPurchEff(s,f,y)= 0.0                                 ! peff
        numerator=          0.0
        denominator=        0.0
        numpurch= 0.0                                            ! peff
        denpurch= 0.0                                            ! peff

        DO 370 r= 1, MNUMCR-2
         DO 370 b= 1, CMnumBldg

          numerator= numerator + FuelShareofService (r,b,s,f) * &
           ( NSD(r,b,s,y) + RSD(r,b,s,y) + SSD(r,b,s,y) )

          numpurch= numpurch + PurchFuelShare(r,b,s,f) *          & ! peff
           PurchSD (r,b,s)                                       ! peff

          DO 370 TsubS= 1, CMnumTechsforService (s)
           t= TechsforService (s,TsubS)
           DO 370 v= 1, CMnumEqV (t)
            IF (TechbyModel(t,v) .EQ. 1) THEN     ! all v this tech
             IF (TechEff(r,s,t,v) .GT. 0.0) THEN
              denominator= denominator + &
              ( TechShareofService (r,b,s,t,v) * FuelbyTech (t,f) / &
                TechEff (r,s,t,v) * &
                ( NSD(r,b,s,y) + RSD(r,b,s,y) + SSD(r,b,s,y) ) )

              denpurch= denpurch +                                & ! peff
              ( PurchTechShare (r,b,s,t,v) * FuelbyTech (t,f) /   & ! peff
                TechEff (r,s,t,v) * PurchSD (r,b,s) )               ! peff
             ENDIF   ! Efficiency greater than 0.0
            ENDIF    ! TechbyModel equal 1
  370   CONTINUE

          IF (denominator .GT. 0.0) &
           CMUSAvgEff (s,f,CURIYR)= numerator / denominator

          IF (denpurch .GT. 0.0)                                  & ! peff
           CMUSPurchEff (s,f,CURIYR)= numpurch / denpurch           ! peff

  360 CONTINUE

!  Process Minor Services:
      DO 9100 s= CMnumMajServ+1, CMnumServ
       DO 9090 r= 1, MNUMCR-2
        DO 9010 b= 1, CMnumBldg

         ! Increase efficiency by exogenous growth rate.
         ! Note: based on efficiency indexed to 1 in 2012;
         AverageEfficiency (r,b,s,1)=  & ! electric only
          PrevYrAverageEfficiency (r,b,s,1) * &
          (1.0 + EffGrowthRate (s))

        IF (s .LT. CMnumServ) THEN
         ! Explicitly set fuel shares to all-electric for office equipment:
         FuelShareofService (r,b,s,1)= 1.0
        ELSE   ! Misc
       !Calc of MISCELLANEOUS fuel shares changed to accomodate explicit misc el. use categories 
!         FSnormfac = MarketPenetration(3,y) *                          & !Misc
!          FuelShareofServiceBASE(r,b,s,1)                              & !Misc
!          + FuelShareofServiceBASE(r,b,s,2) +                          & !Misc
!            FuelShareofServiceBASE(r,b,s,3)                              !Misc
!         FuelShareofService(r,b,s,1) = MarketPenetration(3,y) *        & !Misc
!          FuelShareofServiceBASE(r,b,s,1) / FSnormfac                    !Misc
!         DO f=2,CMnumMajFl                                               !Misc
!          FuelShareofService(r,b,s,f) =                                & !Misc
!           FuelShareofServiceBASE(r,b,s,f) / FSnormfac                   !Misc
!         END DO ! f                                                       Misc
        ! Initialize El. misc. fuel share to 1.0, will subtract off shares for NG and DS
         FuelShareofService (r,b,s,1)= 1.0        ! miscdetail initialize El. misc. fuel share to 1.0, will subtract off shares for NG and DS
!         IF(PRTDBGK.EQ.1) Write(RCDBG,*) " r b s ", r, b ,s              !misctest
!         IF(PRTDBGK.EQ.1) Write(RCDBG,*) " El BaseFS ", FuelShareofServiceBASE(r,b,s,1) !misctest
         DO f=2,CMnumMajFl                                                !miscdetail
          FuelShareofService(r,b,s,f) =                                 & !miscdetail
           (FuelShareofServiceBASE(r,b,s,f) * ServDmdIntenBASE(s,b,r)   & !miscdetail
           /1000.0 * (SurvFloorTotal(r,b,CURIYR)+CMNewFloorSpace(r,b,CURIYR))) / & ! miscdetail
           (ServDmdSurv(s,b,r,CURIYR) + NewServDmd (s,b,r,CURIYR))        !miscdetail
          FuelShareofService(r,b,s,1) = FuelShareofService(r,b,s,1) -   & !miscdetail
           FuelShareofService(r,b,s,f)                                    !miscdetail
!          WRITE (RCDBG,*)" f FS BaseFS ",f,FuelShareofService(r,b,s,f),& !misctest
!            FuelShareofServiceBASE(r,b,s,f)
!          WRITE (RCDBG,*) " El FS ", FuelShareofService(r,b,s,1)," f ",f !misctest
         END DO ! f                                                        miscdetail


!      IF (PRTDBGK.eq.1 .and. b.eq.1 .and. r.eq.1 ) THEN                  !misctest
!        WRITE (RCDBG,*) ' year ',CURIYR+1989, ' iteration ', CURITR      !misctest
!        WRITE (RCDBG,*) " MarketPenetration ", MarketPenetration(3,y), & !misctest
!          " El FS ", FuelShareofService(r,b,s,1)                         !misctest
!      END IF                                                             !misctest
        END IF ! Office equip, Misc. split.

 9010   CONTINUE  ! iteration across buildings

       IF (s .LT. CMnumServ) THEN
        ! propogate results into other eff & fuelshare variables:
        ! Note: values are invariant across building type.
         RegionAvgEff (r,s,1)= AverageEfficiency (r,1,s,1)
         RegionFuelShare (r,s,1)= FuelShareofService (r,1,s,1)

         DO 9020 d= 1, CMDecision
          DecAvgEff (r,s,d,1,y)= RegionAvgEff (r,s,1)
          DecFuelShare (r,s,d,1,y)= 1.0 ! all-electric
 9020    CONTINUE

       ELSE    ! Misc
        ! propogate results into other eff & fuelshare variables:
        DO 9050 f= 1, CMnumMajFl
         ! Note: efficiency values are invariant across building type.
         RegionAvgEff (r,s,f) = AverageEfficiency(r,1,s,f)       ! Misc
         numerator = 0.0                                         ! Misc
         denominator = 0.0                                       ! Misc

         DO 9055 b = 1, CMnumBldg
          numerator = numerator + FuelShareofService (r,b,s,f) *  & ! Misc
           (NewServDmd(s,b,r,y) + ServDmdSurv(s,b,r,y))          ! Misc
          denominator = denominator +                             & ! Misc
           (NewServDmd(s,b,r,y) + ServDmdSurv(s,b,r,y))          ! Misc
 9055    CONTINUE
        IF (denominator .GT. 0.0)                                 & ! Misc
         RegionFuelShare (r,s,f) = numerator / denominator       ! Misc
        DO d= 1, CMDecision
         DecAvgEff(r,s,d,f,y)= RegionAvgEff (r,s,f)              ! Misc
         DecFuelShare (r,s,d,f,y) = RegionFuelShare (r,s,f)      ! Misc
        END DO ! Across decision types
 9050   CONTINUE
       END IF ! Office equip and Misc. split.

 9090   CONTINUE! minor service iteration across Census Divisions
 9100  CONTINUE  ! minor service iteration across services

      RETURN
      END

! ********************************************************************
! This is the Consumption modeling subroutine of the Commercial Module
! of the National Energy Modeling System, NEMS.
! ********************************************************************

      SUBROUTINE COMConsumption (RCDBG)
      IMPLICIT NONE
      INTEGER FILE_MGR
      include'parametr'
      include'ncntrl'
      include'comout'
      include'cogen'
      include'qsblk'
      include'macout'
      include'comparm'
      include'comvars'
      include'eusprc'
      include'apq'
      include'commrep' ! nems ftab report variables
      include'steoblock' ! include common block with STEO variables ! STEOread-avg

!   DISTGEN OUTPUTS (NO. OF INSTALLED UNITS, TOTAL ELEC. GENERATION
!      GENERATION FOR OWN USE, HOT WATER GAS CONSUMPTION AVOIDED AND
!      SUPPLIED BY FUEL CELLS TO NET AGAINST GAS HOT WATER DEMAND) AND
!      INVESTMENT CAPITAL EXPENDITURES
!        NTEK=1 FOR SOLAR
!        NTEK=2 FOR FUEL CELLS
!        NTEK=3 FOR CONVENTIONAL COGEN - GAS Engine
!        NTEK=4 FOR CONVENTIONAL COGEN - COAL
!        NTEK=5 FOR CONVENTIONAL COGEN - OIL
!        NTEK=6 FOR CONVENTIONAL COGEN - MSW
!        NTEK=7 FOR CONVENTIONAL COGEN - Gas Turbines
!        NTEK=8 FOR Gas Micro Turbines
!        NTEK=9 FOR HYDRO
!        NTEK=10 FOR Wood
!        NTEK=11 FOR Wind
!
      INTEGER NTEK                                                  !distgen
      PARAMETER (NTEK=11)                                           !distgen
      COMMON/CMDGOUT/UNITS(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    TRILLS(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    capacity(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &      !varyxkw
                    TRILLSOWNUSE(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    fuelusage(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    HWBTU(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    SHBTU(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    INVEST(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    Ifueltype(ntek)
       REAL*4 UNITS, TRILLS, capacity, TRILLSOWNUSE,TotUnits, &     !varyxkw
               fuelusage,HWBTU,shbtu,INVEST
       Integer ifueltype                                            !distgen
!    Common block for communicating with benchmarking routine       !distgen
       COMMON/SolarPV/SolarPVContrib(MNUMYR,MNUMCR)                 !distgen
       REAL*4 SolarPVContrib                                        !distgen
 !    Minor Fuel Floorspace Adjustment Factor Dodge --> CBECS by Division
       REAL*4     FloorAdj (MNUMCR-2)                                      !minorfuel project
       REAL*4     Unadjusted                                               !Efficiency Index 
       REAL*4     SSDnoShell(MnumCR-2,CMnumBldg,CMnumServ)                 !Efficiency Index
       REAL*4     RSDnoShell(MnumCR-2,CMnumBldg,CMnumServ)                 !Efficiency Index
       REAL*4     NSDnoShell(MnumCR-2,CMnumBldg,CMnumServ)                 !Efficiency Index
       REAL*4     SDbyFuel(CMNumMajFl,CMnumServ,CMnumBldg,MNUMCR-2,MNUMYR) !Efficiency Index
       COMMON /EffInd/ SSDnoShell,RSDnoShell,NSDnoShell,SDbyFuel           !Efficiency Index  
!     Variables for miscellaneous electricity detail
!   Add Declarations for detailed miscellaneous end use calculations       ! miscdetail
      COMMON /MiscEl/ CoffeeBrewers,XfmrsDry,Security,ElVehicles,        & ! miscdetail
                 KitchenVent,LabRefFrz,LrgVidBoard,           & ! miscdetail
                 FumeHoods,Laundry,MedImaging,Elevators,Escalators,      & ! miscdetail
                 xmisccalc                                                 ! miscdetail
      REAL*4    CoffeeBrewers,XfmrsDry,Security,ElVehicles,              & ! miscdetail
                 KitchenVent,LabRefFrz,LrgVidBoard,           & ! miscdetail
                 FumeHoods,Laundry,MedImaging,Elevators,Escalators         ! miscdetail
      REAL*4    xmisccalc    ! holding variable for curiyr-14 to use in misc equations

      COMMON /MiscElQ/ CoffeeBrewersElQ,XfmrsDryElQ,SecurityElQ,         & ! miscdetail
                 KitchenVentElq,LabRefFrzElq,LrgVidBoardElq, & ! miscdetail
                 ElVehiclesElQ,FumeHoodsElQ,LaundryElQ,MedImagingElQ,    & ! miscdetail
                 ElevatorsElQ,EscalatorsElQ,TotExplicitMiscElQ             ! miscdetail
      REAL*4 CoffeeBrewersElQ(MNUMCR-2,CMnumBldg,MNUMYR)                   ! miscdetail
      REAL*4 XfmrsDryElQ(MNUMCR-2,CMnumBldg,MNUMYR)                        ! miscdetail
      REAL*4 SecurityElQ(MNUMCR-2,CMnumBldg,MNUMYR)                        ! miscdetail
      REAL*4 ElVehiclesElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 KitchenVentElq(MNUMCR-2,CMnumBldg,MNUMYR)                     ! miscdetail
      REAL*4 LabRefFrzElQ(MNUMCR-2,CMnumBldg,MNUMYR)                       ! miscdetail
      !REAL*4 VidDisplayElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 LrgVidBoardElQ(MNUMCR-2,CMnumBldg,MNUMYR)                     ! miscdetail
      REAL*4 FumeHoodsElQ(MNUMCR-2,CMnumBldg,MNUMYR)                       ! miscdetail
      REAL*4 LaundryElQ(MNUMCR-2,CMnumBldg,MNUMYR)                         ! miscdetail
      REAL*4 MedImagingElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 ElevatorsElQ(MNUMCR-2,CMnumBldg,MNUMYR)                       ! miscdetail
      REAL*4 EscalatorsElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 TotExplicitMiscElQ(MNUMCR-2,CMnumBldg,MNUMYR)                 ! miscdetail

      ! REAL*4 TechShareofService (MNUMCR,CMnumBldg,CMnumServ,
      !                               CMnumTechs,CMnumEqVint)
      ! Proportion of a given service demand that is satisfied by
      ! equipment of a particular technology & vintage within a
      ! given Census Division, building type.  This array is
      ! initialized with 2012 CBECS shares (from file KTECH),
      ! and is computed for subsequent years.  For each forecast
      ! year it represents the market shares for the previous
      ! year, until it is recalculated for the current year in
      ! the Technology Choice subroutine.

      ! REAL*4 FuelShareofService (MNUMCR,CMnumBldg,CMnumServ,
      !                                           CMnumMajFl)
      ! For Census Division r, building type b, service s, and
      ! fuel f, FuelShareofService (r,b,s,f) is the proportion
      ! of service demand for service s in b,r that is satisfied
      ! by fuel f in the previous year, until updated for the
      ! current year in the Technology Choice subroutine.

      ! REAL*4 AverageEfficiency (MNUMCR,CMnumBldg,CMnumServ,
      !                                          CMnumMajFl)
      ! AverageEfficiency (r,b,s,f) is the effective average
      ! efficiency of the equipment mix using fuel f to satisfy
      ! service demand s in building type b of Census Division r in
      ! the current year, as calculated in the Technology Choice
      ! subroutine.

      ! REAL*4 PrevYrTechShareofService (MNUMCR,CMnumBldg,
      !                 CMnumServ,CMnumTechs,CMnumEqVint)
      ! Proportion of a given service demand that was satisfied by
      ! equipment of a particular technology & vintage within a
      ! given Census Division, and building type during the
      ! previous year.

      ! REAL*4 PrevYrFuelShareofService (MNUMCR,CMnumBldg,
      !                             CMnumServ,CMnumMajFl)
      ! For Census Division r, building type b, service s, and
      ! fuel f, PrevYrFuelShareofService (r,b,s,f) is the proportion
      ! of service demand for service s in b,r that was satisfied
      ! by fuel f during the previous year.

      ! REAL*4 PrevYrAverageEfficiency (MNUMCR,CMnumBldg,
      !                                CMnumServ,CMnumMajFl)
      ! PrevYrAverageEfficiency (r,b,s,f) is the effective average
      ! efficiency of the equipment mix using fuel f to satisfy
      ! service demand s in building type b of Census Division r
      ! during the previous year.

      ! REAL*4 DecFuelShare (MNUMCR,CMnumServ,CMDecision,CMnumMajFl,
      !                                                    MNUMYR)
      ! For Census Division r, service s, and fuel f,
      ! DecFuelShare (r,s,d,f,y) is the proportion of service
      ! demand for service s in region r subject to decision type d
      ! that is satisfied by fuel f in year y.

      ! REAL*4 DecAvgEff (MNUMCR,CMnumServ,CMDecision,CMnumMajFl,
      !                                                  MNUMYR)
      ! DecAvgEff (r,s,d,f,y) is the effective average
      ! efficiency of the equipment mix selected in decision segment
      ! d using fuel f to satisfy service demand s in Census Division
      ! r in year y.

      ! REAL*4 NSD (MNUMCR,CMnumBldg,CMnumServ,11:MNUMYR)
      ! New service demand associated with new floor space.
      ! Will replace NewServDmd in CMServDmdNw common block in
      ! Compar2 include file.

      ! REAL*4 RSD (MNUMCR,CMnumBldg,CMnumServ,11:MNUMYR)
      ! Replacement service demand associated with retiring equipment.
      ! Will replace RetireServDmd in CMServDmdNw common block in
      ! Compar2 include file.

      ! REAL*4 SSD (MNUMCR,CMnumBldg,CMnumServ,11:MNUMYR)
      ! Surviving service demand subject to retrofit decisions.
      ! Will replace ServDmdSurv in CMServDmdNw common block in
      ! Compar2 include file.


      REAL*4 CMCogenEL (MNUMCR-2,CMnumBldg, &
                CmnumMajFl+CMnumMinFl+CMnumRenew+CMnumOthrFl, &
                                 MNUMYR)
      ! Cogenerated Electricity Forecast.  For year CURIYR,
      ! Census Division r, Building Type b, and Fuel Type f,
      ! CMCogenEl (r,b,f,CURIYR) is the current
      ! year forecast for electricity cogeneration in Trillions of
      ! BTUs using fuel f.

      REAL*4 CMCogenElTot    !???
      ! variable used to hold aggregation of,y CMCogenEl (r,b,f,y)
      ! across building type and fuel, for the current Census
      ! Division and year.  Units are Trillions of BTUs.

      REAL*4 CMCogenTE (MNUMCR-2,CMnumBldg, &
                 CMnumMajFl+CMnumMinFl+CMnumRenew+CMnumOthrFl) !???
      ! Total thermal energy produced during electricity
      ! cogeneration, and available to satisfy service demands.
      ! Units are Trillions of BTUs.

      REAL*4 CMCogenConsump (MNUMCR-2,CMnumBldg, &
                 CMnumMajFl+CMnumMinFl+CMnumRenew+CMnumOthrFl) !???
      ! For Census Division r, building type b, and fuel f,
      ! CMCogenConsump (r,b,f) is the consumption of fuel f in
      ! Trillions of BTUs for the purpose of electricity
      ! cogeneration in the current year.

      REAL*4 STEOdat (CMnumALLFl,KSTEOYR-MSEDYR) !STEO estimates
      ! National commercial sector consumption for fuel f
      ! in year y from Short Term Energy Outlook.  Units are
      ! Trillions of BTUs.

      REAL*4 FlagRegSTEO (CMnumALLFl) !Flag for regional STEO estimates
      ! Flag denoting if regional commercial sector consumption for fuel f
      ! is available from Short Term Energy Outlook.  1=yes, 0=no.  regsteo

!     Following 2 variables are for reporting unbenchmarked consump
!     excluding nonbldg consumption and nonutility generation
      REAL*4 UnBenchedCon (CMnumMajFl+CMnumMinFl+CMnumRenew, &
                                      CMnumBldg,MNUMCR,MNUMYR)
      REAL*4 CMFinalUnbenchUse (CMnumMajFl+CMnumMinFl+CMnumRenew, &
                                                MNUMCR,MNUMYR)
      REAL*4 PriceDelta (CMnumMajFl) ! Change in fuel price relative to base yr

      COMMON /CMCONBK1/ UnBenchedCon, CMFinalUnbenchUse

      INTEGER*4 IREG, IFUEL, ISERV, IBLDTP, I, IOS, count, NumErr
      INTEGER*4 RCODE1, RC1, RC2, RCL1, RCL2, RCDBG
      INTEGER*4 IFUEL2
      REAL*4 TERM1, TERM2, TERM3
      REAL*4 numerator     !  holding variable for use in divisions
      REAL*4 denominator   !  holding variable to avoid zero divide
      REAL*4 temp          ! partial result holding variable
      REAL*4 minflprice    ! minor fuel price holding variable
      REAL*4 avgmf         ! temp variable for Minorfuel projections
      REAL*4 totmf         ! temp variable for Minorfuel projections
      REAL*4 totlag        ! temp variable for Minorfuel projections
      REAL*4 totalfs       ! temp variable for Minorfuel projections holding total floorspace in a region
      REAL*4 SDCompositeElas ! composite SD elasticity factor
      REAL*4 PrElaAdj      ! composite elasticity adjust for dist svcs   mfela
      REAL*4 SRElasAmt     ! Amount to add to consumption for short      noneg
                           ! run elasticity, can be + or -
      REAL*4 ReboundEquip  ! Amount to add to consumption for rebound    noneg
                           ! effect, can be + or -
      REAL*4 SRElas        ! Holding variable for SR elasticity input    kelast
      REAL*4 EF1, EF2, EF3 ! elasticity distribution factors to spread
                           ! short run price elasticity effect over 3 years
      INTEGER*4 elasbaseyr ! holding variable for base year index
      Integer   nt         !  loop index for generation technologies
      INTEGER*4 infile     !  Reusable file handle
      INTEGER*4 r,       & !  Region (Census Division)
                b,       & !  Building type
                s,       & !  Service
                f,       & !  Fuel
                y,       & !  Year (1 = 1990)
                iyr,     & !  year for loop when y is curiyr
                d,       & !  Decision type (New, Replacement, Retrofit)
                t,       & !  Technology
                v          !  Vintage
      REAL*4   xElShr(10)  ! Electricity Shares by End use       !distgen
      REAL*4   xtot        ! Holding sum for share computation   !distgen
      REAL*4   TrillsOwnUseAdj, HWBtuAdj, SHBtuAdj, FuelUsageAdj      !distgen
      REAL*4   xstdsavshr  ! Holding calc for xformr stnd savings share computation  !ebill
      REAL*4   prsvShpSavings  ! potential annual savings from spray valve standard based on current year shipments (includes flsp growth) - ebill
          REAL*4   prsvStdSavings  ! all current year savings from spray valve standard including existing units purchased after standard effective - ebill
          REAL*4   FoodSvcHlthLdgFloorTot ! total current food service, health, and lodging floorspace - to distribute hot water savings from standard - ebill
          REAL*4   FoodSvcHlthLdgFlrPrev ! previous year food service, health, and lodging floorspace - to distribute hot water savings from standard - ebill
!bookmark
       ! Default prices by fuel for Minorfuel projections
       REAL*4 DefaultPrices(8)
       Data DefaultPrices/18.,7.,15.,8.,16.,2.,16.,16./

          
      INTEGER*4 NRGBILL    ! switch for energy bill provisions: 0 - no bill, 1 - bill without unfunded provisions, 2 - bill with unfunded items - ebill
      INTEGER*4 STIMULUS    ! switch for feb 2009 stimulus package provisions: 0 - no stimulus, 1 - with stimulus; default is with stimulus - arra09
      INTEGER*4 RTOVALUE   ! External routine to get options
      EXTERNAL RTOVALUE

          NRGBILL = RTOVALUE("NRGBILL  ",0)  ! Get energy bill switch value
          STIMULUS = RTOVALUE("STIMULUS  ",0)! Get stimulus package switch value arra09

      ! Initialize
        EF1=0.5; EF2=0.35; EF3=0.15
      DO 100 IREG= 1, MNUMCR-2
        DO 100 IFUEL= 1, CMnumMajFl+CMnumMinFl+CMnumRenew
         CMFinalUnbenchUse (IFUEL,IREG,CURIYR)= 0.0
         CMFinalEndUse (IFUEL,IREG,CURIYR)= 0.0
             SolarPVContrib (CURIYR, IREG)= 0.0
         DO 100 IBLDTP= 1, CMnumBldg
          UnBenchedCon (IFUEL,IBLDTP,IREG,CURIYR) = 0.0
100       FinalEndUseCon (IFUEL,IBLDTP,IREG,CURIYR)= 0.0

       IF (CURIYR.EQ.CMFirstYr .AND. CURITR.EQ.1) THEN
        DO 250 y= 1, MSEDYR  ! Get historical SEDS data
        DO 250 IREG= 1, MNUMCR-2
         CMSEDS (1,IREG,y)= QSELCM (IREG,y)
         CMSEDS (2,IREG,y)= QSNGCM (IREG,y)
         CMSEDS (3,IREG,y)= QSDSCM (IREG,y)
         CMSEDS (4,IREG,y)= QSRSCM (IREG,y)
         CMSEDS (5,IREG,y)= QSLGCM (IREG,y)
         CMSEDS (6,IREG,y)= QSCLCM (IREG,y)
         CMSEDS (7,IREG,y)= QSMGCM (IREG,y)
         CMSEDS (8,IREG,y)= QSKSCM (IREG,y)
250    CONTINUE
!
!  Get STEO data from file:  
!   Initialize array in case data set is incomplete
!
      DO f= 1, CMnumMajFl+CMnumMinFl
       DO y= MSEDYR+1, KSTEOYR
            STEOdat (f,y-MSEDYR) = 0.0         ! Initialize array to read STEO estimates. 
            DO r= 1, MNUMCR-2
         CMSEDS (f,r,y) = 0.0
        END DO ! y
       END DO ! r
      END DO  ! f

      infile= FILE_MGR ('O','KSTEO',.FALSE.)
      READ (infile,'(99(/))') ! skip header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KSTEO data set error trapping:'
!  read flags that indicate whether regional STEO estimates are available for each fuel
      NumErr = 0
      READ (infile,*,ERR=281,END=253,IOSTAT=IOS) &
           (FlagRegSTEO (f), f= 1, CMnumMajFl+CMnumMinFl) ! regsteo
        GO TO 282  !regsteo
!
 281      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KSTEO read err',IOS,' on regSTEO flag', &
                            '; skip record and continue read.'
 282    CONTINUE   ! regsteo
      READ (infile,'(//)') !skip three lines after last line read  regsteo
!  read national MER and STEO estimates to use when regional estimates are not available      
      count = 0
      DO 252 f= 1, CMnumMajFl+CMnumMinFl
        count = count + 1
        READ (infile,*,ERR=251,END=253,IOSTAT=IOS) &
             (STEOdat (f,y), y= 1, KSTEOYR-MSEDYR) !  number of years for STEO benchmark
        GO TO 252
!
 251      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KSTEO read err',IOS,' on national record', &
                            count,'; skip record and continue read.'
 252    CONTINUE
      READ (infile,'(/)') !skip two lines after last line read  regsteo
! read STEO regional estimates   regsteo
      count = 0
      DO 284 f= 1, CMnumMajFl+CMnumMinFl
       DO 284 r= 1, MNUMCR-2
        count = count + 1
        READ (infile,*,ERR=283,END=253,IOSTAT=IOS) &
             (CMSEDS (f,r,y), y= MSEDYR+1, KSTEOYR) !  number of years for STEO benchmark
        GO TO 284
!
 283      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KSTEO read err',IOS,' on regional record', &
                            count,'; skip record and continue read.'
 284    CONTINUE
 !  end regsteo read block
        GO TO 254  ! EOF reached in KSTEO when expected
 253    CONTINUE   ! EOF reached in KSTEO prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KSTEO EOF reached prematurely at f =' &
                         ,f,' r =',r
 254    CONTINUE
        infile = FILE_MGR ('C','KSTEO',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,255)
255       FORMAT(/,' KSTEO data set error trapping complete!',/)
         ENDIF

! STEOread-avg
!  Replace ksteo inputs with values for STEO variables from common steoblock - consistent 
!    with MER through latest historical year and convert to trillion Btu. STEO does not 
!    have sector-level liquids variables/coal. MER values for these fuels still need to be input - 
!    currently in ksteo.txt. Regional natural gas values in STEO may also be problematic.

        DO y= MSEDYR+1, KSTEOYR  ! Get MER/STEO data from common block ! STEOread-avg
      !Electricity - convert from bkWh to trills
         STEOdat(1,y-MSEDYR)= EXCCPUS(y) * 3.412
        ! Get regional electricity values
         CMSEDS (1,1,y)= EXCCP_NEC(y) * 3.412
         CMSEDS (1,2,y)= EXCCP_MAC(y) * 3.412
         CMSEDS (1,3,y)= EXCCP_ENC(y) * 3.412
         CMSEDS (1,4,y)= EXCCP_WNC(y) * 3.412
         CMSEDS (1,5,y)= EXCCP_SAC(y) * 3.412
         CMSEDS (1,6,y)= EXCCP_ESC(y) * 3.412
         CMSEDS (1,7,y)= EXCCP_WSC(y) * 3.412
         CMSEDS (1,8,y)= EXCCP_MTN(y) * 3.412
         CMSEDS (1,9,y)= (EXCCP_PAC(y)+EXCCP_HAK(y)) * 3.412

      !Natural gas
         STEOdat(2,y-MSEDYR)= NGCCBUS(y) * 1000. ! U.S. natural gas consumption in quads to trills
        ! Get regional natural gas values for potential use
         CMSEDS (2,1,y)= NGCCP_NEC(y) / (NGCCP_NEC(y)+NGCCP_MAC(y)+NGCCP_ENC(y)+NGCCP_WNC(y)+NGCCP_SAC(y)+NGCCP_ESC(y)+NGCCP_WSC(y)+NGCCP_MTN(y)+NGCCP_PAC(y)) * NGCCBUS(y) * 1000.
         CMSEDS (2,2,y)= NGCCP_MAC(y) / (NGCCP_NEC(y)+NGCCP_MAC(y)+NGCCP_ENC(y)+NGCCP_WNC(y)+NGCCP_SAC(y)+NGCCP_ESC(y)+NGCCP_WSC(y)+NGCCP_MTN(y)+NGCCP_PAC(y)) * NGCCBUS(y) * 1000.
         CMSEDS (2,3,y)= NGCCP_ENC(y) / (NGCCP_NEC(y)+NGCCP_MAC(y)+NGCCP_ENC(y)+NGCCP_WNC(y)+NGCCP_SAC(y)+NGCCP_ESC(y)+NGCCP_WSC(y)+NGCCP_MTN(y)+NGCCP_PAC(y)) * NGCCBUS(y) * 1000.
         CMSEDS (2,4,y)= NGCCP_WNC(y) / (NGCCP_NEC(y)+NGCCP_MAC(y)+NGCCP_ENC(y)+NGCCP_WNC(y)+NGCCP_SAC(y)+NGCCP_ESC(y)+NGCCP_WSC(y)+NGCCP_MTN(y)+NGCCP_PAC(y)) * NGCCBUS(y) * 1000.
         CMSEDS (2,5,y)= NGCCP_SAC(y) / (NGCCP_NEC(y)+NGCCP_MAC(y)+NGCCP_ENC(y)+NGCCP_WNC(y)+NGCCP_SAC(y)+NGCCP_ESC(y)+NGCCP_WSC(y)+NGCCP_MTN(y)+NGCCP_PAC(y)) * NGCCBUS(y) * 1000.
         CMSEDS (2,6,y)= NGCCP_ESC(y) / (NGCCP_NEC(y)+NGCCP_MAC(y)+NGCCP_ENC(y)+NGCCP_WNC(y)+NGCCP_SAC(y)+NGCCP_ESC(y)+NGCCP_WSC(y)+NGCCP_MTN(y)+NGCCP_PAC(y)) * NGCCBUS(y) * 1000.
         CMSEDS (2,7,y)= NGCCP_WSC(y) / (NGCCP_NEC(y)+NGCCP_MAC(y)+NGCCP_ENC(y)+NGCCP_WNC(y)+NGCCP_SAC(y)+NGCCP_ESC(y)+NGCCP_WSC(y)+NGCCP_MTN(y)+NGCCP_PAC(y)) * NGCCBUS(y) * 1000.
         CMSEDS (2,8,y)= NGCCP_MTN(y) / (NGCCP_NEC(y)+NGCCP_MAC(y)+NGCCP_ENC(y)+NGCCP_WNC(y)+NGCCP_SAC(y)+NGCCP_ESC(y)+NGCCP_WSC(y)+NGCCP_MTN(y)+NGCCP_PAC(y)) * NGCCBUS(y) * 1000.
         CMSEDS (2,9,y)= NGCCP_PAC(y) / (NGCCP_NEC(y)+NGCCP_MAC(y)+NGCCP_ENC(y)+NGCCP_WNC(y)+NGCCP_SAC(y)+NGCCP_ESC(y)+NGCCP_WSC(y)+NGCCP_MTN(y)+NGCCP_PAC(y)) * NGCCBUS(y) * 1000.

       END DO ! y ! STEOread-avg


! write ksteo values before obtaining from common block to verify consistency
      WRITE(RCDBG,*) 'f,  r,  y,  MSEDYR+1 ... KSTEOYR'
      DO f= 1, 2
       DO r= 1, MNUMCR-2
         WRITE(RCDBG,*) f,r,y,  &
          (CMSEDS (f,r,y), y= MSEDYR+1, KSTEOYR)
       END DO ! f
       END DO ! r

!
!  Read minor fuel regression parameters from KMINFL:
!   Initialize array in case data set is incomplete
!
      DO r= 1, MNUMCR-2
       DO f= 1, CMnumMinFl
         MinFuelAlpha (r,f) = 1.0   ! EXP(0.)
         MinFuelBeta (r,f)  = 0.0
       END DO ! f
       END DO ! r
     
       
       
 ! Read floor adjustment factors
      count = 0
      NumErr = 0
      infile= FILE_MGR ('O','KMINFL',.FALSE.) ! open
      READ (infile,'(99(/))')  ! skip header
      IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KMINFL data set error trapping:'
      
      READ (infile,*,ERR=981,END=268,IOSTAT=IOS) &                    !minorfuel project  
           (FloorAdj (r), r= 1, MNUMCR-2) ! Floor Adjustment Factors  !minorfuel project
          GO TO 982  !Floor Adjustment Factors                          !minorfuel project    
!
 981      CONTINUE ! Report read error and loop to next read          !minorfuel project 
          NumErr = NumErr + 1                                         !minorfuel project
          IF(PRTDBGK.EQ.1) &                                          !minorfuel project   
            WRITE(RCDBG,*) 'Comm_Kminfl read err',IOS,' on Floor Adjustment Factors', &  !minorfuel project
                            '; skip record and continue read.'                           !minorfuel project   
 982    CONTINUE   ! Floor Adjustment Factor                                             !minorfuel project
      READ (infile,'(//)') !skip three lines after last line read     !minorfuel project
      
    !  Read minor fuel regression parameters from KMINFL
           
      DO 267 r= 1, MNUMCR-2
       DO 267 f= 1, CMnumMinFl
        count = count + 1
        READ (infile,*,ERR=266,END=268,IOSTAT=IOS) &
           MinFuelAlpha (r,f), MinFuelBeta (r,f)
        GO TO 267
!
 266      CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KMINFL read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 267    CONTINUE
        GO TO 269  ! EOF reached in KMINFL when expected
 268    CONTINUE   ! EOF reached in KMINFL prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KMINFL EOF reached prematurely at r =' &
                         ,r,' f =',f
 269    CONTINUE
        infile = FILE_MGR ('C','KMINFL',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,270)
270       FORMAT(/,' KMINFL data set error trapping complete!',/)
         ENDIF


       END IF  ! first-pass initialization


! MAJOR FUEL CONSUMPTION:
! Compute end use consumption by region, building, service, and fuel
      DO 105 IREG= 1, MNUMCR-2

         ! Fuel specific price differences are computed relative to          ebe
         ! the base year for the current census division.  This is           ebe
         ! accomplished in preparation of an adjustment to consumption       ebe
         ! to account for the effects of changing fuel prices.               ebe
                                                                        !    ebe
        elasbaseyr = CBECSyear - (BASEYR-1)                             !    ebe
                                                                        !    ebe
          ! 1) Electricity-moved inside service loop for end-use pricing     eup
                                                                        !    ebe
          ! 2) Natural Gas                                                   ebe
          numerator= PNGCM (IREG,CURIYR) - PNGCM (IREG,elasbaseyr)      !    ebe
          denominator=         PNGCM (IREG,elasbaseyr)                  !    ebe
          IF (denominator .GT. 0.0) THEN  ! for legitimate prior price       ebe
           PriceDelta (2) = numerator / denominator  ! 2 -> NG               ebe
          ELSE  ! if price is degenerate, ignore elasticity                  ebe
           PriceDelta (2)= 0.0                                          !    ebe
          END IF ! price legitimacy test                                     ebe
                                                                        !    ebe
          ! 3) Distillate                                                    ebe
          numerator= PDSCM (IREG,CURIYR) - PDSCM (IREG,elasbaseyr)      !    ebe
          denominator=         PDSCM (IREG,elasbaseyr)                  !    ebe
          IF (denominator .GT. 0.0) THEN  ! for legitimate prior price       ebe
           PriceDelta (3) = numerator / denominator  ! 3 -> DS               ebe
          ELSE  ! if price is degenerate, ignore elasticity                  ebe
           PriceDelta (3)= 0.0                                          !    ebe
          END IF ! price legitimacy test                                     ebe
                                                                        !    ebe
        DO 105 IFUEL= 1, CMnumMajFl
        
          ! For smart grid provisions in Feb 2009 stimulus package,       arra09
          ! make adjustment to short run price elasticity of              arra09
          ! demand - clear signals based on smart meters, potential       arra09
          ! demand control of equipment.                                  arra09
          
         IF (STIMULUS .GT. 0) THEN                                      ! arra09
           ! Implement effect of smart grid on demand price elasticity    arra09
          IF (CURIYR .EQ. 21 .AND. CURITR .EQ. 1) THEN                  ! arra09 - change elasticity for 2010 on
               ShortRunPriceElasofDmd(IREG,1,IFUEL) = -0.30             ! space heating  arra09
               ShortRunPriceElasofDmd(IREG,2,IFUEL) = -0.30             ! space cooling  arra09
               ShortRunPriceElasofDmd(IREG,3,IFUEL) = -0.27             ! water heating  arra09
               ShortRunPriceElasofDmd(IREG,4,IFUEL) = -0.30             ! ventilation    arra09
               ShortRunPriceElasofDmd(IREG,6,IFUEL) = -0.30             ! lighting       arra09
               ShortRunPriceElasofDmd(IREG,10,1) = -0.10                ! other-plugload arra09
          END IF ! test for 2010, first iteration                         arra09 
         END IF ! test for no stimulus run                                arra09
          
         DO 105 IBLDTP= 1, CMnumBldg
          DO  105 ISERV= 1,CMnumServ

          ! Initialize
          EndUseConsump (IFUEL,ISERV,IBLDTP,IREG,CURIYR)= 0.0
          EUCONwithDG (IFUEL,ISERV,IBLDTP,IREG,CURIYR)= 0.0            !  PVdispatch
                                                                       !    eup
          ! 1) Electricity - with end-use prices - price diff. to base year eup
          numerator= PELCMOUT (IREG,CURIYR,ISERV)     &                !    eup
            - PELCMOUT (IREG,elasbaseyr,ISERV)                         !    eup
          denominator=  PELCMOUT (IREG,elasbaseyr,ISERV)               !    eup
          IF (denominator .GT. 0.0) THEN  ! for legitimate prior price      eup
           PriceDelta (1) = numerator / denominator  ! 1 -> EL              eup
          ELSE  ! if price is degenerate, ignore elasticity                 eup
           PriceDelta (1)= 0.0                                         !    eup
          END IF ! price legitimacy test                                    eup
                                                                       !    eup

        CMEffInd(IFUEL,ISERV,IBLDTP,IREG,CURIYR) =  1.                 !Efficiency Index

      IF (AverageEfficiency (IREG,IBLDTP,ISERV,IFUEL) .GT. 0.00) THEN

        EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR)= &
         FuelShareofService (IREG,IBLDTP,ISERV,IFUEL) / &
         AverageEfficiency (IREG,IBLDTP,ISERV,IFUEL)  * &
         (    NewServDmd (ISERV,IBLDTP,IREG,CURIYR) + &
           RetireServDmd (ISERV,IBLDTP,IREG,CURIYR) + &
             ServDmdSurv (ISERV,IBLDTP,IREG,CURIYR)   )

      IF (PRTDBGK.eq.1 .and. ISERV.eq.10 .and. IFUEL.eq.1 ) THEN        !misctest
       IF ( IBLDTP.eq.1 .and. IREG.eq.1 ) THEN                          !misctest
        WRITE (RCDBG,*) ' year ',CURIYR+1989, ' iteration ', CURITR,  & !misctest
         ' Initial EndUseConsump calcs for BT 1 in CD 1'                !misctest
        write(rcdbg,*) "ServDmdSurv ", ServDmdSurv (ISERV,IBLDTP,IREG,CURIYR)  & !misctestt
         , "NewServDmd ", NewServDmd (ISERV,IBLDTP,IREG,CURIYR)         !misctest
        write(rcdbg,*) "FS ", FuelShareofService (IREG,IBLDTP,ISERV,IFUEL)       !misctest
        write(rcdbg,*) "EndUseQ ", EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) !misctest
       END IF                                                           !misctest
      END IF                                                            !misctest

        IF(SDbyFuel(IFUEL,ISERV,IBLDTP,IREG,CURIYR).GT.0. .and. &      !Efficiency Index
           EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR).gt.0.) &      !Efficiency Index
          CMEffInd(IFUEL,ISERV,IBLDTP,IREG,CURIYR) =       &           !Efficiency Index
            EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) /&           !Efficiency Index
            SDbyFuel(IFUEL,ISERV,IBLDTP,IREG,CURIYR)                   !Efficiency Index

!        Adjustment to consumption to account for response to changes in    ebe
!        fuel prices and the rebound effect.  As equipment and building     ebe
!        shells become more efficient, the marginal cost of service         ebe
!        provision declines.  This section of code applies elasticities to  ebe
!        reflect consumers' response to fuel prices (as price increases,    ebe
!        consumption decreases and vice versa) and to reflect an increase   ebe
!        in consumption due to lower marginal cost of service.              ebe
!                                                                           ebe
       ! consider elasticity effects after 1990 for services that are       ebe
       ! affected by elasticities.                                          ebe
                                                                         !  ebe
       IF (ISERV .LE. CMnumSDElas .AND. &
        EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR).GT. 0.0) THEN      !  qtest
        SRElas = ShortRunPriceElasofDmd(IREG,ISERV,IFUEL)                !  kelast
!         PriceDelta(IFUEL)                                              !  noneg
!        IF (ABS(SRElasAmt).GT. 0.5) THEN                                !  noneg
!         IF (SRElasAmt .GT. 0.0) SRElasAmt = 0.5                        !  noneg
!         IF (SRElasAmt .LT. 0.0) SRElasAmt = -0.5                       !  noneg
!          IF (PRTDBGK.EQ.1) THEN                                        !  noneg
!          WRITE(RCDBG,*) "Short term price elasticity effect too &
!            large, capped at ",SRElasAmt," of current consumption"      !  noneg
!           WRITE(RCDBG,*) "Year ",CURIYR,"Itr ",CURITR,               & !  noneg
!             "CD ",IREG," End-Use ",ISERV," Fuel ",IFUEL                !  noneg
!          END IF ! Write to KDEBUG when cap is hit.                        noneg
!         END IF  ! Cap elasticity effect at 50 percent of current consumption

       EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR)=               &    ! Price  ebe
        EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) *             &    ! ebe
      KELAST(IFUEL,IREG,ISERV,SRelas,EF1,EF2,EF3,elasbaseyr,rcdbg)       ! kelast

      IF (ISERV .LE. 6) THEN                                             !  carbon
                                                                         !  ebe
       IF (AverageEfficiencyBASE(IREG,IBLDTP,ISERV,IFUEL).GT.0.0 &
          .AND. CURIYR .GT. CMFirstYr) THEN                             ! Rbndfix
        ReboundEquip = -0.15 *                                        & !  noneg
         (1. - AverageEfficiency(IREG,IBLDTP,ISERV,IFUEL) /           & !  noneg
         AverageEfficiencyBASE (IREG,IBLDTP,ISERV,IFUEL) )              !  noneg
        IF (ABS(ReboundEquip) .GT. 0.25) THEN                           !  noneg
         IF (ReboundEquip .GT. 0.0) ReboundEquip = 0.25                 !  noneg
         IF (ReboundEquip .LT. 0.0) ReboundEquip = -0.25                !  noneg
          IF (PRTDBGK.EQ.1) THEN                                        !  noneg
          WRITE(RCDBG,*) "Rebound effect too large, capped at ",      & !  noneg
            ReboundEquip," of current consumption"                      !  noneg
           WRITE(RCDBG,*) "Year ",CURIYR,"Itr ",CURITR,               & !  noneg
          "CD ",IREG," End-Use ",ISERV," Fuel ",IFUEL," BT ",IBLDTP     !  noneg
          END IF ! Write to KDEBUG when cap is hit.                        noneg
         END IF  ! Cap rebound effect at 25 percent of current consumption

       EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR)=               &   ! Equip  ebe
        EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) *               & !    ebe
        ( 1. + ReboundEquip )                                           !  noneg
       END IF ! AverageEfficiencyBASE>0
      END IF ! No rebound for refrig, off. equip. and other                carbon
        IF (ISERV .EQ. 1 )                                            & ! Building shell rebound  ebe
        EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR)=                   & !    ebe
         EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) *                 & !    ebe
         ( 1. + ( -0.15 * (ShellHeatFactor(IBLDTP,IREG,1,curiyr) -1.) ))            ! endshel-shlfactor
        IF (ISERV .EQ. 2 )                                            & ! Building shell rebound  ebe
        EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR)=                   & !    ebe
         EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) *                 & !    ebe
         ( 1. + ( -0.15 * (ShellCoolFactor(IBLDTP,IREG,1,curiyr) -1.) ))            ! endshel-shlfactor


                                                                        !    ebe
       END IF     ! rebound elasticity consideration past first model year   Rbndfix
       END IF     ! consumption consideration for efficiency > 0             ebe



      ! For non-lighting services, the calculation above yields
      ! consumption in Trills; however, lighting service demand
      ! is in units of 10**9 lumen yrs, and lighting efficacy
      ! with units of lumens/watt is used instead of efficiency.
      ! To convert the ratio of lighting service demand over
      ! efficacy to Trills, a conversion factor of 1/.03343 must
      ! be applied, based on 1 lumen/watt = 3.343*10**7 lumen yr
      ! per Trill:

      IF (ISERV .EQ. 6) EndUseConsump (IFUEL,ISERV,IBLDTP,IREG,CURIYR)= &
        EndUseConsump (IFUEL,ISERV,IBLDTP,IREG,CURIYR) / 0.03343

      ! Perform weather correction for heating and cooling:
      y= CURIYR ! for notational brevity, use single char subscripts:
      b= IBLDTP
      f= IFUEL
      r= IREG
      s= ISERV
      IF (s.EQ.1 .OR. s.EQ.2) THEN    ! s=1 -> heating, 2->cooling
       IF(DegreeDays(s,r,CBECSyear-(BASEYR-1)).GT.0.0) THEN ! if denom. nonzero
         IF (s.EQ.1) EndUseConsump (f,s,b,r,y)= EndUseConsump (f,s,b,r,y) * &
          DegreeDays (s,r,y) / DegreeDays (s,r,CBECSyear-(BASEYR-1))
         IF (s.EQ.2) EndUseConsump (f,s,b,r,y)= EndUseConsump (f,s,b,r,y) * &
          (DegreeDays (s,r,y) / DegreeDays (s,r,CBECSyear-(BASEYR-1)))**1.10  
       END IF  ! avoidance of division by zero
      END IF   ! weather correction to heating and cooling


105   CONTINUE
!    Adjustments to End use consumption for Distributed Generation
!     
!  Distributed Generation Section
!    Solar PV is assumed to be used to offset air conditioning, ventilation,
!          lighting, and other electricity consumption.
!    Generation by other distributed generation resources is assumed to be 
!          used to offset electricity consumption for end uses, based on
!          end-use share of total electricity use.
!    Credit for space and water heating provided is netted out of fuel
!          consumption for those services.  The netted credit is given
!          to the fuel used by the distributed generation resource, i.e., 
!          natural gas space and water heating consumption is credited for
!          gas-fired technologies.
!
!   Fuel consumption by the distibuted generation technology is added to
!          other. At some point, may want to break fuel usage into components
!          for specific end uses - but accounting is not currently in model
!          for this.

       DO 106 IBLDTP= 1, CmnumBldg                                   !distgen
       DO 106 IREG= 1, MNUMCR-2                                      !distgen
!                                                                    !distgen
!  Compute Building/Region Specific Shares of Elec for End-Use Adjsts!distgen
       xtot=0.                                                       !distgen

       Do 107 ISERV=1,10                                             !distgen
       xtot=xtot+                                                  & !distgen
          EndUseConsump(1,ISERV,IBLDTP,IREG,CURIYR)                  !distgen
       xElShr(ISERV)=0.                                              !distgen
 107   Continue                                                      !distgen
                                                                     !distgen
       Do 108 ISERV=1,10                                             !distgen
       IF(xtot.gt.0.)                                              & !distgen
        xElShr(ISERV)=EndUseConsump(1,ISERV,IBLDTP,IREG,CURIYR)/xtot !distgen
 108   Continue                                                      !distgen

!Add up this year's generation for own-use (excluding PV, NTEK 1, which is handled below)
            TrillsOwnUseAdj =                                      & !distgen
              TrillsOwnUse(curiyr,ireg,ibldtp,2)                   & !distgen
             +TrillsOwnUse(curiyr,ireg,ibldtp,3)                   & !distgen
             +TrillsOwnUse(curiyr,ireg,ibldtp,4)                   & !distgen
             +TrillsOwnUse(curiyr,ireg,ibldtp,5)                   & !distgen
             +TrillsOwnUse(curiyr,ireg,ibldtp,6)                   & !distgen
             +TrillsOwnUse(curiyr,ireg,ibldtp,7)                   & !distgen
             +TrillsOwnUse(curiyr,ireg,ibldtp,8)                   & !distgen
             +TrillsOwnUse(curiyr,ireg,ibldtp,9)                   & !distgen
             +TrillsOwnUse(curiyr,ireg,ibldtp,10)                  & !distgen
             +TrillsOwnUse(curiyr,ireg,ibldtp,11)                    !distgen
  
!               (fuel=1, electricity)                                !distgen
!        deduct electricity generation using calculated share        !distgen
!         skew PV towards specific end-use loads                     !distgen
          Unadjusted=EndUseConsump(1,1,IBLDTP,IREG,CURIYR)                        !Efficiency Index
          EndUseConsump(1,1,IBLDTP,IREG,CURIYR) =                  & !distgen
             EndUseConsump(1,1,IBLDTP,IREG,CURIYR)                 & !distgen
             -TrillsOwnUseAdj*xElShr(1)                              !distgen
           IF(EndUseConsump(1,1,IBLDTP,IREG,CURIYR).gt.0.)            &           !Efficiency Index 
           CMEffInd(1,1,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
           CMEffInd(1,1,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
           Unadjusted / EndUseConsump(1,1,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          Unadjusted=EndUseConsump(1,2,IBLDTP,IREG,CURIYR)                        !Efficiency Index
          EndUseConsump(1,2,IBLDTP,IREG,CURIYR) =                   & !distgen
             EndUseConsump(1,2,IBLDTP,IREG,CURIYR)                  & !distgen
             -(TrillsOwnUse(curiyr,ireg,ibldtp,1)-                  & !distgen
               TrillsOwnUse(CMFirstYr,ireg,ibldtp,1))*.12           & !distgen - !PVshare
             - TrillsOwnUseAdj*xElShr(2)                              !distgen
           IF(EndUseConsump(1,2,IBLDTP,IREG,CURIYR).gt.0.)            &           !Efficiency Index 
           CMEffInd(1,2,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
           CMEffInd(1,2,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
           Unadjusted / EndUseConsump(1,2,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          Unadjusted=EndUseConsump(1,3,IBLDTP,IREG,CURIYR)                        !Efficiency Index
          EndUseConsump(1,3,IBLDTP,IREG,CURIYR) =                   & !distgen
             EndUseConsump(1,3,IBLDTP,IREG,CURIYR)                  & !distgen
             - TrillsOwnUseAdj*xElShr(3)                              !distgen
           IF(EndUseConsump(1,3,IBLDTP,IREG,CURIYR).gt.0.)            &           !Efficiency Index 
           CMEffInd(1,3,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
           CMEffInd(1,3,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
           Unadjusted / EndUseConsump(1,3,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          Unadjusted=EndUseConsump(1,4,IBLDTP,IREG,CURIYR)                       !Efficiency Index
          EndUseConsump(1,4,IBLDTP,IREG,CURIYR) =                   & !distgen
             EndUseConsump(1,4,IBLDTP,IREG,CURIYR)                  & !distgen
             -(TrillsOwnUse(curiyr,ireg,ibldtp,1)-                  & !distgen
               TrillsOwnUse(CMFirstYr,ireg,ibldtp,1))*.13           & !distgen - !PVshare
             - TrillsOwnUseAdj*xElShr(4)                              !distgen
           IF(EndUseConsump(1,4,IBLDTP,IREG,CURIYR).gt.0.)            &           !Efficiency Index 
           CMEffInd(1,4,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
           CMEffInd(1,4,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
           Unadjusted / EndUseConsump(1,4,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          Unadjusted=EndUseConsump(1,5,IBLDTP,IREG,CURIYR)                        !Efficiency Index
          EndUseConsump(1,5,IBLDTP,IREG,CURIYR) =                   & !distgen
             EndUseConsump(1,5,IBLDTP,IREG,CURIYR)                  & !distgen
             - TrillsOwnUseAdj*xElShr(5)                              !distgen
           IF(EndUseConsump(1,5,IBLDTP,IREG,CURIYR).gt.0.)            &           !Efficiency Index 
           CMEffInd(1,5,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
           CMEffInd(1,5,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
           Unadjusted / EndUseConsump(1,5,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          Unadjusted=EndUseConsump(1,6,IBLDTP,IREG,CURIYR)                        !Efficiency Index
          EndUseConsump(1,6,IBLDTP,IREG,CURIYR) =                   & !distgen
             EndUseConsump(1,6,IBLDTP,IREG,CURIYR)                  & !distgen
             -(TrillsOwnUse(curiyr,ireg,ibldtp,1)-                  & !distgen
               TrillsOwnUse(CMFirstYr,ireg,ibldtp,1))*.16           & !distgen - !PVshare
             - TrillsOwnUseAdj*xElShr(6)                              !distgen
           IF(EndUseConsump(1,6,IBLDTP,IREG,CURIYR).gt.0.)            &           !Efficiency Index 
           CMEffInd(1,6,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
           CMEffInd(1,6,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
           Unadjusted / EndUseConsump(1,6,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          Unadjusted=EndUseConsump(1,7,IBLDTP,IREG,CURIYR)                        !Efficiency Index
          EndUseConsump(1,7,IBLDTP,IREG,CURIYR) =                   & !distgen
             EndUseConsump(1,7,IBLDTP,IREG,CURIYR)                  & !distgen
             - TrillsOwnUseAdj*xElShr(7)                              !distgen
           IF(EndUseConsump(1,7,IBLDTP,IREG,CURIYR).gt.0.)            &           !Efficiency Index 
           CMEffInd(1,7,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
           CMEffInd(1,7,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
           Unadjusted / EndUseConsump(1,7,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          EndUseConsump(1,8,IBLDTP,IREG,CURIYR) =                   & !distgen
             EndUseConsump(1,8,IBLDTP,IREG,CURIYR)                  & !distgen
             - TrillsOwnUseAdj*xElShr(8)                              !distgen

          EndUseConsump(1,9,IBLDTP,IREG,CURIYR) =                   & !distgen
             EndUseConsump(1,9,IBLDTP,IREG,CURIYR)                  & !distgen
             - TrillsOwnUseAdj*xElShr(9)                              !distgen

          EndUseConsump(1,10,IBLDTP,IREG,CURIYR) =                  & !distgen
             EndUseConsump(1,10,IBLDTP,IREG,CURIYR)                 & !distgen
             -(TrillsOwnUse(curiyr,ireg,ibldtp,1)-                  & !distgen
               TrillsOwnUse(CMFirstYr,ireg,ibldtp,1))*.59           & !distgen - !PVshare
             - TrillsOwnUseAdj*xElShr(10)                             !distgen

!ADD BACK PV GENERATION FOR TOTAL ELECTRICITY LOAD FOR USE BY EMM     !PVdispatch
!Loads not affected by PV generation = EndUseConsump     !PVdispatch
          EUCONwithDG (1,1,IBLDTP,IREG,CURIYR) =                    & !PVdispatch
             EndUseConsump(1,1,IBLDTP,IREG,CURIYR)                    !distgen
          EUCONwithDG (1,2,IBLDTP,IREG,CURIYR) =                    & !PVdispatch
             EndUseConsump(1,2,IBLDTP,IREG,CURIYR)                  & !distgen
             +(TrillsOwnUse(curiyr,ireg,ibldtp,1)-                  & !PVdispatch
               TrillsOwnUse(CMFirstYr,ireg,ibldtp,1))*.12             !PVdispatch
          EUCONwithDG (1,3,IBLDTP,IREG,CURIYR) =                    & !PVdispatch
             EndUseConsump(1,3,IBLDTP,IREG,CURIYR)                    !distgen
          EUCONwithDG (1,4,IBLDTP,IREG,CURIYR) =                    & !PVdispatch
             EndUseConsump(1,4,IBLDTP,IREG,CURIYR)                  & !distgen
             +(TrillsOwnUse(curiyr,ireg,ibldtp,1)-                  & !PVdispatch
               TrillsOwnUse(CMFirstYr,ireg,ibldtp,1))*.13             !PVdispatch
          EUCONwithDG (1,5,IBLDTP,IREG,CURIYR) =                    & !PVdispatch
             EndUseConsump(1,5,IBLDTP,IREG,CURIYR)                    !distgen
          EUCONwithDG (1,6,IBLDTP,IREG,CURIYR) =                    & !PVdispatch
             EndUseConsump(1,6,IBLDTP,IREG,CURIYR)                  & !distgen
             +(TrillsOwnUse(curiyr,ireg,ibldtp,1)-                  & !PVdispatch
               TrillsOwnUse(CMFirstYr,ireg,ibldtp,1))*.16             !PVdispatch
          EUCONwithDG (1,7,IBLDTP,IREG,CURIYR) =                    & !PVdispatch
             EndUseConsump(1,7,IBLDTP,IREG,CURIYR)                    !distgen
          EUCONwithDG (1,8,IBLDTP,IREG,CURIYR) =                    & !PVdispatch
             EndUseConsump(1,8,IBLDTP,IREG,CURIYR)                    !distgen
          EUCONwithDG (1,9,IBLDTP,IREG,CURIYR) =                    & !PVdispatch
             EndUseConsump(1,9,IBLDTP,IREG,CURIYR)                    !distgen
          EUCONwithDG (1,10,IBLDTP,IREG,CURIYR) =                   & !PVdispatch
             EndUseConsump(1,10,IBLDTP,IREG,CURIYR)                 & !distgen
             +(TrillsOwnUse(curiyr,ireg,ibldtp,1)-                  & !PVdispatch
               TrillsOwnUse(CMFirstYr,ireg,ibldtp,1))*.59             !PVdispatch

       IF(curiyr.eq.27 .and. ibldtp.eq.7 .and. ireg.eq.9) &           !PVdispatch
         write(rcdbg,*) 'y,f,s,EUCon,EUConwithDG ',curiyr,ifuel,   &  !PVdispatch
         EndUseConsump(1,2,IBLDTP,IREG,CURIYR),                &  !PVdispatch
         EUConwithDG(1,2,IBLDTP,IREG,CURIYR),                  &  !PVdispatch
         (TrillsOwnUse(curiyr,ireg,ibldtp,1)-                  & !PVdispatch
               TrillsOwnUse(CMFirstYr,ireg,ibldtp,1))*.12             !PVdispatch 


!      Natural Gas                                                 !distgen
!               (fuel =2, ntek=2,3,7 and 8                         !distgen
!                ntek2=fuel cells, 3= engine 7=turbine 8=micro     !distgen
!           Add up this year's net end use adjustments and net fuel usage
!             Gas Space Heating
              SHBtuAdj =                                        &   
               shbtu(curiyr,ireg,ibldtp,2) & !distgen
              +shbtu(curiyr,ireg,ibldtp,3) & !distgen
              +shbtu(curiyr,ireg,ibldtp,7) & !distgen
              +shbtu(curiyr,ireg,ibldtp,8)   !distgen
!             Gas Water Heating
              HWBtuAdj =                                        &   
               hwbtu(curiyr,ireg,ibldtp,2) & !distgen
              +hwbtu(curiyr,ireg,ibldtp,3) & !distgen
              +hwbtu(curiyr,ireg,ibldtp,7) & !distgen
              +hwbtu(curiyr,ireg,ibldtp,8)   !distgen
!             Gas Fuel Usage
              FuelUsageAdj =                                        &   
               fuelusage(curiyr,ireg,ibldtp,2) & !distgen
              +fuelusage(curiyr,ireg,ibldtp,3) & !distgen
              +fuelusage(curiyr,ireg,ibldtp,7) & !distgen
              +fuelusage(curiyr,ireg,ibldtp,8)   !distgen

          Unadjusted=EndUseConsump(2,1,IBLDTP,IREG,CURIYR)                        !Efficiency Index
          EndUseConsump(2,1,IBLDTP,IREG,CURIYR) =                & !distgen
             EndUseConsump(2,1,IBLDTP,IREG,CURIYR)-SHBtuAdj
           IF(EndUseConsump(2,1,IBLDTP,IREG,CURIYR).gt.0.)            &           !Efficiency Index 
           CMEffInd(2,1,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
           CMEffInd(2,1,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
           Unadjusted / EndUseConsump(2,1,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          Unadjusted=EndUseConsump(2,3,IBLDTP,IREG,CURIYR)                        !Efficiency Index
          EndUseConsump(2,3,IBLDTP,IREG,CURIYR) =                & !distgen
             EndUseConsump(2,3,IBLDTP,IREG,CURIYR)-HWBtuAdj        !distgen
           IF(EndUseConsump(2,3,IBLDTP,IREG,CURIYR).gt.0.)            &           !Efficiency Index 
           CMEffInd(2,3,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
           CMEffInd(2,3,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
           Unadjusted / EndUseConsump(2,3,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          EndUseConsump(2,10,IBLDTP,IREG,CURIYR) =               & !distgen
             EndUseConsump(2,10,IBLDTP,IREG,CURIYR)+FuelUsageAdj

!        Distillate Oil consumption adjustments
!               (fuel =3, service=10, ntek=5)                      !distgen
!           Add up this year's net end use adjustments and net fuel usage
!             Distillate Space Heating
              SHBtuAdj = shbtu(curiyr,ireg,ibldtp,5) !distgen
!             Distillate Water Heating
              HWBtuAdj = hwbtu(curiyr,ireg,ibldtp,5) !distgen
!             Gas Fuel Usage
              FuelUsageAdj = fuelusage(curiyr,ireg,ibldtp,5)

!       Need to rule out potential negatives -- caused by             !distgen
!        disagreement between form 867 data and SEDS/CBECS ests       !distgen
          Unadjusted=EndUseConsump(3,1,IBLDTP,IREG,CURIYR)                          !Efficiency Index
          EndUseConsump(3,1,IBLDTP,IREG,CURIYR) =                & !distgen
             Max(EndUseConsump(3,1,IBLDTP,IREG,CURIYR)           & !distgen
             -shbtu(curiyr,ireg,ibldtp,5),0.)                      !distgen
           IF(EndUseConsump(3,1,IBLDTP,IREG,CURIYR).gt.0.)              &           !Efficiency Index 
             CMEffInd(3,1,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
             CMEffInd(3,1,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
             Unadjusted / EndUseConsump(3,1,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          Unadjusted=EndUseConsump(3,3,IBLDTP,IREG,CURIYR)                          !Efficiency Index
          EndUseConsump(3,3,IBLDTP,IREG,CURIYR) =                & !distgen
             Max(EndUseConsump(3,3,IBLDTP,IREG,CURIYR)           & !distgen
             -hwbtu(curiyr,ireg,ibldtp,5),0.)                      !distgen
           IF(EndUseConsump(3,3,IBLDTP,IREG,CURIYR).gt.0.)              &           !Efficiency Index 
             CMEffInd(3,3,IBLDTP,IREG,CURIYR) =                         &           !Efficiency Index
             CMEffInd(3,3,IBLDTP,IREG,CURIYR) *                         &           !Efficiency Index
             Unadjusted / EndUseConsump(3,3,IBLDTP,IREG,CURIYR)                     !Efficiency Index

          EndUseConsump(3,10,IBLDTP,IREG,CURIYR) =               & !distgen
             EndUseConsump(3,10,IBLDTP,IREG,CURIYR)              & !distgen
             +FuelUsageAdj

!  SAVINGS FOR TRANSFORMER STANDARD NOW INCLUDED BASED ON TIAX REPORT SEE miscdetail CODE
      ! Decrement misc. electricity use to account for distribution transformer standards
      !   in the Energy Policy Act of 2005  ebill

!      IF  (NRGBILL .GT. 0 .AND. CURIYR .GE. 18)  THEN                 !  ebill
!        xstdsavshr = 0.0000010806 * (1+0.090190)**FLOAT(CURIYR-18)     !  ebill
!           EndUseConsump (1,10,IBLDTP,IREG,CURIYR)=                     & !  effects for 2007 transformer stnd ebill
!                 EndUseConsump (1,10,IBLDTP,IREG,CURIYR) * (1 - xstdsavshr)   !  ebill
!      END IF ! ebill distribution transformer standard


 106    continue

!  FUEL CONSUMPTION TO PROVIDE DISTRICT SERVICES:

      y= CURIYR
      DistServSteam (y)= 0.0  ! prepare to accumulate
      DO b= 1, CMnumBldg
       DO s= 1, CMnumDHServ
        DO r= 1, MNUMCR-2

!  ADD FUEL LOOP FOR ELASTICITY ADJUSTMENT TO DISTRICT STEAM

         PrElaAdj = 0.0                                              !mfela
         DO f= 1, CMnumMajFl
               SRElas = ShortRunPriceElasofDmd(r,s,f)
           PrElaAdj = PrElaAdj +                                   & !mfela
            DistServFuelShr(r,f) *                                 & !mfela - ds08
             KELAST(f,r,s,SRElas,EF1,EF2,EF3,elasbaseyr,rcdbg)       !mfela
         END DO  ! f                                                 !mfela

          IF (s .LT. 3) THEN   !check for heating or cooling to apply weather correction
               IF (DegreeDays(s,r,CBECSyear-(BASEYR-1)).GT. 0.0) THEN    !if denom. nonzero
               DistServSteam (y)= DistServSteam (y) + &
              (DistServSteamEUI (r,b,s) * (SurvFloorTotal (r,b,y) + &
              CMNewFloorSpace (r,b,y)) /1000.0     & ! 10**9 Btu -> 10**12 Btu
              * PrElaAdj * DegreeDays(s,r,y) / DegreeDays(s,r,CBECSyear-(BASEYR-1)))              !mfela
               END IF ! avoidance of division by zero

          ELSE    ! no weather correction for water heating
               DistServSteam (y)= DistServSteam (y) + &
            (DistServSteamEUI (r,b,s) * (SurvFloorTotal (r,b,y) + &
            CMNewFloorSpace (r,b,y)) /1000.0     & ! 10**9 Btu -> 10**12 Btu
            * PrElaAdj)                                        !mfela
          END IF  ! weather correction for heating and cooling
         END DO  ! r

        DO f= 1, CMnumMajFl
         DistServConsump (MNUMCR,b,s,f,y)= 0.0  ! prepare to accumulate
         DO r= 1, MNUMCR-2

          IF (s .LT. 3) THEN   !check for heating or cooling to apply weather correction
               IF (DegreeDays(s,r,CBECSyear-(BASEYR-1)).GT. 0.0) THEN    !if denom. nonzero
             DistServConsump (r,b,s,f,y)= DistServSteamEUI (r,b,s) * &
              (SurvFloorTotal (r,b,y) + CMNewFloorSpace (r,b,y)) * &
                DistServFuelShr (r,f) / DistServBoilerEff (f) &                !ds08
                * KELAST(f,r,s,SRElas,EF1,EF2,EF3,elasbaseyr,rcdbg)          & ! mfela
                * DegreeDays(s,r,y) / DegreeDays(s,r,CBECSyear-(BASEYR-1))   & ! weather correction - actual weather data
                      / 1000.0   ! 10**9 Btu -> 10**12 Btu
               END IF ! avoidance of division by zero

          ELSE    ! no weather correction for water heating
           DistServConsump (r,b,s,f,y)= DistServSteamEUI (r,b,s) * &
           (SurvFloorTotal (r,b,y) + CMNewFloorSpace (r,b,y)) * &
             DistServFuelShr (r,f) / DistServBoilerEff (f) &                   !ds08
             * KELAST(f,r,s,SRElas,EF1,EF2,EF3,elasbaseyr,rcdbg)             & !mfela
             / 1000.0   ! 10**9 Btu -> 10**12 Btu
          END IF  ! weather correction for heating and cooling

      ! Accumulate national total
      DistServConsump (MNUMCR,b,s,f,y)= &
         DistServConsump (MNUMCR,b,s,f,y) + DistServConsump (r,b,s,f,y)

      ! Add District Services Consumption to end-use consumption
      EndUseConsump (f,s,b,r,y)= EndUseConsump (f,s,b,r,y) &
        + DistServConsump (r,b,s,f,y)
      ! Add District Services Consumption to total electricity load variables
      IF (f .EQ. 1) THEN   !only have total load filled in for electricity  !PVdispatch
        EUCONwithDG (f,s,b,r,y) = EUCONwithDG (f,s,b,r,y)         & !PVdispatch
          + DistServConsump (r,b,s,f,y)                             !PVdispatch
      END IF  ! add district services to total electricity load variables

         END DO  ! r
        END DO   ! f
       END DO    ! s
      END DO     ! b

!  Aggregate national total across buildings for FTAB:
      DO s= 1, CMnumDHServ
       DO f= 1, CMnumMajFl
        CMUSDistServ (s,f,CURIYR)= 0.0
        DO b= 1, CMnumBldg
         CMUSDistServ (s,f,CURIYR)= CMUSDistServ (s,f,CURIYR) &
           + DistServConsump (MNUMCR,b,s,f,CURIYR) / 1000.0 ! Quads
        END DO ! b
       END DO  ! f
      END DO   ! s

          IF (NRGBILL .GT. 0) THEN  !  check switch for including ebill provisions                                                            ebill
          !  Calculate current year national savings for pre-rinse spray valve standard that goes into effect January 1, 2006                 ebill
           IF (CURIYR .GT. 16 .AND. CURITR .EQ. 1) THEN  ! Check for effective date of spray valve standard, only make calcs first iteration  ebill

            ! Total current year food service, health, and lodging floorspace for later distribution of standard savings                      ebill
            FoodSvcHlthLdgFlrPrev = 0.0   ! initialize previous year affected floorspace                                                      ebill
            IF (CURIYR .GT. 17) FoodSvcHlthLdgFlrPrev = FoodSvcHlthLdgFloorTot   ! capture previous year affected floorspace                  ebill
            FoodSvcHlthLdgFloorTot = 0.0  ! initialize total for current year                                                                 ebill
            DO r = 1, MNUMCR-2                                                                                                              ! ebill
             DO b = 4, 6                  ! only total floorspace for food service(4), health(5) and lodging(6)                               ebill
                  FoodSvcHlthLdgFloorTot = FoodSvcHlthLdgFloorTot +   &                                                                     ! ebill
                   CMNewFloorSpace(r,b,y) + SurvFloorTotal(r,b,y)                                                                           ! ebill
                 END DO ! b                                                                                                                   ebill
            END DO  ! r                                                                                                                       ebill

            ! Select to calculate current year standard savings                                                                               ebill
            valvestd:  select case (CURIYR)   ! calculate savings from spray valve standard 2006 on                                           ebill
            case (17) valvestd  ! initialize savings for current year shipments, first year of standard SprayValve                            ebill
              prsvShpSavings = 11.9599 ! 2006 initialization of potential standard savings from current year shipment (trills)                ebill
              prsvStdSavings = prsvShpSavings  ! Assume savings only from shipments, first year of standard                                 ! ebill
            case (18:21) valvestd  ! phase-in savings to allow for stock turnover (5 yr life, stock at standard level in 5 years)             ebill
              ! Increment savings from current year shipments to account for sector growth                                                    ebill
              prsvShpSavings = prsvShpSavings * (FoodSvcHlthLdgFloorTot/FoodSvcHlthLdgFlrPrev)                                              ! ebill
              ! Calc savings from current year shipments plus previous post-standard shipments                                                ebill
              prsvStdSavings = prsvStdSavings + prsvShpSavings                                                                              ! ebill
            case (22: ) valvestd  ! stock at standard level, maintain savings and increment by sector growth                                  ebill
              prsvStdSavings = prsvStdSavings + prsvShpSavings * (FoodSvcHlthLdgFloorTot/FoodSvcHlthLdgFlrPrev-1.0)                         ! ebill
              ! Calc savings from current year shipments to provide base for next year's sector growth                                        ebill
              prsvShpSavings = prsvShpSavings * (FoodSvcHlthLdgFloorTot/FoodSvcHlthLdgFlrPrev)                                              ! ebill
            end select valvestd   ! calculate savings from spray valve standard 2006 on                                                       ebill

           END IF  ! Check for effective date of 2006 pre-rinse spray valve standard, calcs for out years                                     ebill
          END IF  ! Check switch for including ebill provisions                                                                               ebill

      DO 109 IFUEL= 1, CMnumMajFl
       DO 109 IBLDTP= 1, CMnumBldg
        DO 109 ISERV= 1,CMnumServ
         EndUseConsump(IFUEL,ISERV,IBLDTP,MNUMCR,CURIYR)=0.0
         EUCONwithDG (IFUEL,ISERV,IBLDTP,MNUMCR,CURIYR)=0.0                     !PVdispatch
         DO 109 IREG= 1, MNUMCR-2

          IF(EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR).LT.0.0) WRITE(RCDBG,104)ifuel,iserv,ibldtp,ireg
 104      Format(" *** COMM.F *** End-use consump for fuel: ",i3," serv: ",i3," bldg: ", i3," div: ",i3, " is negative --> resetting to 0.0")

          ! copied from below the next If block
          EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) =     &      !noneg
             max(EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR),0.0)  !noneg
          IF (IFUEL.EQ.1) EUCONwithDG(IFUEL,ISERV,IBLDTP,IREG,CURIYR) =     &      !noneg PVdispatch
             max(EUCONwithDG(IFUEL,ISERV,IBLDTP,IREG,CURIYR),0.0)  !noneg

                 IF (NRGBILL .GT. 0) THEN  !  check switch for including ebill provisions
                  IF(ISERV.EQ. 3 .AND. CURIYR.GT.16) THEN  ! Apply decrement to water heating Q for 2006 spray valve stnd                             - ebill
                  IF(curiyr.eq.51 .and. ibldtp.eq.5 .and. ireg.eq.2) write(rcdbg,*) 'before ',ifuel, EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR)    ! ebill
                    IF (IBLDTP .GT. 3 .AND. IBLDTP .LT. 7) &  ! Limit decrement to applicable building types                                          - ebill
                       EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) =     &                                                                          ! ebill
                        EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) -    &                                                                          ! ebill
                        prsvStdSavings * ((CMNewFloorSpace(IREG,IBLDTP,CURIYR) +      &  ! decrement spray valve std savings, proportional            - ebill
                        SurvFloorTotal(IREG,IBLDTP,CURIYR))/FoodSvcHlthLdgFloorTot) * &  ! to share of applicable floorspace and to                   - ebill
                        (EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) /              &  ! fuel share of water heating consumption for                - ebill
                        (EndUseConsump(1,ISERV,IBLDTP,IREG,CURIYR)+EndUseConsump(2,ISERV,IBLDTP,IREG,CURIYR) &  ! this CD and BT                        ebill
                        + EndUseConsump(3,ISERV,IBLDTP,IREG,CURIYR)))                                                                                 ! ebill
                    if(curiyr.eq.51 .and. ibldtp.eq.5 .and. ireg.eq.2) write(rcdbg,*) 'after ',ifuel, EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR)   ! ebill
                  END IF  ! Apply decrement to water heating Q for pre-rinse spray valve standard                                                     - ebill
                 END IF  ! Check switch for including ebill provisions                                                                                  ebill

          EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR) =     &      !noneg
             max(EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR),0.0)  !noneg

          EndUseConsump(IFUEL,ISERV,IBLDTP,MNUMCR,CURIYR) = &
           EndUseConsump(IFUEL,ISERV,IBLDTP,MNUMCR,CURIYR) + &
           EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR)
          FinalEndUseCon (IFUEL,IBLDTP,IREG,CURIYR)= &
            FinalEndUseCon (IFUEL,IBLDTP,IREG,CURIYR)  + &
            EndUseConsump(IFUEL,ISERV,IBLDTP,IREG,CURIYR)
          UnBenchedCon (IFUEL,IBLDTP,IREG,CURIYR)= &
           FinalEndUseCon (IFUEL,IBLDTP,IREG,CURIYR)
 
          IF (IFUEL.EQ.1) EUCONwithDG(IFUEL,ISERV,IBLDTP,MNUMCR,CURIYR)=   &      !PVdispatch
           EUCONwithDG(IFUEL,ISERV,IBLDTP,MNUMCR,CURIYR) +   &       !PVdispatch
           EUCONwithDG(IFUEL,ISERV,IBLDTP,IREG,CURIYR)               !PVdispatch


109   CONTINUE

!  Compute National Consumption Totals for NEMS FTAB Report:

      DO 150 IFUEL= 1, CMnumMajFl
       DO 150 ISERV= 1, CMnumServ
        CMUSConsumption (ISERV,IFUEL,CURIYR)= 0.0

        DO 140 IREG= 1, MNUMCR-2
         DO 140 IBLDTP= 1, CMnumBldg
          CMUSConsumption  (ISERV,IFUEL,CURIYR)= &
           CMUSConsumption (ISERV,IFUEL,CURIYR) + &
           EndUseConsump   (IFUEL,ISERV,IBLDTP,IREG,CURIYR)
  140   CONTINUE

        ! Convert totals from trillions to quadrillions of BTUs:
        CMUSConsumption (ISERV,IFUEL,CURIYR)= &
         CMUSConsumption (ISERV,IFUEL,CURIYR) / 1000.0

  150 CONTINUE

      y= CURIYR  ! for brevity

!  COMPUTE AVERAGE EFFICIENCIES OF MINOR SERVICES FOR FTAB

      DO s= CMnumMajServ+1, CMnumServ-1
       DO f= 1, CMnumMajFl
        CMUSAvgEff (s,f,y)= 0.0
        numerator= 0.0
        denominator= 0.0
        DO r= 1, MNUMCR-2
         DO b= 1, CMnumBldg
          numerator= numerator + FuelShareofService (r,b,s,f) * &
            (NewServDmd (s,b,r,y) + RetireServDmd (s,b,r,y) &
                                  + ServDmdSurv (s,b,r,y)  )
         END DO ! b
        END DO  ! r
        denominator= CMUSConsumption (s,f,y) * 1000.0 ! Quad->Tril
        If (denominator .GT.0.0) &
         CMUSAvgEff (s,f,y)= numerator / denominator
       END DO   ! f
      END DO    ! s

!  Minorfuel projections based on Energy Intensity * Floorspace 
      DO r= 1, MNUMCR-2
       DO f= CMnumMajFl+1, CMnumMajFl+CMnumMinFl

! Collect appropriate fuel price (note, 1987$)
             minflprice=0.0
             IF (f .EQ. 4) THEN
                IF(PRSCM (r,CURIYR).gt.0.) minflprice= (PRSCM (r,CURIYR))
              ELSE IF (f .EQ. 5) THEN
                IF(PlgCM (r,CURIYR).gt.0.) minflprice= (PLGCM (r,CURIYR))
              ELSE IF (f .EQ. 6) THEN
                IF(PclCM (r,CURIYR).gt.0.) minflprice= (PCLCM (r,CURIYR))           
              ELSE IF (f .EQ. 7) THEN
                IF(PmgCM (r,CURIYR).gt.0.) minflprice= (PMGCM (r,CURIYR))
              ELSE IF (f .EQ. 8) THEN
                IF(PksCM (r,CURIYR).gt.0.) minflprice= (PKSCM (r,CURIYR))                 
             END IF
          If (minflprice.eq.0.) then 
          write(6,'(A,I4,A,I4)') "Warning:  Price for commercial fuel <= 0. Fuel=",F,", CensusDiv=",R
          minflprice=defaultprices(f) 
          END IF
! the following commented out lines are for debug printing
           totalfs=0.0
           DO b= 1, CMnumBldg
            totalfs=totalfs+(SurvFloorTotal (r,b,CURIYR) + CMNewFloorSpace (r,b,CURIYR))
           enddo
 
!        use appropriate own-price elasticity for projected intensity (consumption per square foot)    !minorfuel project
!          also scale price to 2005 dollars was used in regressions)
  !         
            If (MinFuelBeta (r,f-CMnumMajFl).GT.0.)  MinFuelBeta (r,f-CMnumMajFl)=0.0
          

           temp = exp(MinFuelAlpha (r,f-CMnumMajFl) + log(minflprice / MC_JPGDP(-2) &
               * MC_JPGDP(2005-baseyr+1))* MinFuelBeta (r,f-CMnumMajFl)) * FloorAdj(r) 
            
   
               
!        multiply intensity by floorspace to get projected energy consumption and scale to trillion Btu
 !             for coal no growth for floor space
           DO b= 1, CMnumBldg
            if(f.ne.6 ) then
              FinalEndUseCon (f,b,r,CURIYR)= temp * 1000. *  &                      
               (SurvFloorTotal (r,b,CURIYR) + CMNewFloorSpace (r,b,CURIYR))
             else
              FinalEndUseCon (f,b,r,CURIYR)= temp * 1000. *  &                      
             (SurvFloorTotal (r,b,2005-baseyr+1) + CMNewFloorSpace (r,b,2005-baseyr+1))
                    

            endif
           END DO ! b
  
       END DO  ! f for minor fuels

       DO 390 b= 1, CMnumBldg                                           !distgen
        DO 390 nt= 1, ntek                                              !distgen
         f=ifueltype(nt)                                                !distgen
         if (f.le.8)goto 390    ! skip modeled fuels, solar 
         FinalEndUseCon (f,b,r,CURIYR)=                               & !distgen
          FinalEndUseCon (f,b,r,CURIYR) + fuelusage(curiyr,r,b,nt)      !distgen
!        Wood and Hydro here -- biomass/wood = 9, hydro = 11            !distgen
  390  CONTINUE                                                         !distgen

      END DO   ! r


!  Calculate unbenchmarked and final consumption by Census division for reporting

      DO 190 IREG= 1, MNUMCR-2
       DO 190 IBLDTP= 1, CMnumBldg
        DO 198 IFUEL= 1, CMnumMajFl
         CMFinalUnbenchUse (IFUEL,IREG,CURIYR) = &
         CMFinalUnbenchUse (IFUEL,IREG,CURIYR) + &
          UnBenchedCon (IFUEL,IBLDTP,IREG,CURIYR)
198      CMFinalEndUse (IFUEL,IREG,CURIYR)= &
          CMFinalEndUse (IFUEL,IREG,CURIYR) + &
           FinalEndUseCon (IFUEL,IBLDTP,IREG,CURIYR)

        DO 199 IFUEL2= CMnumMajFl+1, &
               CMnumMajFl+CMnumMinFl+CMnumRenew
199      CMFinalEndUse (IFUEL2,IREG,CURIYR)= &
          CMFinalEndUse (IFUEL2,IREG,CURIYR) + &
          FinalEndUseCon (IFUEL2,IBLDTP,IREG,CURIYR)
190   CONTINUE

!  Calculate total US consumption and populate CMSEDS for STEO years
!  by sharing out national STEO estimates to Census divisions.  Share 
!  is determined by Census division share of total US consumption from
!  model.

      IF (CURIYR .GT. MSEDYR .AND. CURIYR .LE. KSTEOYR) THEN
        DO f= 1, CMnumALLFl
         IF (FlagRegSTEO(f).EQ. 0.0 .AND. CURIYR .GE. MSEDYR+1) THEN  ! regsteo - share national if regional Q not available
          CMFinalEndUse(f,MNUMCR,CURIYR) = 0.0  ! Initialize national Q to share STEO.
          CMNonBldgUse(f,MNUMCR,CURIYR-1) = 0.0  ! Initialize national nonbldg Q to share STEO.
          DO r = 1, MNUMCR-2
           CMFinalEndUse(f,MNUMCR,CURIYR)= &
              CMFinalEndUse(f,MNUMCR,CURIYR) + &
              CMFinalEndUse(f,r,CURIYR)
             CMNonBldgUse(f,MNUMCR,CURIYR-1)= &     ! ebe0503
              CMNonBldgUse(f,MNUMCR,CURIYR-1) + &   ! ebe0503
              CMNonBldgUse(f,r,CURIYR-1)            ! ebe0503
          END DO  ! Loop over Census divisions to calculate US total Q

            ! Use Census div share of US Q to share out national STEO estimates.
          DO r = 1, MNUMCR-2
           CMSEDS(f,r,CURIYR)= STEOdat(f,CURIYR-MSEDYR) * &
              ((CMFinalEndUse(f,r,CURIYR)+CMNonBldgUse(f,r,CURIYR-1)) / &
              (CMFinalEndUse(f,MNUMCR,CURIYR)+CMNonBldgUse(f,MNUMCR,CURIYR-1)))  ! Add prev.yr nonbldg Q to preserve share ebe0503
          END DO  ! Loop over Census divisions to share out STEO estimates.
         ELSE ! regsteo - regional Q available and has been read into CMSEDS
         END IF  ! regsteo - check flag to see if regional Q available
        END DO   ! Loop over fuels with STEO data (major + minor fuels)
      END IF    ! Check for STEO years to calculate regional estimates if needed.

!  In preparation of adding solar contribution to consumption totals,
!  first add solar thermal contribution of solar water heaters (tech class 20)
!  chosen in tech choice subroutine.  Credit given for avoided electricity Q
!  at efficiency factor of residential electric water heater (currently 0.83).

       DO IREG= 1, MNUMCR-2
        DO v= 1, CMnumEqV(20)                                           ! solacct
         DO d= 1, 3                                                     ! solacct
          Do ibldtp =1,CMnumBldg                                        ! solacct !add b to ksdout
           SolarRenewableContrib (IREG,3,CURIYR)=                     & ! solacct
            SolarRenewableContrib (IREG,3,CURIYR) +                   & ! solacct 
            EquipSD(IREG,ibldtp,20,v,d,CURIYR)*0.6/0.83                 ! solacct !add b to ksdout
           END DO ! building types                                                !add b to ksdout
         END DO ! Loop over decision types
        END DO  ! Loop over solar water heater vintages.
       END DO   ! Loop over Census divisions.

!  Compute Total US Consumption by Building Type for FTAB Report:

      DO 210 IBLDTP= 1, CMnumBldg
       CMFinalEndUseCon (IBLDTP,CURIYR)= 0.0


       DO 220 IREG= 1, MNUMCR-2

        ! sum contrib from maj fuels, min fuels, & nonsolar renewabls
        DO 230 IFUEL= 1, CMnumMajFl+CMnumMinFl+CMnumRenew
         CMFinalEndUseCon (IBLDTP,CURIYR)= &
          CMFinalEndUseCon (IBLDTP,CURIYR) + &
            FinalEndUseCon (IFUEL,IBLDTP,IREG,CURIYR)
  230   CONTINUE

        ! add contribution from Solar Thermal consumption:


        DO 240 ISERV= 1, CMnumSolarServ
         IF ( ISERV .EQ. 6 ) THEN
          CMFinalEndUseCon (IBLDTP,CURIYR)= &
           CMFinalEndUseCon (IBLDTP,CURIYR) + &
           SolarRenewableContrib (IREG,ISERV,CURIYR) / CMnumBldg &
            * (29.916 / SunlightEfficacy)
            !  29.916 TRILLS PER GIGAWATTYEAR
         ELSE
          CMFinalEndUseCon (IBLDTP,CURIYR)= &
           CMFinalEndUseCon (IBLDTP,CURIYR) + &
           SolarRenewableContrib (IREG,ISERV,CURIYR) / CmnumBldg
         END IF  ! special conversion of ST light GLY to TRILL
 240    CONTINUE !services


!  add solar pv generation trills                        !distgen

         CMFinalEndUseCon (IBLDTP,CURIYR)=             & !distgen
            CMFinalEndUseCon (IBLDTP,CURIYR)           & !distgen
             +Trills(curiyr,ireg,ibldtp,1)               !distgen

           SolarPVContrib (CURIYR,ireg) =              & !distgen
             SolarPVContrib (CURIYR,ireg)              & !distgen
             +Trills(curiyr,ireg,ibldtp,1) * KFossilHR/3412. !distgen    
                                                         ! - conversion to fossil fuel equivalent for renewables

  220  CONTINUE  ! across Census Divisions

        ! convert totals from trillions to quadrillions of BTUs:
        CMFinalEndUseCon (IBLDTP,CURIYR)= &
         CMFinalEndUseCon (IBLDTP,CURIYR) / 1000.0

  210 CONTINUE   ! across Building Types

      IF (PRTDBGK.EQ.1 .AND. CURIYR.EQ.CMFirstYr .AND. CURITR.EQ.1) THEN
         WRITE (RCDBG,*) 'Final End Use Consumption before Benchmarking by Fuel'
       DO 129 IREG= 1, MNUMCR-2
       WRITE (RCDBG,530) (BASEYR-1)+CURIYR,CMRegions(IREG)
129     WRITE (RCDBG,535) (CMFinalEndUse (IFUEL,IREG,CURIYR), &
                        IFUEL= 1, CMnumAllFl+CMnumRenew)
      END IF
! *****FORMAT Statements
500   FORMAT ('>>>>> Subroutine COMConsumption called ', &
              I4,1X,'Iter ',I2)
510   FORMAT (I4,' Iter=',I2,1X,A20,' Bldg Type=',I2,1X, &
              A20,/,' Final End Use Consumption=', &
              F9.2,' Cogen Share=',F4.2,' Cogen Fuel=',F7.2, &
              ' Cogen Efficiency=',F5.2)
520   FORMAT (/,'Minor Fuel End Use Consumption for Region=',I2, &
              ' Building Type=',I2)
525   FORMAT (2X,5(F6.2,1X))
530   FORMAT (/,1X,I4,1X,A20)
535   FORMAT (2X,11(F9.2,1X))
      RETURN

        CONTAINS
!
!====Distributed SR Elasticity Calculation Function
      REAL FUNCTION KELAST*4 (F,R,S,ALPHA,EF1,EF2,EF3,elasbaseyr,report)
!
! This function returns the lagged (or spread out) short run elasticity
!  adjustment based on changes in fuel prices relative to the base year


      IMPLICIT NONE

      REAL*4  EF1,EF2,EF3    ! distributional shares for elasticity effects-should sum to 1.0
      REAL*4  ALPHA          ! total short run elasticity from input file
      REAL*4  FAC1,FAC2,FAC3 ! factors corresponding to elasticity effects of particular years
              
      INTEGER F,R,S

        INTEGER*4 elasbaseyr,report   ! link to kdebug output file handle

        FAC1=1. ; FAC2=1. ; FAC3=1.   ! Initialize

        IF (CURIYR.GE.elasbaseyr+1)THEN

         elasticity:  select case (F)    ! distribute SR price elasticity over 3 previous years
                                      ! using fuel specific variable names for prices
            case (1) elasticity      ! electricity
             IF (PELCMOUT(R,elasbaseyr,S) .GT. 0.0) THEN
                     FAC1=(PELCMOUT(R,CURIYR,S)/PELCMOUT(R,elasbaseyr,S))**(ALPHA*EF1)
                     FAC2=(PELCMOUT(R,CURIYR-1,S)/PELCMOUT(R,elasbaseyr,S))**(ALPHA*EF2)
                     FAC3=(PELCMOUT(R,CURIYR-2,S)/PELCMOUT(R,elasbaseyr,S))**(ALPHA*EF3)
                   END IF
              case (2) elasticity      ! natural gas
             IF (PNGCM(R,elasbaseyr) .GT. 0.0) THEN
                     FAC1=(PNGCM(R,CURIYR)/PNGCM(R,elasbaseyr))**(ALPHA*EF1)
                     FAC2=(PNGCM(R,CURIYR-1)/PNGCM(R,elasbaseyr))**(ALPHA*EF2)
                     FAC3=(PNGCM(R,CURIYR-2)/PNGCM(R,elasbaseyr))**(ALPHA*EF3)
                   END IF
              case (3) elasticity      ! distillate
                 IF (PDSCM(R,elasbaseyr) .GT. 0.0) THEN
                     FAC1=(PDSCM(R,CURIYR)/PDSCM(R,elasbaseyr))**(ALPHA*EF1)
                     FAC2=(PDSCM(R,CURIYR-1)/PDSCM(R,elasbaseyr))**(ALPHA*EF2)
                     FAC3=(PDSCM(R,CURIYR-2)/PDSCM(R,elasbaseyr))**(ALPHA*EF3)
                   END IF

         end select elasticity

         IF (CURIYR .LT. elasbaseyr+3) FAC3=1.
         IF (CURIYR .LT. elasbaseyr+2) FAC2=1.

        END IF

        KELAST = FAC1*FAC2*FAC3

!        WRITE(report,*)'SRElasAmt=',SRElasAmt, '  YEAR ',CURIYR, 
!        WRITE(report,*)' fuel ',F,

       RETURN
       END FUNCTION KELAST

      END
!********
      SUBROUTINE COMBenchmarking (RCDBG,RCQUAN)
      IMPLICIT NONE
      INTEGER  FILE_MGR
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'comparm'
      include 'comvars'
      include 'commrep'
      include 'eusprc'
      include 'apq'
      include 'macout'
      include 'qsblk'
      include 'emission'
      INTEGER*4 IREG, IFUEL, RCODE1, RCQUAN, RCDBG
      REAL*4 FinalUseTotal(CMnumAllFl)
      INTEGER*4 RC1, RCL1, I, IOS, count, NumErr
      REAL*4 TOTALFLR, KADJ (3,25)
      INTEGER*4 infile     !  Reusable file handle
      INTEGER*4 f,   & ! fuel index
                b, & ! building type index
                e,   & ! emission type index
                r,   & ! Census Division index
                y, y2   ! year index
      INTEGER*2 DecayBM ! STEO Mistie Benchmarking Switch
      INTEGER*4 LastDecayYr ! Year which STEO Benchmarking is 0
      INTEGER*2 NoHist ! Switch - set to 1 in KPARM to turn off calibration to history and STEO ! NoHistory
      COMMON /STEOBenchSwitches/ LastDecayYr,DecayBM,NoHist
      REAL*4 STEOTieDecayFactor  ! to taper STEO benchmarking term
      REAL*4 CalendarYear     ! current year index + (BASEYR-1)
      REAL*4 LastSTEOYr       ! calendar year of end of STEO data
      REAL*4 FirstNonBenchYr  ! cal. year STEO benchmarking term
                                 ! becomes zero
      REAL*4 minflprice    ! minor fuel price holding variable current year
      REAL*4 minpricebase  ! minor fuel price holding variable previous year
      REAL*4 SEDSTOTALFS !  temp variable for Minorfuel projections holding total floorspace in a region FOR LAST HISTORICAL YEAR 
      INTEGER*4 STEOBM          ! System STEO benchmarking switch
      INTEGER*4 RTOVALUE      ! External routine to get options
      REAL*4 SRElas        ! Holding variable for SR elasticity input    kelast
        REAL*4 EF1, EF2, EF3 ! elasticity distribution factors to spread
                             ! short run price elasticity effect over 3 years
      INTEGER*4 elasbaseyr      ! holding variable for base year index
      INTEGER*4 NRGBILL    ! switch for energy bill provisions: 0 - no bill, 1 - bill without unfunded provisions, 2 - bill with unfunded items - ebill
      INTEGER*4 STIMULUS    ! switch for feb 2009 stimulus package provisions: 0 - no stimulus, 1 - with stimulus; default is with stimulus - arra09
      EXTERNAL RTOVALUE

       COMMON/SolarPV/SolarPVContrib(MNUMYR,MNUMCR)     !distgen
       REAL*4 SolarPVContrib                            !distgen
       REAL*4 DiffQBM(MNUMCR)                           !Difference between CHP and SEDS biomass Q
       
       COMMON /Misc2/ WaterServicesElQ                                 !miscdetail
       REAL*4 WaterServicesElQ(MNUMCR-2,MNUMYR)                        !miscdetail
       REAL*4 WaterServicesQGrowth                                     !miscdetail

      STEOBM= RTOVALUE("STEOBM  ",0)  ! get STEO benchmarking switch
      NRGBILL = RTOVALUE("NRGBILL  ",0)  ! Get energy bill switch value
      STIMULUS = RTOVALUE("STIMULUS  ",0)! Get stimulus package switch value arra09

      ! Initialize
        EF1=0.5; EF2=0.35; EF3=0.15                                     !   SRelas
      elasbaseyr = CBECSyear - (BASEYR-1)                               !   SRelas

!  Calculate consumption for water distribution and treatment (in trills) based
!    on TIAX August 2006 report - part of nonbldg Q. National estimate shared 
!    to Census divisions using division-level population from MACOUT file

      y= CURIYR
      DO r= 1, MNUMCR-2                                                 !miscdetail 
        WaterServicesElQ(r,y)= (0.0057*(FLOAT(y-11))**3.0             & !miscdetail 
          - 0.3252*(FLOAT(y-11))**2.0 + 7.7630*(FLOAT(y-11))          & !miscdetail 
          + 156.3600)*(MC_NP(r,y)/MC_NP(11,y))                          !miscdetail 
      END DO ! r                                                        !miscdetail


       DO 290 IFUEL= 1, CMnumAllFl
290     FinalUseTotal(IFUEL)= 0.0

! Compute SEDS Benchmarking Quantities if this is a SEDS year

      IF (CURIYR .LE. MSEDYR+1) THEN ! SEDS data is available.
                                   ! STEO data from the first year
                                   ! after the last SEDS year is
                                   ! actually MER data and is
                                   ! considered equivalent to SEDS


       DO r= 1, MNUMCR-2
        DO f= 1, CMnumMajFl+CMnumMinFl
         ! Compute SEDS mistie:
         SEDSMistie (f,r)= CMSEDS (f,r,CURIYR) - &         
                           CMFinalEndUse (f,r,CURIYR)

         CMNonBldgUse (f,r,CURIYR)= SEDSMistie (f,r)

 ! Set nonbuilding use to 0.0 if not calibrating to history or STEO (NoHist=1 in KPARM.txt)
         IF (NoHist.EQ.1)CMNonBldgUse (f,r,CURIYR)= 0.0             ! NoHistory

         IF (PRTDBGK.EQ.1) WRITE (RCDBG,510) CMRegions (r), &
              CMAllFuels (f), CMNonBldgUse (f,r,CURIYR), &
              (BASEYR-1)+CURIYR
        END DO ! f
       END DO  ! r

      ELSE IF (CURIYR .GT. MSEDYR+1 .AND. CURIYR .LE. KSTEOYR) THEN

       DO r= 1, MNUMCR-2
        WaterServicesQGrowth = WaterServicesElQ(r,CURIYR) - &        !miscdetail 
          WaterServicesElQ(r,MSEDYR+1)                               !miscdetail - MER for 1st year after SEDS

        DO f= 1, CMnumMajFl+CMnumMinFl
        
      ! First year after historical data - calculate historical mistie  ! STEOread-avg
      ! as average of mistie for last 5 historical years - could parameterize number of years for average ...
         IF (CURIYR .EQ. MSEDYR+2 .AND. CURITR .EQ. 1) THEN          ! STEOread-avg
          SEDSMistie (f,r)= 0.0
          DO y2= 1, 5
           SEDSMistie (f,r)= SEDSMistie (f,r) + CMNonBldgUse (f,r,CURIYR-y2) ! STEOread-avg
          END DO !y2  ! STEOread-avg
          SEDSMistie (f,r)= SEDSMistie (f,r)/5.0 ! STEOread-avg
         END IF   ! STEOread-avg

         ! Allow SEDS mistie portion of nonbuilding use to grow
         ! at the rate of total floorspace within the Census division:
         ! Compute STEO mistie after SEDS benchmarking
         ! (Note that STEO values are stored in the CMSEDS array along with SEDS values):

!   **********************************************************************************************************
!    Historical SEDS values through MSEDYR (see parametr include); remaining history through MSEDYR+1 for MER
!   **********************************************************************************************************

         IF (f .LT. 4) SRElas = ShortRunPriceElasofDmd(r,10,f)                    !  kelast2 - corrected for bounds check ebe091406

             growmistie:  select case (f)    ! allow elec. and NG SEDS mistie growth with floorspace     ebe072506

             ! for electricity, include LED traffic light standard from Energy Policy Act of         ebill
             ! 2005 (unless ebill provisions switch turned off) and Q growth for water services.     miscdetail

              case (1) growmistie      ! electricity with ebill standard effective in 2006 and water services growth  miscdetail  arra09 mistie treatment
           IF (CURIYR .LT. 17 .OR. NRGBILL .EQ. 0) THEN  !no traffic light standard prior to 2006 or ebill provisions turned off
            STEOMistie (f,r)= CMSEDS (f,r,CURIYR) - &
             (CMFinalEndUse (f,r,CURIYR) + (SEDSMistie(f,r) + &
             (CMTotalFlspc(r,CMnumBldg+1,CURIYR) - CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1)) &
             /CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1) * ABS(SEDSMistie(f,r) -    &
             WaterServicesElQ(r,MSEDYR+1))+WaterServicesQGrowth))      ! miscdetail
            CMNonBldgUse (f,r,CURIYR)= SEDSMistie (f,r) + &
             (CMTotalFlspc(r,CMnumBldg+1,CURIYR) - CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1)) &
             /CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1) * ABS(SEDSMistie(f,r) -    &
             WaterServicesElQ(r,MSEDYR+1))+WaterServicesQGrowth        ! miscdetail
           ELSE  !  phase in traffic light standard effects over 10 years to account for stock turnover  ebill
            STEOMistie (f,r)= CMSEDS (f,r,CURIYR) - &
             (CMFinalEndUse (f,r,CURIYR) + (SEDSMistie(f,r) * &
                            (1 - (0.0008148 * FLOAT(CURIYR - 16))) + &                 !  ebill - phase in signal standard effect
             (CMTotalFlspc(r,CMnumBldg+1,CURIYR) - CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1)) &
             /CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1) * ABS(SEDSMistie(f,r) -    &
             WaterServicesElQ(r,MSEDYR+1))+WaterServicesQGrowth))      ! miscdetail
            CMNonBldgUse (f,r,CURIYR)= SEDSMistie (f,r) * &
                            (1 - (0.0008148 * FLOAT(CURIYR - 16))) + &                 !  ebill - phase in signal standard effect
             (CMTotalFlspc(r,CMnumBldg+1,CURIYR) - CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1)) &
             /CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1) * ABS(SEDSMistie(f,r) -    &
             WaterServicesElQ(r,MSEDYR+1))+WaterServicesQGrowth        ! miscdetail
           END IF  !Check for 2006 - effective date of ebill LED traffic light standard

              case (2) growmistie           ! natural gas SEDS mistie no longer grows with floorspace    miscdetail
!         For AEO 2009 - take out growth in natural gas SEDS mistie ebe081408 
!           STEOMistie (f,r)= CMSEDS (f,r,CURIYR) - &
!             (CMFinalEndUse (f,r,CURIYR) + (SEDSMistie(f,r) + &
!            (MC_COMMFLSP(r,1,CURIYR) - MC_COMMFLSP(r,1,MSEDYR+2)) &
!             /MC_COMMFLSP(r,1,MSEDYR+2) * ABS(SEDSMistie(f,r))))
!           CMNonBldgUse (f,r,CURIYR)= SEDSMistie (f,r) + &
!            (MC_COMMFLSP(r,1,CURIYR) - MC_COMMFLSP(r,1,MSEDYR+2)) &
!             /MC_COMMFLSP(r,1,MSEDYR+2) * ABS(SEDSMistie(f,r))

           STEOMistie (f,r)= CMSEDS (f,r,CURIYR) - &         ! ebe081408
             (CMFinalEndUse (f,r,CURIYR) + SEDSMistie(f,r))  ! ebe081408
           CMNonBldgUse (f,r,CURIYR)= SEDSMistie (f,r)       ! ebe081408

              case default growmistie       ! all other fuels, no SEDS mistie growth with floorspace    miscdetail
           STEOMistie (f,r)= CMSEDS (f,r,CURIYR) - &
             (CMFinalEndUse (f,r,CURIYR) + SEDSMistie(f,r))
           CMNonBldgUse (f,r,CURIYR)= SEDSMistie (f,r)
             end select growmistie          ! ebill and miscdetail
			
         IF (STEOBM.EQ.1 .AND. ComSTEOBM.EQ.1) THEN ! Check for STEO benchmark switches on

          !  Adjustment factors for KSTEOYR

          IF (CURIYR .GT. MSEDYR+1 .AND. CURIYR .LE. KSTEOYR .AND. KSTEOCLS .EQ. 1) THEN !change to all fuels, STEO years beyond MER
!            IF (f .EQ. 1) THEN   ! If ksteocls is 1, do not bench to STEO in KSTEOYR -get within 2% instead                   
           IF ((STEOMistie(f,r)/CMSEDS(f,r,CURIYR)) .LT. -0.02) THEN      ! If more than 2% above STEO,
             STEOMistie(f,r)= STEOMistie(f,r) + 0.02 * CMSEDS(f,r,CURIYR)     ! bench to 2% above STEO
           ELSE IF ((STEOMistie(f,r)/CMSEDS(f,r,CURIYR)) .GT. 0.02) THEN  ! If more than 2% below STEO,
             STEOMistie(f,r)= STEOMistie(f,r) - 0.02 * CMSEDS(f,r,CURIYR)     ! bench to 2% below STEO
           ELSE
             STEOMistie(f,r)= 0.0                                         ! If within 2% of STEO, don't bench at all.
                    
           END IF   ! Check for 2% margin
!            END IF   ! If ksteocls is 1, do not bench electricity to STEO in KSTEOYR -get within 2% instead
          END IF  ! KSTEOCLS electricity non-benching option

          CMNonBldgUse (f,r,CURIYR)= &
           CMNonBldgUse (f,r,CURIYR) +  STEOMistie (f,r)
		   
		   			
 ! Set nonbuilding use to 0.0 if not calibrating to history or STEO (NoHist=1 in KPARM.txt)
           IF (NoHist.EQ.1)CMNonBldgUse (f,r,CURIYR)= 0.0             ! NoHistory

         END IF   ! Check for STEO Benchmarking switches on

         IF (PRTDBGK.EQ.1) WRITE (RCDBG,510) CMRegions (r), &
              CMAllFuels (f), CMNonBldgUse (f,r,CURIYR), &
              (BASEYR-1)+CURIYR
        END DO
       END DO
	 
	  
      ELSE ! Barry Cohen's benchmarking scheme, where the final STEO
           ! mistie is ramped down to 0 in the year LastDecayYr, with
           ! inclusion of historical SEDS benchmarking factor throughout.
       CalendarYear= (BASEYR-1) + CURIYR
       LastSTEOYr=   (BASEYR-1) + KSTEOYR
       FirstNonBenchYr= LastDecayYr
       IF (DecayBM.EQ.0) THEN
            STEOTieDecayFactor = 1.0
       ELSE
            STEOTieDecayFactor= 1.0 -   (CalendarYear-LastSTEOYr)/ &
                                     (FirstNonBenchYr-LastSTEOYr)
       ENDIF  ! calculating decay factor for benchmarking
       IF (STEOTieDecayFactor .LT. 0.0) STEOTieDecayFactor= 0.0
       DO r= 1, MNUMCR-2
        WaterServicesQGrowth = WaterServicesElQ(r,CURIYR) - &        !miscdetail 
          WaterServicesElQ(r,MSEDYR+1)                               !miscdetail 
        DO f= 1, CMnumMajFl+CMnumMinFl

        ! Calculate non building-specific portion of expression:
        !  an unfortunate necessity due to uniquely-named fuel price variables

         IF (f .EQ. 1) THEN
          minflprice= max(.01,PELCMOUT (r,CURIYR,10))            ! defend mfelas
          minpricebase= max(.01,PELCMOUT(r,CBECSyear-BASEYR+1,10))  ! mfelas
         ELSE IF (f .EQ. 2) THEN
          minflprice= max(.01,PNGCM (r,CURIYR))                  ! defend mfelas
          minpricebase= max(.01,PNGCM(r,CBECSyear-BASEYR+1))        ! mfelas
         ELSE IF (f .EQ. 3) THEN
          minflprice= max(.01,PDSCM (r,CURIYR))                  ! defend mfelas
          minpricebase= max(.01,PDSCM(r,CBECSyear-BASEYR+1))        ! mfelas
         ELSE IF (f .EQ. 4) THEN
          minflprice= max(.01,PRSCM (r,CURIYR))                  ! defend mfelas
          minpricebase= max(.01,PRSCM(r,CBECSyear-BASEYR+1))        ! mfelas
         ELSE IF (f .EQ. 5) THEN
          minflprice= max(.01,PLGCM (r,CURIYR))                  ! defend mfelas
          minpricebase= max(.01,PLGCM(r,CBECSyear-BASEYR+1))        ! mfelas
         ELSE IF (f .EQ. 6) THEN
          minflprice= max(.01,PCLCM (r,CURIYR))                  ! defend mfelas
          minpricebase= max(.01,PCLCM(r,CBECSyear-BASEYR+1))        ! mfelas
         ELSE IF (f .EQ. 7) THEN
          minflprice= max(.01,PMGCM (r,CURIYR))                  ! defend mfelas
          minpricebase= max(.01,PMGCM(r,CBECSyear-BASEYR+1))        ! mfelas
         ELSE IF (f .EQ. 8) THEN
          minflprice= max(.01,PKSCM (r,CURIYR))                  ! defend mfelas
          minpricebase= max(.01,PKSCM(r,CBECSyear-BASEYR+1))        ! mfelas
         ELSE ! there must be some mistake                            mfelas
         END IF
         
             SEDStotalfs=0.0        !minor fuelproject                            
           DO b= 1, CMnumBldg
            SEDStotalfs=SEDStotalfs+(SurvFloorTotal (r,b,MSEDYR+1) + CMNewFloorSpace (r,b,MSEDYR+1))
           enddo


!   **********************************************************************************************************
!    Historical SEDS values through MSEDYR (see parametr include); remaining history through MSEDYR+1 for MER
!   **********************************************************************************************************

         nonbldgXfuel:  select case (f)   ! calculate nonbldg use according to fuel - including price elasticity where appropriate
          case (1) nonbldgXfuel  !  electricity(1) including water services Q and sector growth and ebill traffic light standard
           SRElas = ShortRunPriceElasofDmd(r,10,f)                         !kelast2
           IF (NRGBILL .EQ. 0) THEN  !check switch for including energy bill provisions (0=off, 1=on, 2=on including unappropriated provisions)
                CMNonBldgUse (f,r,CURIYR)= (SEDSMistie (f,r) +           &
                 (CMTotalFlspc(r,CMnumBldg+1,CURIYR) - CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1)) &
                 /CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1) * ABS(SEDSMistie(f,r) -    &
                 WaterServicesElQ(r,MSEDYR+1))+WaterServicesQGrowth) *   & !miscdetail
                 KELAST2(f,r,10,SRelas,EF1,EF2,EF3,elasbaseyr,rcdbg)       !kelast2
           ELSE  ! energy bill provisions included, incorporate LED traffic light standard
                trafficlight:  select case (curiyr)    ! decrement original non-bldg use to account for
                                              ! LED traffic light standard in Energy Policy Act of 2005 ebill
                 case (1:16) trafficlight      ! ebill standard not effective until 2006
                CMNonBldgUse (f,r,CURIYR)= (SEDSMistie (f,r) +           &
                 (CMTotalFlspc(r,CMnumBldg+1,CURIYR) - CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1)) &
                 /CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1) * ABS(SEDSMistie(f,r) -    &
                 WaterServicesElQ(r,MSEDYR+1))+WaterServicesQGrowth) *   & !miscdetail
                 KELAST2(f,r,10,SRelas,EF1,EF2,EF3,elasbaseyr,rcdbg)       !kelast2
                       case (17:26) trafficlight      ! phase in standard effects over 10 years to account for stock turnover
                CMNonBldgUse (f,r,CURIYR)= (SEDSMistie (f,r) *           &
                            (1 - (0.0008148 * FLOAT(CURIYR - 16))) +     & !ebill - phase in signal standard effect
                 (CMTotalFlspc(r,CMnumBldg+1,CURIYR) - CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1)) &
                 /CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1) * ABS(SEDSMistie(f,r) -    &
                 WaterServicesElQ(r,MSEDYR+1))+WaterServicesQGrowth) *   & !miscdetail
                 KELAST2(f,r,10,SRelas,EF1,EF2,EF3,elasbaseyr,rcdbg)       !kelast2
                       case default trafficlight      ! after 2015 standard has reached full effect
                CMNonBldgUse (f,r,CURIYR)= (SEDSMistie (f,r)*0.9918518 + & !ebill - full effect of signal standard
                 (CMTotalFlspc(r,CMnumBldg+1,CURIYR) - CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1)) &
                 /CMTotalFlspc(r,CMnumBldg+1,MSEDYR+1) * ABS(SEDSMistie(f,r) -    &
                 WaterServicesElQ(r,MSEDYR+1))+WaterServicesQGrowth) *   & !miscdetail
                 KELAST2(f,r,10,SRelas,EF1,EF2,EF3,elasbaseyr,rcdbg)       !kelast2
                end select trafficlight         !ebill
           END IF  ! check switch for including energy bill provisions

          case (2) nonbldgXfuel  !  natural gas(2) including price elasticity
!        For AEO2009 include major fuel elasticity but no sector growth ebe081408 
           SRElas = ShortRunPriceElasofDmd(r,10,f)                         !kelast2
!           CMNonBldgUse (f,r,CURIYR)= (SEDSMistie (f,r) +               &
!            (MC_COMMFLSP(r,1,CURIYR) - MC_COMMFLSP(r,1,MSEDYR+2))       &
!            / MC_COMMFLSP(r,1,MSEDYR+2) * ABS(SEDSMistie(f,r))) *       &
           CMNonBldgUse (f,r,CURIYR)= SEDSMistie (f,r) *                 & !ebe081408 
            KELAST2(f,r,10,SRelas,EF1,EF2,EF3,elasbaseyr,rcdbg)            !kelast2
          case (3) nonbldgXfuel  !  distillate(3) include major fuel elasticity but no sector growth
           CMNonBldgUse (f,r,CURIYR)= SEDSMistie (f,r) *                 &
            KELAST2(f,r,10,SRelas,EF1,EF2,EF3,elasbaseyr,rcdbg)            !kelast2 - distillate
               
         case (6) nonbldgXfuel  !  minor fuels - coal(6)- no growth with sector, no elasticity  Minorfuel project
           CMNonBldgUse (f,r,CURIYR)=  SEDSMistie (f,r)   
		   case (4) nonbldgXfuel  !  minor fuels - residual(4)- no growth with sector, no elasticity  Minorfuel project
          CMNonBldgUse (f,r,CURIYR)=  SEDSMistie (f,r) 
          case default nonbldgXfuel  !  minor fuels - resid(4), LPG(5), mo gas(7)and kerosene(8) - no growth with sector, include elasticity        Minorfuel project
           CMNonBldgUse (f,r,CURIYR)= ((SEDSMistie (f,r)/ SEDStotalfs)*  & !Change to no growth with floorspace for minor fuels ebe062603
            ((minflprice/minpricebase)** MinFuelBeta(r,f - CMnumMajFl))) *SEDStotalfs  !mfelas
         end select nonbldgXfuel    !treatment of nonbldg fuel use by fuel
         
         IF (STEOBM.EQ.1 .AND. ComSTEOBM.EQ.1) THEN           !STEObm - removed exclusion of natural gas (f=2)
          IF (f.EQ. 4) THEN     !MSEDYR=24 for 2013     !STEObenchresid
            CMNonBldgUse(f,r,CURIYR) = CMNonBldgUse(f,r,CURIYR) + STEOMistie (f,r) * STEOTieDecayFactor					   !STEObenchresid
          ELSE                                                                                                  !STEObenchresid
            CMNonBldgUse(f,r,CURIYR) = CMNonBldgUse(f,r,CURIYR) + STEOMistie (f,r) * STEOTieDecayFactor
          END IF                                                                                                !STEObenchNG
         END IF  ! Check for STEO Benchmarking switches on
			

 ! Set nonbuilding use to 0.0 if not calibrating to history or STEO (NoHist=1 in KPARM.txt)
         IF (NoHist.EQ.1)CMNonBldgUse (f,r,CURIYR)= 0.0             ! NoHistory

         IF (PRTDBGK.EQ.1) WRITE (RCDBG,510) CMRegions (r),              &
             CMAllFuels (f), CMNonBldgUse (f,r,CURIYR),                  &
             (BASEYR-1)+CURIYR
        END DO
       END DO

      END IF  ! calculating SEDS tie


! Benchmark consumption to SEDS/STEO:
      DO r= 1, MNUMCR-2
       DO f= 1, CMnumMajFl+CMnumMinFl
         CMFinalEndUse(f,r,CURIYR)= &
          CMFinalEndUse (f,r,CURIYR) + CMNonBldgUse (f,r,CURIYR)
        IF(f .EQ. 1) THEN                                                  !noneg
          CMFinalEndUse(f,r,CURIYR) =                                    & !noneg
          max(CMFinalEndUse(f,r,CURIYR), 0.1)                              !noneg
         IF (CMFinalEndUse(f,r,CURIYR) .EQ. 0.1)                         & !noneg
          WRITE(RCDBG,*) "Electricity use set to 0.1 trills for CD ",    & !noneg
            r, " in year ", CURIYR," Itr ",CURITR, "to avoid 0 Q!!"        !noneg
        ELSE
          CMFinalEndUse(f,r,CURIYR) =                                    & !noneg
          max(CMFinalEndUse(f,r,CURIYR), 0.0)                              !noneg
        END IF ! Guard against non-positive fuel use                        noneg
       END DO
      END DO
		 
		 If (f.eq.4 .AND. CURIYR.EQ.28) then
			CMFinalEndUse(4,r,28)=CMFinalEndUse(4,r,27)
			end if
!  Compute environmental emissions from Commercial Buildings sector:

      ! Read emissions factors first time through only:
      IF ((CURIYR .EQ. CMFirstYr) .AND. (CURITR .EQ. 1)) THEN
!
!   Initialize array in case data set is incomplete
!
       DO f= 1, CMnumMajFl+CMnumMinFl+CMnumRenew
        DO e= 1, CMnumEmiss
         EmissionFactor (f,e) = 0.0
        END DO
       END DO

       infile= FILE_MGR ('O','KEMFAC            ',.FALSE.)
       READ (infile,'(99(/))')  ! skip over header
       IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KEMFAC data set error trapping:'
!
        count = 0
        NumErr = 0
        DO 110 f= 1, CMnumMajFl+CMnumMinFl+CMnumRenew
         count = count + 1
         READ (infile,*,ERR=109,END=111,IOSTAT=IOS) &
             (EmissionFactor (f,e), e= 1, CMnumEmiss)
         GO TO 110
!
 109     CONTINUE ! Report read error and loop to next read
         NumErr = NumErr + 1
         IF(PRTDBGK.EQ.1) &
           WRITE(RCDBG,*) 'Comm_KEMFAC read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 110     CONTINUE
         GO TO 112  ! EOF reached in KEMFAC when expected
 111     CONTINUE   ! EOF reached in KEMFAC prematurely; write msg
         NumErr = NumErr + 1
         IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KEMFAC EOF reached prematurely at f =',f
 112    CONTINUE
!
        infile= FILE_MGR ('C','KEMFAC            ',.FALSE.)
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,113)
 113      FORMAT(/,' KEMFAC data set error trapping complete!',/)
         ENDIF


      END IF  ! reading emissions factors first time through

      ! Compute emissions by fuel and region:
      ! Note: Consumption units are 10**12 BTU;
      !       Emission factor units are lbs/(10**6 BTU);
      !       Multiplication results in emission units of 10**6 lbs;
      !       Current desired emission units are million metric tons;
      !       There are 2204.62 lbs / M.T.;
      !       Therefore, a conversion factor of 2204.62 is used.
      DO 120 f= 1, CMnumMajFl+CMnumMinFl+CMnumRenew
       DO 120 e= 1, CMnumEmiss
        DO 120 r= 1, MNUMCR-2
         CMEmissions (f,e,r)= &
           CMFinalEndUse (f,r,CURIYR) * EmissionFactor (f,e) / &
           2204.62  ! MMLbs -> MM M.T.
  120 CONTINUE

      ! Total each emission type across fuel usage:
      DO 130 e= 1, CMnumEmiss
       DO 130 r= 1, MNUMCR-2
        ! initialize; total across fuels is stored in array position
        ! corresponding to the next fuel slot after the last fuel:
        CMEmissions (CMnumMajFl+CMnumMinFl+CMnumRenew+1,e,r)= 0.0
        DO 130 f= 1, CMnumMajFl+CMnumMinFl+CMnumRenew
         CMEmissions (CMnumMajFl+CMnumMinFl+CMnumRenew+1,e,r)= &
          CMEmissions (CMnumMajFl+CMnumMinFl+CMnumRenew+1,e,r) + &
          CMEmissions (f,e,r)
  130 CONTINUE

      ! Total each emission type across Census Divisions:
      DO 140 f= 1, CMnumMajFl+CMnumMinFl+CMnumRenew + 1
       DO 140 e= 1, CMnumEmiss
        ! initialize:
        CMEmissions (f,e,MNUMCR)= 0.0
        DO 140 r= 1, MNUMCR-2
         CMEmissions (f,e,MNUMCR)= &
          CMEmissions (f,e,MNUMCR) + CMEmissions (f,e,r)
  140 CONTINUE

!  Map Emissions to Global Data Structure:
      DO 142 e= 1, CMnumEmiss

       ! map US total emissions of each emission type by fuel:
       EMCM (1,e,CURIYR)= CMEmissions (2,e,MNUMCR) ! NG
       EMCM (2,e,CURIYR)= CMEmissions (3,e,MNUMCR)  & ! DS
                        + CMEmissions (4,e,MNUMCR)  & ! RS
                        + CMEmissions (5,e,MNUMCR)  & ! LP
                        + CMEmissions (7,e,MNUMCR)  & ! MG
                        + CMEmissions (8,e,MNUMCR) ! KR
       EMCM (3,e,CURIYR)= CMEmissions (6,e,MNUMCR) ! CL
       EMCM (4,e,CURIYR)= CMEmissions (9,e,MNUMCR) ! WD
       EMCM (5,e,CURIYR)= CMEmissions (10,e,MNUMCR) ! MSW

       ! map total emissions by Census Division:
       DO 146 r= 1, MNUMCR
        EMCMC (r,e,CURIYR)= CMEmissions (CMnumMajFl+CMnumMinFl+ &
                                               CMnumRenew+1,e,r)
  146  CONTINUE

  142  CONTINUE  ! Emissions mapping to GDS

      ! Report results if CURIYR is in the KPARM debug range:
      IF ( (PRTDBGK .EQ. 1)              .AND. &
           (BASEYR-1+CURIYR .GE. dbgy1)  .AND. &
           (BASEYR-1+CURIYR .LE. dbgy2) ) THEN

       WRITE (RCDBG,4000) (BASEYR-1)+CURIYR, CURITR, &
                          (CMEmissionName (e), e= 1, CMnumEmiss)
 4000  FORMAT (//,'1   Total Emissions by Census Division for ', &
                   I4,', Iteration ',I2,/, &
                  '    (Million Metric Tons)',///,24x,10(10x,A4))
       WRITE (RCDBG,'(//)')

       DO 4010 r= 1, MNUMCR
        IF (r .EQ. 10) GOTO 4010  ! CA not yet a separate Census Div
        WRITE (RCDBG,4020) CMRegions (r), &
         (CMEmissions (CMnumMajFl+CMnumMinFl+CMnumRenew+1,e,r), &
                                                e=1, CMnumEmiss)
 4020   FORMAT (4x,A20,10(1x,F13.3))
 4010  CONTINUE

      END IF  ! Emissions report

! Map fuel consumption to NEMS arrays.

         ! Initialize US totals to zero:
         QELCM(MNUMCR,CURIYR)= 0.0  ! Purchased Electricity
         QNGCM(MNUMCR,CURIYR)= 0.0  ! Natural Gas, Total
         QGFCM(MNUMCR,CURIYR)= 0.0  ! Natural Gas, Core
         QGICM(MNUMCR,CURIYR)= 0.0  ! Natural Gas, non-Core
         QDSCM(MNUMCR,CURIYR)= 0.0  ! Distillate
         QRSCM(MNUMCR,CURIYR)= 0.0  ! Residual Fuel
         QRLCM(MNUMCR,CURIYR)= 0.0  ! Resid, Low Sulfur
         QLGCM(MNUMCR,CURIYR)= 0.0  ! Liquid Petroleum Gases
         QCLCM(MNUMCR,CURIYR)= 0.0  ! Coal
         QMGCM(MNUMCR,CURIYR)= 0.0  ! Motor Gasoline
         QKSCM(MNUMCR,CURIYR)= 0.0  ! Kerosene
         QSTCM(MNUMCR,CURIYR)= 0.0  ! Solar Thermal
         QPVCM(MNUMCR,CURIYR)= 0.0  ! Solar Photovoltaic
         QBMCM(MNUMCR,CURIYR)= 0.0  ! Biomass
         y= CURIYR                  ! for brevity

        DO 150 r= 1, MNUMCR-2
         QELCM(r,y)= CMFinalEndUse (1,r,y)
          QELCM(MNUMCR,y)= QELCM(MNUMCR,y) + QELCM(r,y) ! Incr US Tot
         QNGCM(r,y)= CMFinalEndUse (2,r,y)
          QNGCM(MNUMCR,y)= QNGCM(MNUMCR,y) + QNGCM(r,y)
         QGFCM(r,y)= CMFinalEndUse (2,r,y) * 1.0  ! all firm
          QGFCM(MNUMCR,y)= QGFCM(MNUMCR,y) + QGFCM(r,y)
         QGICM(r,y)= CMFinalEndUse (2,r,y) * 0.0   ! no interruptable
          QGICM(MNUMCR,y)= QGICM(MNUMCR,y) + QGICM(r,y)
         QDSCM(r,y)= CMFinalEndUse (3,r,y)
          QDSCM(MNUMCR,y)= QDSCM(MNUMCR,y) + QDSCM(r,y)
         QRSCM(r,y)= CMFinalEndUse (4,r,y)
          QRSCM(MNUMCR,y)= QRSCM(MNUMCR,y) + QRSCM(r,y)
         QRLCM(r,y)= CMFinalEndUse (4,r,y)
          QRLCM(MNUMCR,y)= QRLCM(MNUMCR,y) + QRLCM(r,y)
         QLGCM(r,y)= CMFinalEndUse (5,r,y)
         QPRCM(r,y)=QLGCM(r,y)
          QLGCM(MNUMCR,y)= QLGCM(MNUMCR,y) + QLGCM(r,y)
         QCLCM(r,y)= CMFinalEndUse (6,r,y)
          QCLCM(MNUMCR,y)= QCLCM(MNUMCR,y) + QCLCM(r,y)
         QMGCM(r,y)= CMFinalEndUse (7,r,y)
          QMGCM(MNUMCR,y)= QMGCM(MNUMCR,y) + QMGCM(r,y)
         QKSCM(r,y)= CMFinalEndUse (8,r,y)
          QKSCM(MNUMCR,y)= QKSCM(MNUMCR,y) + QKSCM(r,y)
         QBMCM(r,y)= CMFinalEndUse (9,r,y) + &
                     CMFinalEndUse (10,r,y)
      
             IF (y .LE. MSEDYR) THEN        !Set estimate of historical biomass Q to correlate to history                   
!         QBMCM(r,y)= QBMCM(r,y) + &
!                     QSBMCM(r,y)           ! Add current year SEDS wood Q value to correlate to history
         DiffQBM(r)= QSBMCM(r,y) - QBMCM(r,y)        ! Calc difference between current year SEDS and NUGS biomass ebe83004
         QBMCM(r,y)= QSBMCM(r,y)           ! Set current year biomass equal to SEDS to correlate to history ebe83004
             ELSE
         IF (y .EQ. CMFirstYr) DiffQBM(r)= QSBMCM(r,MSEDYR) - QBMCM(r,y)
         IF (QBMCM(r,y) .LT. QSBMCM(r,MSEDYR)) QBMCM(r,y)= QBMCM(r,y) + &
                     DiffQBM(r)  ! Incorporate last SEDS biomass Q value to correlate to history ebe83004
             END IF                            !Incorporate estimate of historical biomass Q
          QBMCM(MNUMCR,y)= QBMCM(MNUMCR,y) + QBMCM(r,y)
         QSTCM(r,y)= ( SolarRenewableContrib (r,1,y) + &
                     SolarRenewableContrib (r,2,y) + &
                     SolarRenewableContrib (r,3,y) + &
                     SolarRenewableContrib (r,6,y) * &
                      (29.916 / SunlightEfficacy) ) * KFossilHR/3412.    !  - conversion to fossil fuel equivalent for renewables
                     ! 29.916 TRILLS PER GIGAWATTYEAR
          QSTCM(MNUMCR,y)= QSTCM(MNUMCR,y) + QSTCM(r,y)     !distgen

         QPVCM(r,y)= SolarPVContrib(y,r)    !distgen
          QPVCM(MNUMCR,y)= QPVCM(MNUMCR,y) + QPVCM(r,y)     !distgen
150     CONTINUE
      QPRCM(MNUMCR,y)= QLGCM(MNUMCR,y)

      DO 280 IREG= 1, MNUMCR-2
      DO 280 IFUEL= 1, CMnumAllFl
280     FinalUseTotal(IFUEL)= FinalUseTotal(IFUEL) + &
                              CMFinalEndUse (IFUEL,IREG,CURIYR)
      IF (PRTDBGK.EQ.1) THEN
       WRITE (RCQUAN,502) (BASEYR-1)+CURIYR, CURITR
       WRITE (RCQUAN,610)
!       TOTALFLR= 0.0  !can remove now that MAM doesn't output total floorspace
!       DO 172 IREG= 1, MNUMCR-2  !can remove now that MAM doesn't output total floorspace
!       TOTALFLR= MC_COMMFLSP (IREG,1,CURIYR)  +  TOTALFLR   !can remove now that MAM doesn't output total floorspace
!172    WRITE (RCQUAN,503) CMRegions(IREG),MC_COMMFLSP(IREG,1,CURIYR)/1000.- MC_COMMFLSP(IREG,8,CURIYR)/1000. !subtract mfg. flsp  !MFG no longer included; can remove now that MAM doesn't output total floorspace
!       WRITE (RCQUAN,511) TOTALFLR/1000.                                     !subtract mfg. flsp   !MFG no longer included; can remove now that MAM doesn't output total floorspace
       DO 173 IREG= 1, MNUMCR-2
173    WRITE (RCQUAN,504) CMRegions(IREG), MC_YPDR(IREG,CURIYR)
       DO 174 IREG= 1, MNUMCR-2
174    WRITE (RCQUAN,506)  CMRegions(IREG), MC_NP(IREG,CURIYR)
       WRITE (RCQUAN,507)  MC_RMGBLUSREAL(CURIYR)
       WRITE (RCQUAN,501) (BASEYR-1)+CURIYR, CURITR
       DO 170 IREG= 1, MNUMCR-2
        DO 170 IFUEL= 1, CMnumAllFl
170      WRITE (RCQUAN,505) CMRegions(IREG), CMAllFuels(IFUEL), &
                        CMFinalEndUse (IFUEL,IREG,CURIYR)
       DO 295 IFUEL= 1, CMnumAllFl
        WRITE (RCQUAN,509) (BASEYR-1)+CURIYR, CURITR, &
                  CMAllFuels(IFUEL), FinalUseTotal(IFUEL)
295   CONTINUE

      END IF


! *****FORMAT Statements
500   FORMAT ('>>>>> Subroutine COMBenchmarking called ', &
              I4,1X,'Iter ',I2)
501   FORMAT (/,' Commercial fuel quantities mapped to NEMS ', &
              I4,' Iter= ',I4)
502   FORMAT (' Macroeconomic data ',I4,' Iter ',I4)
610   FORMAT ('   Macro Floorspace in million square feet')
!503   FORMAT (' Floorspace for ',A20,1X,F9.3)  !can remove now that MAM doesn't output total floorspace
504   FORMAT (' Disposable income for ',A20,1X,F9.3)
505   FORMAT (' >> ',A20,1X,A20,1X,F9.2)
506   FORMAT (' Population for ',A20,1X,F9.4)
507   FORMAT (' Ten year Treasury bond rate ',F5.2)
508   FORMAT (' SEDS ',A20,1X,9(F7.2,1X))
509   FORMAT (1X,I4,' Iter=',I2,' National Total ',A20,1X,F9.2)
510   FORMAT (A20,1X,A20,1X,'CMNonBldgUse= ',F9.2,' (',I4,')')
!511   FORMAT ('  Macro National Floorspace ',9X,F9.3)  !can remove now that MAM doesn't output total floorspace
      RETURN

        CONTAINS

!====Distributed SR Elasticity Calculation Function
      REAL FUNCTION KELAST2*4 (F,R,S,ALPHA,EF1,EF2,EF3,elasbaseyr,report)

! This function returns the lagged (or spread out) short run elasticity
!  adjustment based on changes in fuel prices relative to the base year


      IMPLICIT NONE

      REAL*4  EF1,EF2,EF3    ! distributional shares for elasticity effects-should sum to 1.0
      REAL*4  ALPHA          ! total short run elasticity from input file
      REAL*4  FAC1,FAC2,FAC3 ! factors corresponding to elasticity effects of particular years
              
      INTEGER F,R,S

        INTEGER*4 elasbaseyr,report   ! link to kdebug output file handle

        FAC1=1. ; FAC2=1. ; FAC3=1.   ! Initialize

        IF (CURIYR.GE.elasbaseyr+1)THEN

         elasticity:  select case (F)    ! distribute SR price elasticity over 3 previous years
                                      ! using fuel specific variable names for prices
            case (1) elasticity      ! electricity
             IF (PELCMOUT(R,elasbaseyr,S) .GT. 0.0) THEN
                     FAC1=(PELCMOUT(R,CURIYR,S)/PELCMOUT(R,elasbaseyr,S))**(ALPHA*EF1)
                     FAC2=(PELCMOUT(R,CURIYR-1,S)/PELCMOUT(R,elasbaseyr,S))**(ALPHA*EF2)
                     FAC3=(PELCMOUT(R,CURIYR-2,S)/PELCMOUT(R,elasbaseyr,S))**(ALPHA*EF3)
                   END IF
              case (2) elasticity      ! natural gas
             IF (PNGCM(R,elasbaseyr) .GT. 0.0) THEN
                     FAC1=(PNGCM(R,CURIYR)/PNGCM(R,elasbaseyr))**(ALPHA*EF1)
                     FAC2=(PNGCM(R,CURIYR-1)/PNGCM(R,elasbaseyr))**(ALPHA*EF2)
                     FAC3=(PNGCM(R,CURIYR-2)/PNGCM(R,elasbaseyr))**(ALPHA*EF3)
                   END IF
              case (3) elasticity      ! distillate
                 IF (PDSCM(R,elasbaseyr) .GT. 0.0) THEN
                     FAC1=(PDSCM(R,CURIYR)/PDSCM(R,elasbaseyr))**(ALPHA*EF1)
                     FAC2=(PDSCM(R,CURIYR-1)/PDSCM(R,elasbaseyr))**(ALPHA*EF2)
                     FAC3=(PDSCM(R,CURIYR-2)/PDSCM(R,elasbaseyr))**(ALPHA*EF3)
                   END IF

         end select elasticity

         IF (CURIYR .LT. elasbaseyr+3) FAC3=1.
         IF (CURIYR .LT. elasbaseyr+2) FAC2=1.

        END IF

        KELAST2 = FAC1*FAC2*FAC3

!        WRITE(report,*)'SRElasAmt=',SRElasAmt, '  YEAR ',CURIYR, 
!        WRITE(report,*)' fuel ',F,

       RETURN
       END FUNCTION KELAST2


      END

!*****
      SUBROUTINE COMReport (RCDBG,RCRPT,SDReportOpts,ReportOption)     ! sw11-95
      IMPLICIT NONE
      INTEGER  FILE_MGR
      include'parametr'
      include'ncntrl'
      include'comparm'
      include'comvars'
      include'commrep'
      include'comout'
      include'eusprc' ! might consider writing this out for the ac price
      include'apq'

      ! REAL*4 TechShareofService (MNUMCR,CMnumBldg,CMnumServ,
      !                               CMnumTechs,CMnumEqVint)
      ! Proportion of a given service demand that is satisfied by
      ! equipment of a particular technology & vintage within a
      ! given Census Division, building type.  This array is
      ! initialized with 2012 CBECS shares (from file KTECH),
      ! and is computed for subsequent years.  For each forecast
      ! year it represents the market shares for the previous
      ! year, until it is recalculated for the current year in
      ! the Technology Choice subroutine.

      ! REAL*4 FuelShareofService (MNUMCR,CMnumBldg,CMnumServ,
      !                                           CMnumMajFl)
      ! For Census Division r, building type b, service s, and
      ! fuel f, FuelShareofService (r,b,s,f) is the proportion
      ! of service demand for service s in b,r that is satisfied
      ! by fuel f in the previous year, until updated for the
      ! current year in the Technology Choice subroutine.

      ! REAL*4 AverageEfficiency (MNUMCR,CMnumBldg,CMnumServ,
      !                                          CMnumMajFl)
      ! AverageEfficiency (r,b,s,f) is the effective average
      ! efficiency of the equipment mix using fuel f to satisfy
      ! service demand s in building type b of Census Division r in
      ! the current year, as calculated in the Technology Choice
      ! subroutine.

      ! REAL*4 PrevYrTechShareofService (MNUMCR,CMnumBldg,
      !                 CMnumServ,CMnumTechs,CMnumEqVint)
      ! Proportion of a given service demand that was satisfied by
      ! equipment of a particular technology & vintage within a
      ! given Census Division, and building type during the
      ! previous year.

      ! REAL*4 PrevYrFuelShareofService (MNUMCR,CMnumBldg,
      !                             CMnumServ,CMnumMajFl)
      ! For Census Division r, building type b, service s, and
      ! fuel f, PrevYrFuelShareofService (r,b,s,f) is the proportion
      ! of service demand for service s in b,r that was satisfied
      ! by fuel f during the previous year.

      ! REAL*4 PrevYrAverageEfficiency (MNUMCR,CMnumBldg,
      !                                CMnumServ,CMnumMajFl)
      ! PrevYrAverageEfficiency (r,b,s,f) is the effective average
      ! efficiency of the equipment mix using fuel f to satisfy
      ! service demand s in building type b of Census Division r
      ! during the previous year.

      ! REAL*4 DecFuelShare (MNUMCR,CMnumServ,CMDecision,CMnumMajFl,
      !                                                    MNUMYR)
      ! For Census Division r, service s, and fuel f,
      ! DecFuelShare (r,s,d,f,y) is the proportion of service
      ! demand for service s in region r subject to decision type d
      ! that is satisfied by fuel f in year y.

      ! REAL*4 DecAvgEff (MNUMCR,CMnumServ,CMDecision,CMnumMajFl,
      !                                                 MNUMYR)
      ! DecAvgEff (r,s,d,f,y) is the effective average
      ! efficiency of the equipment mix selected in decision segment
      ! d using fuel f to satisfy service demand s in Census Division
      ! r in year y.

      ! REAL*4 NSD (MNUMCR,CMnumBldg,CMnumServ,MNUMYR)
      ! New service demand associated with new floor space.
      ! Will replace NewServDmd in CMServDmdNw common block in
      ! Compar2 include file.

      ! REAL*4 RSD (MNUMCR,CMnumBldg,CMnumServ,MNUMYR)
      ! Replacement service demand associated with retiring equipment.
      ! Will replace RetireServDmd in CMServDmdNw common block in
      ! Compar2 include file.

      ! REAL*4 SSD (MNUMCR,CMnumBldg,CMnumServ,MNUMYR)
      ! Surviving service demand subject to retrofit decisions.
      ! Will replace ServDmdSurv in CMServDmdNw common block in
      ! Compar2 include file.

      ! INTEGER*4 FuelbyTech (CMnumTechs,CMnumMajFl)
      ! Set to 1 if tech (row) uses fuel (col); 0 otherwise

      ! INTEGER*4 TechbyService (CMnumServ,CMnumTechs)
      ! Set to 1 if service (row) provided by tech (col); 0 otherwise

      ! INTEGER*4 CMnumTechsforService (CMnumServ)
      ! For service s, CMnumTecsforService (s) is the number of
      ! technologies input from the technology characterization
      ! file, KTECH, that provide that service.

      ! INTEGER*4 TechsforService (CMnumServ,CMnumTechs)
      ! For service s, TechsforService (s,TsubS) contains the
      ! technology subscript, t, of a technology providing
      ! service s, for each value of TsubS between 1 and
      ! CMnumTechsforService (s).  The value of t is that assigned
      ! in KTECH, and will be between 1 and CMnumTechs.

      ! REAL*4 TechEff (MNUMCR,CMnumServ,CMnumTechs,CMnumEqVint)
      ! Efficiencies of specific equipment, with allowance for
      ! regional variation, and equipment use for multiple services

      ! REAL*4 TechCost (MNUMCR,CMnumTechs,CMnumEqVint,4) - in comparm
      ! For equipment of technology t and vintage v in region r:
      ! TechCost(r,t,v,1) = installed capital cost per unit service demand
      ! TechCost(r,t,v,2) = annual O&M cost per unit service demand
      ! TechCost(r,t,v,3) = subsidy cost per unit service demand           !investsub
      ! TechCost(r,t,v,4) = 111d subsidy cost per unit service demand      !investsub 111(d)

      ! REAL*4 TechLife (CMnumTechs,CMnumEqVint)
      ! Average life expectancy of equipment, in years

      ! INTEGER*4 TechAvailability (MNUMCR,CMnumTechs,CMnumEqVint,2)
      ! For equipment of technology t and vintage v:
      ! TechAvailability(r,t,v,1) = calendar year first available to buy  !regionalize tech availability
      ! TechAvailability(r,t,v,2) = last year of equipment availability  !regionalize tech availability

      ! REAL*4 BehaviorShare (CMnumServ,CmnumBldg,CMDecision,CMnumRule)
      ! From file KBEHAV;
      ! For technology choice decision type d in building type b,
      ! and service type s,
      ! BehaviorShare (s,b,d,r) is the proportion of consumers following
      ! behavior rule r during the equipment selection process.

      ! REAL*4 CapacityFactor (MNUMCR-2,CMnumBldg,CMnumServ)
      ! From file KCAPFAC
      ! Capacity factors by region, building type, and service


      integer*4 SDReportOpts(2)                                        ! sw10-95
      INTEGER*4 RCODE1, RC1, RC2, RCL1, RCL2, RCDBG, RCRPT
      INTEGER*4 IREG, ISERV, IFUEL, IBLDTP, I, I2, I3, I4, I5, &
                IVINT, I6, p
      REAL*4 TOTAL1(MNUMCR), TOTAL2(MNUMCR), TOTAL3(MNUMCR), &
             TOTAL11(MNUMCR,CMnumServ), TOTAL12(MNUMCR,CMnumServ), &
             TOTAL13(MNUMCR,CMnumServ), &
             TOTAL21(MNUMCR,CMnumServ,CMnumMajFl), &
             TOTAL22(MNUMCR,CMnumServ,CMnumMajFl), &
             TOTAL23(MNUMCR,CMnumServ,CMnumMajfl), &
             NTOTAL1, NTOTAL2, NTOTAL3, NYTOTAL1(MNUMYR), &
             NYTOTAL2(MNUMYR), NYTOTAL3(MNUMYR), &
             YTOTAL1(MNUMCR,MNUMYR),YTOTAL2(MNUMCR,MNUMYR), &
             YTOTAL3(MNUMCR,MNUMYR),YTOTAL4(MNUMCR,MNUMYR), &
             YTOTAL5(MNUMCR,MNUMYR),YTOTAL6(MNUMCR), &
             YTOTAL7(MNUMCR,MNUMYR)
!     Following 2 variables are for reporting unbenchmarked consump
!     excluding nonbldg consumption and nonutility generation
      REAL*4 UnBenchedCon (CMnumMajFl+CMnumMinFl+CMnumRenew, &
                                      CMnumBldg,MNUMCR,MNUMYR)
      REAL*4 CMFinalUnbenchUse (CMnumMajFl+CMnumMinFl+CMnumRenew, &
                                                MNUMCR,MNUMYR)
      COMMON /CMCONBK1/ UnBenchedCon, CMFinalUnbenchUse

      CHARACTER*10 ServName(3), RuleName (3)
      CHARACTER*11 Decision(3)
      REAL*4 TEMP1, TEMP2, TEMP3
      REAL*4 TEMP4(MNUMYR),TEMP5(MNUMYR),TEMP6(MNUMYR)
!  Holding variables for system error workaround:
      REAL ConsumpByFBRy11to20 (10,CMnumMajFl,CMnumBldg,MNUMCR-2)
      REAL ConsumpByFBRy21to26 (11,CMnumMajFl,CMnumBldg,MNUMCR-2)
      INTEGER*2 IYR   ! year index, 1 = 1990
      INTEGER*4 r,       & !  Region (Census Division)
                b,       & !  Building type
                s,       & !  Service
                f,       & !  Fuel
                y       !  Year (1 = 1990)
      DATA ServName /'Retiring','Surviving','New'/
      DATA Decision /'New','Replacement','Retrofit'/
      DATA RuleName /'Least Cost','Same Fuel','Same Tech'/
      INTEGER*4 dbfile     ! COMM database output file handle
      Character*20 VarName ! Name of KDBOUT output variable
      REAL*4 VarVal        ! Value of KDBOUT output variable
      INTEGER*4 sdfile  ! Equip Service Demand output file handle
      INTEGER*4 TsubS   ! Index into TechsforService array
      INTEGER*4 t       ! Technology index
      INTEGER*4 v       ! Equipment model index
      INTEGER*4 d       ! Decision type index
      INTEGER*4 maxreg  ! maximum regions for reporting                sw10-95
      INTEGER*4 minreg  ! minimum regions for reporting                sw10-95
      INTEGER*4 maxdec  ! maximum decision types for reporting         sw10-95
      INTEGER*4 mindec  ! minimum decision types for reporting         sw10-95
      INTEGER*4 ReportOption ! KRPT reporting switch                   sw11-95

!     Variables for miscellaneous electricity detail
      COMMON /MiscEl/ CoffeeBrewers,XfmrsDry,Security,ElVehicles,        & ! miscdetail
                 KitchenVent,LabRefFrz,VidDisplay,LrgVidBoard,           & ! miscdetail
                 FumeHoods,Laundry,MedImaging,Elevators,Escalators,      & ! miscdetail
                 xmisccalc                                                 ! miscdetail
      REAL*4    CoffeeBrewers,XfmrsDry,Security,ElVehicles,              & ! miscdetail
                 KitchenVent,LabRefFrz,VidDisplay,LrgVidBoard,           & ! miscdetail
                 FumeHoods,Laundry,MedImaging,Elevators,Escalators         ! miscdetail
      REAL*4    xmisccalc    ! holding variable for curiyr-11 to use in misc equations

      COMMON /MiscElQ/ CoffeeBrewersElQ,XfmrsDryElQ,SecurityElQ,         & ! miscdetail
                 KitchenVentElq,LabRefFrzElq,LrgVidBoardElq, & ! miscdetail
                 ElVehiclesElQ,FumeHoodsElQ,LaundryElQ,MedImagingElQ,    & ! miscdetail
                 ElevatorsElQ,EscalatorsElQ                                ! miscdetail
      REAL*4 CoffeeBrewersElQ(MNUMCR-2,CMnumBldg,MNUMYR)                   ! miscdetail
      REAL*4 XfmrsDryElQ(MNUMCR-2,CMnumBldg,MNUMYR)                        ! miscdetail
      REAL*4 SecurityElQ(MNUMCR-2,CMnumBldg,MNUMYR)                        ! miscdetail
      REAL*4 ElVehiclesElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 KitchenVentElq(MNUMCR-2,CMnumBldg,MNUMYR)                     ! miscdetail
      REAL*4 LabRefFrzElQ(MNUMCR-2,CMnumBldg,MNUMYR)                       ! miscdetail
      !REAL*4 VidDisplayElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 LrgVidBoardElQ(MNUMCR-2,CMnumBldg,MNUMYR)                     ! miscdetail
      REAL*4 FumeHoodsElQ(MNUMCR-2,CMnumBldg,MNUMYR)                       ! miscdetail
      REAL*4 LaundryElQ(MNUMCR-2,CMnumBldg,MNUMYR)                         ! miscdetail
      REAL*4 MedImagingElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      REAL*4 ElevatorsElQ(MNUMCR-2,CMnumBldg,MNUMYR)                       ! miscdetail
      REAL*4 EscalatorsElQ(MNUMCR-2,CMnumBldg,MNUMYR)                      ! miscdetail
      COMMON /Misc2/ WaterServicesElQ                                      ! miscdetail
      REAL*4 WaterServicesElQ(MNUMCR-2,MNUMYR)                             ! miscdetail
 
       IF (ReportOption.eq.0) goto 999
       WRITE (RCRPT,530) SCEN, DATE

!
!  EQUIPMENT INVESTMENT COSTS BY REGION, SERVICE AND YEAR
      WRITE (RCRPT,561) iKtechCostyr + 1989, (CBECSyear+I, I= 1, 12)
      DO 566 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions (IREG)
        DO 566 ISERV= 1, CMnumMajServ
         WRITE (RCRPT,562) CMServices (ISERV), " Capital Invest", &        !investsub
            (InvestCost (IREG,ISERV,I4), I4= CMFirstYr, CMFirstYr+11)
         WRITE (RCRPT,562) CMServices (ISERV), " Subsidy Invest", &        !investsub
            (SubsidyInvestCost (IREG,ISERV,I4), I4= CMFirstYr, CMFirstYr+11)
         WRITE (RCRPT,562) CMServices (ISERV), " Sub111d Invest", &        !inestsub 111(d)
            (Subsidy111dInvestCost (IREG,ISERV,I4), I4= CMFirstYr, CMFirstYr+11)
566   CONTINUE

      WRITE (RCRPT,557)
      DO 567 ISERV= 1, CMnumMajServ
         WRITE (RCRPT,562) CMServices (ISERV), " Capital Invest", &        !investsub
            (InvestCost (11,ISERV,I4), I4= CMFirstYr, CMFirstYr+11)
         WRITE (RCRPT,562) CMServices (ISERV), " Subsidy Invest", &        !investsub
            (SubsidyInvestCost (11,ISERV,I4), I4= CMFirstYr, CMFirstYr+11)
         WRITE (RCRPT,562) CMServices (ISERV), " Sub111d Invest", &        !investsub 111(d)
            (Subsidy111dInvestCost (11,ISERV,I4), I4= CMFirstYr, CMFirstYr+11)
567   CONTINUE

      IF (LASTYR .GT. CMFirstYr+11) THEN
      WRITE (RCRPT,561) iKtechCostyr + 1989, (CBECSyear+I, I= 13, 24)
      DO 568 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions (IREG)
        DO 568 ISERV= 1, CMnumMajServ
         WRITE (RCRPT,562) CMServices (ISERV), " Capital Invest", &        !investsub
            (InvestCost (IREG,ISERV,I4), I4= CMFirstYr+12, CMFirstYr+23)
         WRITE (RCRPT,562) CMServices (ISERV), " Subsidy Invest", &        !investsub
            (SubsidyInvestCost (IREG,ISERV,I4), I4= CMFirstYr+12, CMFirstYr+23)
         WRITE (RCRPT,562) CMServices (ISERV), " Sub111d Invest", &        !investsub 111(d)
            (Subsidy111dInvestCost (IREG,ISERV,I4), I4= CMFirstYr+12, CMFirstYr+23)
568   CONTINUE

      WRITE (RCRPT,557)
      DO 569 ISERV= 1, CMnumMajServ
         WRITE (RCRPT,562) CMServices (ISERV), " Capital Invest", &        !investsub
            (InvestCost (11,ISERV,I4), I4= CMFirstYr+12, CMFirstYr+23)
         WRITE (RCRPT,562) CMServices (ISERV), " Subsidy Invest", &        !investsub
            (SubsidyInvestCost (11,ISERV,I4), I4= CMFirstYr+12, CMFirstYr+23)
         WRITE (RCRPT,562) CMServices (ISERV), " Sub111d Invest", &        !investsub 111(d)
            (Subsidy111dInvestCost (11,ISERV,I4), I4= CMFirstYr+12, CMFirstYr+23)
569   CONTINUE

      IF (LASTYR .GT. CMFirstYr+23) THEN
      WRITE (RCRPT,561) iKtechCostyr + 1989, (CBECSyear+I, I= 25, LASTYR-CMFirstYr+1)
      DO 563 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions (IREG)
        DO 563 ISERV= 1, CMnumMajServ
         WRITE (RCRPT,562) CMServices (ISERV), " Capital Invest", &        !investsub
            (InvestCost (IREG,ISERV,I4), I4= CMFirstYr+24, LASTYR)
         WRITE (RCRPT,562) CMServices (ISERV), " Subsidy Invest", &        !investsub
            (SubsidyInvestCost (IREG,ISERV,I4), I4= CMFirstYr+24, LASTYR)
         WRITE (RCRPT,562) CMServices (ISERV), " Sub111d Invest", &        !investsub 111(d)
            (Subsidy111dInvestCost (IREG,ISERV,I4), I4= CMFirstYr+24, LASTYR)
563   CONTINUE

      WRITE (RCRPT,557)
      DO 564 ISERV= 1, CMnumMajServ
         WRITE (RCRPT,562) CMServices (ISERV), " Capital Invest", &        !investsub
            (InvestCost (11,ISERV,I4), I4= CMFirstYr+24, LASTYR)
         WRITE (RCRPT,562) CMServices (ISERV), " Subsidy Invest", &        !investsub
            (SubsidyInvestCost (11,ISERV,I4), I4= CMFirstYr+24, LASTYR)
         WRITE (RCRPT,562) CMServices (ISERV), " Sub111d Invest", &        !investsub 111(d)
            (Subsidy111dInvestCost (11,ISERV,I4), I4= CMFirstYr+24, LASTYR)
564   CONTINUE
      END IF    !LASTYR GREATER THAN CMFirstYr+23
      END IF    !LASTYR GREATER THAN CMFirstYr+11
        WRITE (RCRPT,'(/)')

!
       ! Calculate and report historical year CBECS-SEDS misties,
       !  and CBECS-STEO:
!       DO 116 y= CMFirstYr, KSTEOYR + 1                     ! sides
       DO 116 y= CMFirstYr, KSTEOYR

       DO 125 IFUEL= 1, CMnumAllFl
        CMFinalEndUse (IFUEL,11,y)= 0.0 ! Initialize
        DO 125 IREG= 1, MNUMCR-2
125      CMFinalEndUse(IFUEL,11,y)= &
          CMFinalEndUse(IFUEL,11,y)  + &
           CMFinalEndUse(IFUEL,IREG,y)

      ! National Nonbuilding use by fuel:
      DO 5 IFUEL= 1, CMnumMajFl
       CMNonBldgUse (IFUEL,MNUMCR,y)= 0.0
       DO 5 IREG= 1, MNUMCR-2
        CMNonBldgUse (IFUEL,MNUMCR,y)= CMNonBldgUse (IFUEL,MNUMCR,y) + &
             CMNonBldgUse (IFUEL,IREG,y)
    5 CONTINUE

      ! National Nonbuilding use by Census Division:
      DO 10 IREG= 1, MNUMCR
       TOTAL1 (IREG)= 0.0
       DO 10 IFUEL= 1, CMnumMajFl
        TOTAL1 (IREG)= TOTAL1 (IREG) + CMNonBldgUse (IFUEL,IREG,y)
   10 CONTINUE


! SEDS - CBECS Mistie:
      WRITE (RCRPT,529) (BASEYR-1)+y
       DO 115 IREG= 1, MNUMCR
        IF (IREG .EQ. 10) GOTO 115  ! CA not yet a Census Division
        WRITE(RCRPT,528) CMRegions(IREG),(CMNonBldgUse(IFUEL,IREG,y), &
                         IFUEL= 1, CMnumMajFl), TOTAL1 (IREG)
  115  CONTINUE
  116  CONTINUE

! SEDS Data
      DO 247 y= CMFirstYr, KSTEOYR
      WRITE (RCRPT,527) (BASEYR-1)+y
      DO 114 IREG= 1, MNUMCR-2
       WRITE (RCRPT,525) CMRegions (IREG)
        DO 114 IFUEL= 1, CMnumAllFl
         TOTAL1(IFUEL)= 0.0
114      WRITE (RCRPT,526) CMAllFuels(IFUEL), CMSEDS (IFUEL,IREG,y)

      DO 245 IREG= 1, MNUMCR-2
       DO 245 IFUEL= 1, CMnumAllFl
245     TOTAL1 (IFUEL)= TOTAL1 (IFUEL) + CMSEDS (IFUEL,IREG,y)
      WRITE (RCRPT,560)
      DO 246 IFUEL= 1, CMnumAllFl
246    WRITE (RCRPT,526) CMAllFuels(IFUEL), TOTAL1(IFUEL)
 247  CONTINUE

!      DO 234 s= 1, CMnumMajServ ! Major Services only
!       WRITE (RCRPT,'(/,1X,A18)') CMServices (s)
!      DO 234 I= 1, 3 ! Decision Type (New, Replacement, Retrofit)
!        WRITE (RCRPT,550) Decision (I)
!         DO 234 IBLDTP= 1, CMnumBldg
!          WRITE (RCRPT,551) BldgName (IBLDTP),
!     *      (BehaviorShare (s,IBLDTP,I,I2), I2= 1, 3)
!234   CONTINUE
! End of Behavior Rules

! Risk Premiums and Proportions
!      WRITE (RCRPT,552)
!       DO 235 y= CMFirstYr, LASTYR
!       DO 235 s= 1, CMnumMajServ
!        WRITE (RCRPT,'(1X,I4,1X,A20)') y+(BASEYR-1), CMServices (s)
!      DO 235 p= 1, CMnumPref
!235    WRITE (RCRPT,553)
!     *     TimePrefProp (s,p,y), TimePrefPrem (s,p,y)
!C End of Risk Premiums and Proportions

! Floorspace
      WRITE (RCRPT,533) (CBECSyear+I-1, I=1,13)

      DO 203 I= CMFirstYr, LASTYR
       NYTOTAL1(I)= 0.0
       NYTOTAL2(I)= 0.0
       NYTOTAL3(I)= 0.0
       TEMP4(I)=    0.0
       TEMP5(I)=    0.0
       TEMP6(I)=    0.0
       YTOTAL7(11,I)= 0.0
       IF (I.EQ.CMFirstYr) TEMP1= 0.0
      DO 203 IREG= 1, MNUMCR-2
       YTOTAL1(IREG,I)= 0.0
       YTOTAL2(IREG,I)= 0.0
       YTOTAL3(IREG,I)= 0.0
       YTOTAL4(IREG,I)= 0.0
       YTOTAL5(IREG,I)= 0.0
       YTOTAL7(IREG,I)= 0.0 ! Gross New in year t
       IF (I.EQ.CMFirstYr) YTOTAL6(IREG)= 0.0
       DO 202 IBLDTP= 1, CMnumBldg
        YTOTAL1(IREG,I)= CMNewFloorSpace(IREG,IBLDTP,I) + &
                         YTOTAL1 (IREG,I)
        YTOTAL2(IREG,I)= SurvFloorTotal(IREG,IBLDTP,I)+YTOTAL2(IREG,I)
        YTOTAL4(IREG,I)= AgedNew (IREG,IBLDTP,I)+YTOTAL4(IREG,I)
         DO 244 I2= CMFirstYr, I
244      YTOTAL7(IREG,I)= YTOTAL7(IREG,I) &
                        + CMNewFloorSpace(IREG,IBLDTP,I2)
         DO 236 IVINT= 1, CMnumBldgVint
          IF (I.EQ.CMFirstYr) YTOTAL6(IREG)= YTOTAL6(IREG) &
                        + CBECSFlrSpc(IREG,IBLDTP,IVINT)
236      CONTINUE
202    CONTINUE
          IF (I .EQ. CMFirstYr) THEN
           YTOTAL4(IREG,I)= 0.0
          END IF
!      Aged CBECS = Total Surviving - Aged New
       YTOTAL5(IREG,I)= YTOTAL2(IREG,I) - YTOTAL4(IREG,I)
       YTOTAL3(IREG,I)= YTOTAL1(IREG,I) + YTOTAL2(IREG,I)
       NYTOTAL1(I)= NYTOTAL1(I) + YTOTAL1(IREG,I)
       NYTOTAL2(I)= NYTOTAL2(I) + YTOTAL2(IREG,I)
       NYTOTAL3(I)= NYTOTAL3(I) + YTOTAL3(IREG,I)
       TEMP4(I)= TEMP4(I) + YTOTAL4(IREG,I)
       TEMP5(I)= TEMP5(I) + YTOTAL5(IREG,I)
       YTOTAL7(11,I)= YTOTAL7(11,I) + YTOTAL7(IREG,I)
       IF (I.EQ.CMFirstYr) TEMP1= TEMP1 + YTOTAL6(IREG)

203    CONTINUE

      DO 201 IREG= 1, MNUMCR-2
201   WRITE (RCRPT,554) CMRegions(IREG),               &
       (YTOTAL1(IREG,I), I=CMFirstYr,CMFirstYr+11),    &
       (YTOTAL7(IREG,I6), I6= CMFirstYr,CMFirstYr+11), &
        YTOTAL6(IREG), &
       (YTOTAL5(IREG,I5), I5= CMFirstYr,CMFirstYr+11), &
       (YTOTAL4(IREG,I4), I4= CMFirstYr,CMFirstYr+11), &
       (YTOTAL2(IREG,I2), I2= CMFirstYr,CMFirstYr+11), &
        YTOTAL6(IREG), (YTOTAL3(IREG,I3), I3= CMFirstYr,CMFirstYr+11)
      WRITE (RCRPT,555) (NYTOTAL1(I),I= CMFirstYr,CMFirstYr+11), &
         (YTOTAL7(11,I6), I6= CMFirstYr,CMFirstYr+11), &
         TEMP1, (TEMP5(I5),I5= CMFirstYr,CMFirstYr+11),&
         (TEMP4(I4),I4= CMFirstYr,CMFirstYr+11),       &
         (NYTOTAL2(I2),I2= CMFirstYr,CMFirstYr+11),    &
         TEMP1, (NYTOTAL3(I3),I3= CMFirstYr,CMFirstYr+11)

      IF (LASTYR .GT. CMFirstYr+11) THEN
      WRITE (RCRPT,533) (CBECSyear+I, I=13,25)
      DO 204 IREG= 1, MNUMCR-2
204   WRITE (RCRPT,534) CMRegions(IREG), &
        (YTOTAL1(IREG,I),  I=  CMFirstYr+12,CMFirstYr+24), &
        (YTOTAL7(IREG,I6), I6= CMFirstYr+12,CMFirstYr+24), &
        (YTOTAL5(IREG,I4), I4= CMFirstYr+12,CMFirstYr+24), &
        (YTOTAL4(IREG,I5), I5= CMFirstYr+12,CMFirstYr+24), &
        (YTOTAL2(IREG,I2), I2= CMFirstYr+12,CMFirstYr+24), &
        (YTOTAL3(IREG,I3), I3= CMFirstYr+12,CMFirstYr+24)
      WRITE (RCRPT,535) (NYTOTAL1(I),I= CMFirstYr+12,CMFirstYr+24), &
         (YTOTAL7(11,I6), I6= CMFirstYr+12,CMFirstYr+24),  &
         (TEMP5(I5),I5= CMFirstYr+12,CMFirstYr+24),        &
         (TEMP4(I4),I4= CMFirstYr+12,CMFirstYr+24),        &
         (NYTOTAL2(I2),I2= CMFirstYr+12,CMFirstYr+24),     &
         (NYTOTAL3(I3),I3= CMFirstYr+12,CMFirstYr+24)

      IF (LASTYR .GT. CMFirstYr+24) THEN
      WRITE (RCRPT,533) (CBECSyear+I, I= 26, LASTYR-CMFirstYr+1)

      DO 205 IREG= 1, MNUMCR-2
205   WRITE (RCRPT,536) CMRegions(IREG), &
        (YTOTAL1(IREG,I),  I=  CMFirstYr+25,LASTYR), &
        (YTOTAL7(IREG,I6), I6= CMFirstYr+25,LASTYR), &
        (YTOTAL5(IREG,I4), I4=  CMFirstYr+25,LASTYR), &
        (YTOTAL4(IREG,I5), I5=  CMFirstYr+25,LASTYR), &
        (YTOTAL2(IREG,I2), I2= CMFirstYr+25,LASTYR), &
        (YTOTAL3(IREG,I3), I3= CMFirstYr+25,LASTYR)
      WRITE (RCRPT,537) (NYTOTAL1(I),I= CMFirstYr+25,LASTYR), &
         (YTOTAL7(11,I6), I6= CMFirstYr+25,LASTYR), &
         (TEMP5(I5),I5= CMFirstYr+25,LASTYR), &
         (TEMP4(I4),I4= CMFirstYr+25,LASTYR), &
         (NYTOTAL2(I2),I2= CMFirstYr+25,LASTYR), &
         (NYTOTAL3(I3),I3= CMFirstYr+25,LASTYR)
      END IF     !LASTYR GREATER THAN CMFirstYr+24
      END IF     !LASTYR GREATER THAN CMFirstYr+11

! End of Table A1

! Longitude Average Efficiencies
      WRITE (RCRPT,541) (CBECSyear+I, I=1,12)
      DO 1209 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions(IREG)
       DO 1209 ISERV= 1, CMnumServ
        WRITE (RCRPT,516) CMServices(ISERV)
        DO 209 I= 1,3
         WRITE (RCRPT,540) Decision (I)
         DO 209 IFUEL= 1, CMnumMajFl
209       WRITE (RCRPT,538) CMAllFuels(IFUEL), &
           (DecAvgEff (IREG,ISERV,I,IFUEL,I2), I2=CMFirstYr,CMFirstYr+11)
1209   CONTINUE

      IF (LASTYR .GT. CMFirstYr+11) THEN
      WRITE (RCRPT,541) (CBECSyear+I, I= 13, 24)
      DO 1210 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions(IREG)
       DO 1210 ISERV= 1, CMnumServ
        WRITE (RCRPT,516) CMServices(ISERV)
        DO 210 I= 1,3
         WRITE (RCRPT,540) Decision (I)
         DO 210 IFUEL= 1, CMnumMajFl
 210      WRITE (RCRPT,538) CMAllFuels(IFUEL), &
           (DecAvgEff (IREG,ISERV,I,IFUEL,I2), &
                                          I2=CMFirstYr+12,CMFirstYr+23)
1210   CONTINUE

      IF (LASTYR .GT. CMFirstYr+23) THEN
      WRITE (RCRPT,541) (CBECSyear+I, I= 25, LASTYR-CMFirstYr+1)
      DO 1211 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions(IREG)
       DO 1211 ISERV= 1, CMnumServ
        WRITE (RCRPT,516) CMServices(ISERV)
        DO 211 I= 1,3
         WRITE (RCRPT,540) Decision (I)
         DO 211 IFUEL= 1, CMnumMajFl
  211     WRITE (RCRPT,538) CMAllFuels(IFUEL), &
          (DecAvgEff (IREG,ISERV,I,IFUEL,I2), &
                                         I2=CMFirstYr+24,LASTYR)
 1211  CONTINUE
      END IF     !LASTYR GREATER THAN CMFirstYr+23
      END IF     !LASTYR GREATER THAN CMFirstYr+11

! End of Longitude Average Efficiencies

! Longitude Benchmarked End Use Consumption
       DO 278 IFUEL= 1, CMnumAllFl
        DO 278 I= CMFirstYr, CMLastYr
278      CMFinalEndUse(IFUEL,11,I)= 0.0

      DO 212 IREG= 1, MNUMCR-2
       DO 212 IFUEL= 1, CMnumAllFl
        DO 212 I= CMFirstYr, CMLastYr
212      CMFinalEndUse(IFUEL,11,I)= &
          CMFinalEndUse(IFUEL,11,I)  + &
           CMFinalEndUse(IFUEL,IREG,I)

      WRITE (RCRPT,542) (CBECSyear+I, I= 1, 12)
      DO 213 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions(IREG)
        DO 213 IFUEL= 1, CMnumAllFl
213      WRITE (RCRPT,543) CMAllFuels(IFUEL), &
          (CMFinalEndUse(IFUEL,IREG,I), I= CMFirstYr, CMFirstYr+11)
        WRITE (RCRPT,546)
        DO 214 IFUEL= 1, CMnumAllFl
214     WRITE (RCRPT,543) CMAllFuels(IFUEL), &
        (CMFinalEndUse(IFUEL,11,I),I=CMFirstYr,CMFirstYr+11)

      IF (LASTYR .GT. CMFirstYr+11) THEN
      WRITE (RCRPT,542) (CBECSyear+I, I= 13, 24)
      DO 215 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions(IREG)
        DO 215 IFUEL= 1, CMnumAllFl
215      WRITE (RCRPT,543) CMAllFuels(IFUEL), &
          (CMFinalEndUse(IFUEL,IREG,I), I= CMFirstYr+12, CMFirstYr+23)
        WRITE (RCRPT,546)
        DO 216 IFUEL= 1, CMnumAllFl
216     WRITE (RCRPT,543) CMAllFuels (IFUEL), &
            (CMFinalEndUse(IFUEL,11,I),I=CMFirstYr+11,CMFirstYr+23)
      IF (LASTYR .GT. CMFirstYr+23) THEN
      WRITE (RCRPT,542) (CBECSyear+I, I= 25, LASTYR-CMFirstYr+1)
      DO 217 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions(IREG)
        DO 217 IFUEL= 1, CMnumAllFl
217      WRITE (RCRPT,543) CMAllFuels(IFUEL), &
            (CMFinalEndUse(IFUEL,IREG,I), I= CMFirstYr+24, LASTYR)
        WRITE (RCRPT,546)
        DO 218 IFUEL= 1, CMnumAllFl
218     WRITE (RCRPT,543) CMAllFuels(IFUEL), &
            (CMFinalEndUse(IFUEL,11,I),I=CMFirstYr+24,LASTYR)
      END IF     !LASTYR GREATER THAN CMFirstYr+23
      END IF     !LASTYR GREATER THAN CMFirstYr+11

! End of Longitude Benchmarked End Use Consumption

! Longitude Non-Benchmarked End-Use Consumption
      DO 255 IFUEL= 1, CMnumMajFl
       DO 255 I= CMFirstYr, LASTYR
      CMFinalUnbenchUse(IFUEL,11,I)= 0.0
255   CMFinalEndUse(IFUEL,11,I)= 0.0
      DO 219 IREG= 1, MNUMCR-2
       DO 219 IFUEL= 1, CMnumMajFl
        DO 219 I= CMFirstYr, LASTYR
         CMFinalUnbenchUse(IFUEL,11,I)= &
          CMFinalUnbenchUse(IFUEL,11,I) + &
           CMFinalUnbenchUse(IFUEL,IREG,I)
219      CMFinalEndUse(IFUEL,11,I)= &
          CMFinalEndUse(IFUEL,11,I)  + &
           CMFinalEndUse(IFUEL,IREG,I)
      WRITE (RCRPT,545) (CBECSyear+I, I= 1, 12)
      DO 220 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions(IREG)
        DO 220 IFUEL= 1, CMnumMajFl
220      WRITE (RCRPT,543) CMAllFuels(IFUEL), &
            (CMFinalUnbenchUse(IFUEL,IREG,I), I= CMFirstYr, CMFirstYr+11)

        WRITE (RCRPT,546)
        DO 221 IFUEL= 1, CMnumMajFl
221     WRITE (RCRPT,544) CMAllFuels(IFUEL), &
             (CMFinalUnbenchUse(IFUEL,11,I), I=CMFirstYr,CMFirstYr+11)

      IF (LASTYR .GT. CMFirstYr+11) THEN
      WRITE (RCRPT,545) (CBECSyear+I, I= 13, 24)
      DO 222 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions(IREG)
        DO 222 IFUEL= 1, CMnumMajFl
         WRITE (RCRPT,543) CMAllFuels(IFUEL), &
        (CMFinalUnbenchUse(IFUEL,IREG,I), I= CMFirstYr+12, CMFirstYr+23)
222   CONTINUE

       WRITE (RCRPT,546)
        DO 223 IFUEL= 1, CMnumMajFl
223     WRITE (RCRPT,544) CMAllFuels(IFUEL), &
          (CMFinalUnbenchUse(IFUEL,11,I), I=CMFirstYr+12,CMFirstYr+23)

      IF (LASTYR .GT. CMFirstYr+23) THEN
      WRITE (RCRPT,545) (CBECSyear+I, I= 25, LASTYR-CMFirstYr+1)
      DO 224 IREG= 1, MNUMCR-2
       WRITE (RCRPT,513) CMRegions(IREG)
        DO 224 IFUEL= 1, CMnumMajFl
224      WRITE (RCRPT,543) CMAllFuels(IFUEL), &
         (CMFinalUnbenchUse(IFUEL,IREG,I), I= CMFirstYr+24,LASTYR)
        WRITE (RCRPT,546)
        DO 225 IFUEL= 1, CMnumMajFl
225      WRITE (RCRPT,544) CMAllFuels(IFUEL), &
          (CMFinalUnbenchUse(IFUEL,11,I), I=CMFirstYr+24,LASTYR)
      END IF     !LASTYR GREATER THAN CMFirstYr+23
      END IF     !LASTYR GREATER THAN CMFirstYr+11

! End of Longitude Non-Benchmarked End-Use Consumption


!  SEE KSDOUT SPREADSHEET FOR SERVICE DEMAND BY SERVICE

!  Print Longitudinal Floorspace Forecast by Building Type:
!  SEE DBOUT FOR FLOORSPACE BY BUILDING TYPE

!  Print Longitudinal Consumption Forecast by Building Type:
!  SEE DBOUT FOR CONSUMPTION BY BUILDING TYPE

!  SEE KDBOUT FOR END-USE CONSUMPTION

!  REPORT FUEL USE FOR DISTRICT SERVICES:

      WRITE (RCRPT,3000) (CBECSyear+I, I= 1, 12)
      DO 3010 r= 1, MNUMCR
       IF (r .EQ. 10) GOTO 3010  ! CA not yet a Census Division
       WRITE (RCRPT,'(3X,A20)') CMRegions (r)
       DO b= 1, CMnumBldg
        WRITE (RCRPT,'(5X,A14)') BldgName (b)
        DO s= 1, CMnumDHServ
         WRITE (RCRPT,'(7X,A20)') CMServices (s)
         DO f= 1, CMnumMajFl
          WRITE (RCRPT,'(9X,A20,1X,13(F7.3,1X))') CMMajor_Fuels (f), &
           (DistServConsump (r,b,s,f,y), y= CMFirstYr, CMFirstYr+11)
         END DO ! f
        END DO  ! s
       END DO   ! b
 3010 CONTINUE  ! r

      IF (LASTYR .GT. CMFirstYr+11) THEN
      WRITE (RCRPT,3000) (CBECSyear+I, I= 13, 24)
      DO 3020 r= 1, MNUMCR
       IF (r .EQ. 10) GOTO 3020  ! CA not yet a Census Division
       WRITE (RCRPT,'(3X,A20)') CMRegions (r)
       DO b= 1, CMnumBldg
        WRITE (RCRPT,'(5X,A14)') BldgName (b)
        DO s= 1, CMnumDHServ
         WRITE (RCRPT,'(7X,A20)') CMServices (s)
         DO f= 1, CMnumMajFl
          WRITE (RCRPT,'(9X,A20,1X,13(F7.3,1X))') CMMajor_Fuels (f), &
           (DistServConsump (r,b,s,f,y), y=CMFirstYr+12,CMFirstYr+23)
         END DO ! f
        END DO  ! s
       END DO   ! b
 3020 CONTINUE  ! r


      IF (LASTYR .GT. CMFirstYr+23) THEN
      WRITE (RCRPT,3000) (CBECSyear+I, I= 25,LASTYR-CMFirstYr+1)
      DO 3030 r= 1, MNUMCR
       IF (r .EQ. 10) GOTO 3030  ! CA not yet a Census Division
       WRITE (RCRPT,'(3X,A20)') CMRegions (r)
       DO b= 1, CMnumBldg
        WRITE (RCRPT,'(5X,A14)') BldgName (b)
        DO s= 1, CMnumDHServ
         WRITE (RCRPT,'(7X,A20)') CMServices (s)
         DO f= 1, CMnumMajFl
          WRITE (RCRPT,'(9X,A20,1X,13(F7.3,1X))') CMMajor_Fuels (f), &
           (DistServConsump (r,b,s,f,y), y= CMFirstYr+24, LASTYR)
         END DO ! f
        END DO  ! s
       END DO   ! b
 3030 CONTINUE  ! r
      END IF     !LASTYR GREATER THAN CMFirstYr+23
      END IF     !LASTYR GREATER THAN CMFirstYr+11


!  REPORT FUEL USE FOR END-USE SERVICES:
!  SEE KDBOUT FOR END-USE CONSUMPTION


!  REPORT BASE YEAR FUEL SHARES AND EFFICIENCIES

      WRITE (RCRPT,3001) (CMServices(s), s= 1, CMnumMajServ)
      DO 3011 r= 1, MNUMCR-2
       WRITE (RCRPT,'(1X,A20)') CMRegions (r)
       DO b= 1, CMnumBldg
        WRITE (RCRPT,'(2X,A14)') BldgName (b)
         DO f= 1, CMnumMajFl
          WRITE (RCRPT,'(3X,A15,7(F6.4,8X))') CMMajor_Fuels (f), &
           (FuelShareofServiceBASE (r,b,s,f), s= 1, CMnumMajServ)
         END DO ! f
       END DO   ! b
 3011 CONTINUE  ! r

      WRITE (RCRPT,3002) (CMServices(s), s = 1, CMnumMajServ)
      DO 3021 r= 1, MNUMCR-2
       WRITE (RCRPT,'(1X,A20)') CMRegions (r)
       DO b= 1, CMnumBldg
        WRITE (RCRPT,'(2X,A14)') BldgName (b)
         DO f= 1, CMnumMajFl
          WRITE (RCRPT,'(3X,A15,7(F7.3,7X))') CMMajor_Fuels (f), &
           (AverageEfficiencyBASE(r,b,s,f), s= 1, CMnumMajServ)
         END DO ! f
       END DO   ! b
 3021 CONTINUE  ! r


!  Generate Commercial Module Run Results Database:
      dbfile= FILE_MGR ('O','KDBOUT',.TRUE.)

      ! The first two output records are the Scenario name
      ! and Datekey for this run, respectively.  Zero values
      ! for the standard indices are output to maintain the
      ! standard record structure:
        Write (dbfile,*)"Division,BldgType,EndUse,Fuel,Year,Amount,Label"
      r= 0
      b= 0
      s= 0
      f= 0
      y= 0
      VarVal= 0.0
      VarName= SCEN
      WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
      VarName= DATE
      WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName

      ! Remaining records consist of key Comm variables.
      ! Variable names and indices are output with
      ! corresponding values; where an index does not
      ! correspond to a variable dimension, an index value
      ! of zero is output.

      ! Surviving Floorspace:
      s= 0
      f= 0
      VarName= 'SurvFloorTotal'
      DO r= 1, MNUMCR-2
       DO b= 1, CMnumBldg
        DO y= CMFirstYr, LASTYR
         VarVal= SurvFloorTotal (r,b,y)
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
        END DO ! y
       END DO  ! b
      END DO   ! r

      ! New Floorspace:
      s= 0   ! These are reinitialized just in case changes are
      f= 0   ! inserted after prior initialization.
      VarName= 'CMNewFloorSpace'
      DO r= 1, MNUMCR-2
       DO b= 1, CMnumBldg
        DO y= CMFirstYr, LASTYR
         VarVal= CMNewFloorSpace (r,b,y)
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
        END DO ! y
       END DO  ! b
      END DO   ! r

      ! Consumption by End Use (Major Fuels Only):
      VarName= 'EndUseConsump'
      DO r= 1, MNUMCR-2
       DO b= 1, CMnumBldg
        DO s= 1, CMnumServ
         DO f= 1, CMnumMajFl
          DO y= CMFirstYr, LASTYR
           VarVal= EndUseConsump (f,s,b,r,y)
           WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
          END DO ! y
         END DO  ! f
        END DO   ! s
       END DO    ! b
      END DO     ! r

      ! moredata - Benchmarking misties and other fuels mapped into enduse +1, building type +1
      b= CMnumBldg+1
      s= CMnumServ+1
      ! VarName= 'CMNonBldgUse' moredata - rename to an end use
        !  include all fuels - minor fuels now show up in the new end use ("unspecified / non-building")
        VarName= 'EndUseConsump'
      DO r= 1, MNUMCR-2
    !  For major fuels non-building use and for all minor fuels use mapping to end use "+1" / bldg type "+1"
       DO f= 1, CMnumAllFl
        DO y= CMFirstYr, LASTYR
         VarVal= CMFinalEndUse (f,r,y)
         If (f <= CMnumMajFl) VarVal= CMNonBldgUse (f,r,y) 
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
        END DO ! y
       END DO  ! f
      END DO   ! r

! moredata -- Detailed projections for miscellaneous electricity use      miscdetail
      f= 1  ! All part of miscellaneous electricity use                   miscdetail
      VarName= 'MiscElConsump'                                          ! miscdetail
      DO r= 1, MNUMCR-2                                                 ! miscdetail
        DO y= CMFirstYr, LASTYR                                         ! miscdetail
          DO b= 1, CMnumBldg                                            ! miscdetail
            VarVal= XfmrsDryElQ (r,b,y)                                 ! miscdetail
            WRITE (dbfile,1000) r,b,1,f,y,VarVal,VarName                ! miscdetail
            VarVal= SecurityElQ (r,b,y)                                 ! miscdetail
            WRITE (dbfile,1000) r,b,2,f,y,VarVal,VarName                ! miscdetail
            VarVal= ElevatorsElQ (r,b,y)                                ! miscdetail
            WRITE (dbfile,1000) r,b,3,f,y,VarVal,VarName                ! miscdetail
            VarVal= EscalatorsElQ (r,b,y)                               ! miscdetail
            WRITE (dbfile,1000) r,b,4,f,y,VarVal,VarName                ! miscdetail
            VarVal= ElVehiclesElQ (r,b,y)                               ! miscdetail
            WRITE (dbfile,1000) r,b,5,f,y,VarVal,VarName                ! miscdetail
            VarVal= CoffeeBrewersElQ (r,b,y)                            ! miscdetail
            WRITE (dbfile,1000) r,b,6,f,y,VarVal,VarName                ! miscdetail
            VarVal= KitchenVentElQ (r,b,y)                              ! miscdetail
            WRITE (dbfile,1000) r,b,7,f,y,VarVal,VarName                ! miscdetail
            VarVal= LaundryElQ (r,b,y)                                  ! miscdetail
            WRITE (dbfile,1000) r,b,8,f,y,VarVal,VarName                ! miscdetail
            VarVal= LabRefFrzElQ (r,b,y)                                ! miscdetail
            WRITE (dbfile,1000) r,b,9,f,y,VarVal,VarName                ! miscdetail
            VarVal= FumeHoodsElQ (r,b,y)                                ! miscdetail
            WRITE (dbfile,1000) r,b,10,f,y,VarVal,VarName               ! miscdetail
            VarVal= MedImagingElQ (r,b,y)                               ! miscdetail
            !WRITE (dbfile,1000) r,b,11,f,y,VarVal,VarName               ! miscdetail
            !VarVal= VidDisplayElQ (r,b,y)                               ! miscdetail
            WRITE (dbfile,1000) r,b,12,f,y,VarVal,VarName               ! miscdetail
            VarVal= LrgVidBoardElQ (r,b,y)                              ! miscdetail
            WRITE (dbfile,1000) r,b,13,f,y,VarVal,VarName               ! miscdetail
          END DO ! b                                                      miscdetail
          b= CMnumBldg+1  ! Assign b to non-building for water services   miscdetail
          VarVal= WaterServicesElQ (r,y)                                ! miscdetail
          WRITE (dbfile,1000) r,b,14,f,y,VarVal,VarName
        END DO ! y                                                        miscdetail
      END DO ! r                                                          miscdetail

! moredata -- dispense with printing this -- new concept is to add non-bldg as an additional end use --> therefore
!             end use energy will add up to final end use energy.
!      ! Consumption by Census Division (All Fuels):
!      s= 0
!      b= 0
!      VarName= 'CMFinalEndUse'
!      DO r= 1, MNUMCR-2
!       DO f= 1, CMnumAllFl
!        DO y= CMFirstYr, LASTYR
!         VarVal= CMFinalEndUse (f,r,y)
!         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
!        END DO ! y
!       END DO  ! f
!      END DO   ! r

! moredata
      ! Commercial Fuel Prices by Census Division (All Fuels):
      s= 0
      b= 0
      VarName= 'FuelPrice(87$)'
      DO r= 1, MNUMCR
       DO f= 1, CMnumAllFl
        DO y= CMFirstYr, LASTYR
            PriceData:  SELECT CASE (f)    
              case (1) PriceData   ! electricity
               Varval=pelcm(r,y) 
              case (2) PriceData   ! natural gas
               Varval=pngcm(r,y)  
              case (3) PriceData   ! distillate
               Varval=pdscm(r,y) 
              case (4) PriceData   ! residual
               Varval=prscm(r,y) 
              case (5) PriceData   ! lpg
               Varval=plgcm(r,y)  
              case (6) PriceData   ! coal
               Varval=pclcm(r,y) 
              case (7) PriceData   ! motor gas
               Varval=pmgcm(r,y) 
              case (8) PriceData   ! kerosene
               Varval=pkscm(r,y)  
            END SELECT PriceData
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
        END DO ! y
       END DO  ! f
      END DO   ! r

! Write Shell Heating and Cooling Factors for Excel Database !shellkdbout
      s= 0                                                   !shellkdbout
      f= 0
      DO r= 1, MNUMCR-2
       DO b= 1, CMnumBldg
        DO y= CMFirstYr, LASTYR
         VarName= 'HeatingFacExist'
         VarVal= ShellHeatFactor(b,r,1,y)
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
         VarName= 'HeatingFacNew'
         VarVal= ShellHeatFactor(b,r,2,y)
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
         VarName= 'CoolingFacExist'
         VarVal= ShellCoolFactor(b,r,1,y)
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
         VarName= 'CoolingFacNew'
         VarVal= ShellCoolFactor(b,r,2,y)
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
        END DO ! y
       END DO  ! b
      END DO   ! r                                           !shellkdbout

! Investments for equipment and subsidies, in billions of iKtechCostyr dollars  !investsub
      b= 0
      s= 0
      f= 0
      DO r= 1, MNUMCR-2
       DO s= 1, CMnumMajServ
        DO y= CMFirstYr, LASTYR
         VarName= 'Capital Invest'
         VarVal= InvestCost(r,s,y)
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
         VarName= 'Subsidy Invest'
         VarVal= SubsidyInvestCost(r,s,y)
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
         VarName= 'Subs111d Invest'
         VarVal= Subsidy111dInvestCost(r,s,y)
         WRITE (dbfile,1000) r,b,s,f,y,VarVal,VarName
        END DO ! y
       END DO  ! s
      END DO   ! r

 1000 FORMAT (I2,',',I2,',',I2,',',I1,',',I2,',',F9.3,',"',A15,'"')

      dbfile= FILE_MGR ('C','KDBOUT',.TRUE.)

!  Generate database of service demand satisfied by equipment type,
!  decision type, and year:

 999  sdfile= FILE_MGR ('O','KSDOUT',.TRUE.)

      ! File header relocated to bottom of text file so Comm DB can create pivot table
      !  without having to manually delete header

      WRITE (sdfile,4000) ((BASEYR-1)+y,y=CMFirstYr,LASTYR)
 4000 FORMAT ('  r,  b,  s,  f,  d,  t,  v, ',<LASTYR-(CMFirstYr-1)>(5X,I4,","), &
       ' Description,','  Eff')                                        ! sw10-95  !add b to ksdout ! effksdout

      ! Write equip service demand out by service:                     ! sw10-95

      ! Report Options                                                 ! sw10-95

        minreg=1                                                       ! sw10-95
        if(SDReportOpts(1).eq.01) maxreg=mnumcr-1                      ! sw10-95
        if(SDReportOpts(1).eq.01) minreg=mnumcr-1                      ! sw10-95
        if(SDReportOpts(1).eq.10) maxreg=mnumcr-2                      ! sw10-95
        if(SDReportOpts(1).eq.11) maxreg=mnumcr-1                      ! sw10-95
        if(SDReportOpts(1).eq.00) goto 5000                            ! sw11-95
        mindec=1                                                       ! sw10-95
        if(SDReportOpts(2).eq.01) maxdec=CMDecision+1                  ! sw10-95
        if(SDReportOpts(2).eq.01) mindec=CMDecision+1                  ! sw10-95
        if(SDReportOpts(2).eq.10) maxdec=CMDecision                    ! sw10-95
        if(SDReportOpts(2).eq.11) maxdec=CMDecision+1                  ! sw10-95

      DO ireg= minreg,maxreg ! for each census division + national total sw10-95
      r = ireg                                                         ! sw10-95
      if (ireg .eq. mnumcr-1) r = mnumcr ! skip hole left for california sw10-95
      DO b= 1, CMnumBldg
       DO s= 1, CMnumMajServ            ! for each service
        DO TsubS= 1, CMnumTechsforService (s)
        t= TechsforService (s,TsubS)   ! get next tech providing this s
        ! Identify fuel used by this technology:
        DO f= 1, CMnumMajFl
         IF (FuelbyTech (t,f) .EQ. 1) GOTO 4010
        END DO ! f
 4010   CONTINUE
!                                                                              Added Efficiency to ksdout effksdout
        DO v= 1, CMnumEqV (t)      ! for each model of this t
!         IF (TechbyModel(t,v) .eq. 1) THEN     ! all v this tech
          DO d= mindec,maxdec     ! for each decision type, plus total           ! sw10-95
!      TOTAL d REMOVED FROM EquipSD ARRAY TO CONSERVE MEMORY                       ebe07-03    
              IF (d .EQ.maxdec .and. maxdec.EQ. CMDecision+1) THEN               ! conserve array memory ebe07-03
      WRITE (sdfile,4020) r,b,s,f,d,t,v,(SUM(EquipSD(r,b,t,v,1:CMDecision,y)), & ! conserve array memory ebe07-03
              y=CMFirstYr,LASTYR), EquipName (t,v),TechEff(d,s,t,v)              ! conserve array memory ebe07-03 ! effksdout
          ELSE                                                                   ! conserve array memory ebe07-03
      WRITE (sdfile,4020) r,b,s,f,d,t,v,(EquipSD (r,b,t,v,d,y),                & ! sw10-95 !add b to ksdout
              y=CMFirstYr,LASTYR), EquipName (t,v),TechEff(d,s,t,v)              ! sw10-95 ! effksdout
              END IF                                                             ! conserve array memory ebe07-03
 4020 FORMAT (7(1X,I2,","),<LASTYR-(CMFirstYR-1)>(1X,F8.3,","),1X,'"',(A),'"',",",F8.2)     ! sw10-95 ! effksdout
          END DO ! d
!         ENDIF  ! TechbyModel(t,v).EQ.1 test
        END DO   ! v
       END DO    ! TsubS
      END DO     ! s
      end DO     ! b
      END DO     ! ireg                                                        ! sw10-95
      

      ! Construct file footder so file is self-documenting:
      WRITE (sdfile,4030) SCEN,DATE
 4030 FORMAT (/,' This is file KSDOUT, output by the Commercial', &
       ' Sector Demand Module',/, &
       ' of the National Energy Modeling System (NEMS).  Each', &
       ' row contains',/, &
       ' the forecasted service demand for each year of the NEMS', &
       ' forecast',/, &
       ' period that is satisfied by a particular type of', &
       ' equipment, for a',/, &
       ' particular decision type.  The first seven fields contain',         & ! eb01-96
       ' indices',/, &
       ' that identify the service provided by the equipment, the', &
       ' fuel used,',/, &
       ' the decision type, the technology type, and the model type,', &
       ' in that',/, &
       ' order.  Indices point to contents of the following lists:',//,  &
       ' r: {Census Divisions 1 through 9, 11=national}',/, &
       ' b: {Building Type - Assembly, Education, Food Sales, Food Services,',/, &
       '     Health Care, Lodging, Office_Lg, Office_Sm, Mercantile & Service,',/, &
       '     Warehouse, Other}',/, &
       ' s: {Space Heating, Space Cooling, Hot Water', &
       ' Heating, Ventilation,',/, &
       '     Cooking, Lighting, Refrigeration}',/, &
       ' f: {Electricity, Natural Gas, Distillate}',/, &
       ' d: {New, Replacement, Retrofit/Surviving, Total}',/, &
       ' t and v are equipment identifiers assigned in KTECH.',/, &
       ' Efficiency units vary by technology type.',/, &
       ' Service Demand units are Trillion Btu out.',//, &
       ' These results correspond to a run having',/, &
       ' Scenario Name:  ',A10,/, &
       '      Date Key:  ',A8)

 5000  sdfile= FILE_MGR ('C','KSDOUT',.TRUE.)
!  Files closed by NEMS integrating module, and this has caused an occasional issue
!       RCRPT= FILE_MGR ('C','KRPT',.TRUE.)

! ======================
! End of All year reports

! Zero out detailed commercial variables for years before first model year  ebe09-06
      DO iyr= 1, CMFirstYr-1
       CMUSSURVFLOORTOT(iyr)=0.0
       CMUSNEWFLOORTOT(iyr)=0.0
       
       DO i= 1, 4
        CMUSCOGEN(i,iyr)=0.0
       END DO ! i for cogen fuel use
       
       DO b= 1, CMnumBldg
        CMFinalEndUseCon (b,iyr)= 0.0
        CMSURVFLOORTOT(b,iyr)=0.0
        CMNEWFLRSPACE(b,iyr)=0.0
       END DO ! b
       
       DO f= 1, CMnumMajFl
        DO s= 1, CMnumServ
         CMUSConsumption (s,f,iyr)= 0.0
         IF (s .LT. 10) THEN
          CMUSAvgEff (s,f,iyr)= 0.0
          CMUSPurchEff(s,f,iyr)= 0.0
          IF (s .LT. 4) CMUSDistServ(s,f,iyr)= 0.0
         END IF  !check for s in range
        END DO !s
       END DO ! f - major fuels  
      END DO  ! iyr - all years prior to CMFirstYr
 

! *****FORMAT Statements
500   FORMAT ('>>>>> Subroutine COMReport called ', &
              I4,1X,'Iter ',I2)
510   FORMAT (/,'   Commercial Floorspace for ',I4,/, &
                '   by Census Division (New, Surviving, Total)',/, &
                '   (million square feet)',/)
511   FORMAT (3X,A20,1X,3(F7.0,2X))
512   FORMAT ('1  Commercial Service Demand for ',I4,/, &
              '   by Census Division and Service',/, &
              '   New, Surviving',/, &
              '   (Trillion BTU out)',/)
513   FORMAT (3X,A20)
514   FORMAT (8X,A20,1X,2(F9.0,4X))
515   FORMAT ('1   Proportion of Service Demand for ',I4,/, &
              '    by Census Division, Service, and Fuel',/, &
              '    Retiring, Surviving, and New',//,29X,'Elec',4X, &
              'Nat Gas',2X,'Dist',/)
516   FORMAT (5X,A20)
517   FORMAT (7X,A20,1X,3(F7.2,1X))
518   FORMAT ('1   Average Equipment Efficiency for ',I4,/, &
              '    by Census Division, Service, and Fuel',/, &
              '    Retiring, Surviving, and New',/)
519   FORMAT ('1   Benchmarked Commercial Fuel Consumption', &
              'for ', I4,/, &
              '    (trillion BTU)',/,95X,'  Total',/)
520   FORMAT (3X,A20,1X,10(F7.1,1X))
521   FORMAT (/,3X,21('='),/,'   Natural Gas- Core    ',10(F7.1,1X))
522   FORMAT ('   Natural Gas- Non-Core',10(F7.1,1X))
523   FORMAT (8X,'National Total',2X,3(F7.0,2X))
524   FORMAT ('1    Unbenchmarked Commercial Fuel Consumption', &
                'for ', I4,/, &
              '     (trillion BTU)',/,95X,'  Total')
525   FORMAT (3X,A20)
526   FORMAT (6X,A20,1X,F6.1)
527   FORMAT ('1   ',I4,' SEDS Values by Division and Fuel',/, &
              '    (trillion BTU)',/)
528   FORMAT (3X,A20,1X,4(F12.2,1X))
529   FORMAT (/,'1   ',I4,' SEDS - CBECS Misties (= NonBuilding Use)',/, &
              '    (trillion BTU)',/, &
         32X,'Elec',6X,'Nat Gas',9X,'Dist',8X,'Total',/)
530   FORMAT ('1  Commercial Model Reports',/,'   Run Scenario= ',A10,/, &
         '   Run Datekey= ',A8,/)
531   FORMAT (10X,'Division Total',5X,2(F9.0,4X))
532   FORMAT (/,'   NATIONAL TOTAL',12X,2(F9.0,4X))
533   FORMAT ('1   Commercial Floorspace',/, &
              '    by Census Division (New, Surviving, Total)',/, &
              '    (million square feet)',//,28X,13(I4,4X),/ )
534   FORMAT (/,3X,A20,/,5X,'Yearly New',11X,13(F7.0,1X),/, &
              5X,'Cum. Gross New to t ',1X,13(F7.0,1X),/, &
              5X,'Aged CBECS          ',1X,13(F7.0,1X),/, &
              5X,'Cum. Aged New to t-1',1X,13(F7.0,1X),/, &
              5X,'Surviving           ',1X,13(F7.0,1X),/, &
              5X,'Total               ',1X,13(F7.0,1X))
535   FORMAT(/,3X,'National',/,5X,'Yearly New',11X,13(F7.0,1X),/, &
              5X,'Cum. Gross New to t ',1X,13(F7.0,1X),/, &
              5X,'Aged CBECS          ',1X,13(F7.0,1X),/, &
              5X,'Cum. Aged New to t-1',1X,13(F7.0,1X),/, &
              5X,'Surviving           ',1X,13(F7.0,1X),/, &
              5X,'Total               ',1X,13(F7.0,1X))
536   FORMAT (/,3X,A20,/,5X,'Yearly New',11X,12(F7.0,1X),/, &
              5X,'Cum. Gross New to t ',1X,12(F7.0,1X),/, &
              5X,'Aged CBECS          ',1X,12(F7.0,1X),/, &
              5X,'Cum. Aged New to t-1',1X,12(F7.0,1X),/, &
              5X,'Surviving           ',1X,12(F7.0,1X),/, &
              5X,'Total               ',1X,12(F7.0,1X))
537   FORMAT(/,3X,'National',/,5X,'Yearly New',11X,12(F7.0,1X),/, &
              5X,'Cum. Gross New to t ',1X,12(F7.0,1X),/, &
              5X,'Aged CBECS          ',1X,12(F7.0,1X),/, &
              5X,'Cum. Aged New to t-1',1X,12(F7.0,1X),/, &
              5X,'Surviving           ',1X,12(F7.0,1X),/, &
              5X,'Total               ',1X,12(F7.0,1X))
538   FORMAT (7X,A20,2X,13(F5.2,1X))
540   FORMAT (6X,A11)
541   FORMAT (/,'1   Average Technology Efficiencies by Year',/, &
              '    by Census Division, Service, Decision Type,', &
         ' and Fuel',/,4x,'(Minor Service Efficiencies based', &
         ' on 2012 efficiency = 1)', &
         //,30X,13(I4,2X),/)
542   FORMAT (/,'1   Benchmarked End-Use Consumption by Year',/, &
              '    by Census Division and Fuel',/, &
              '    (Trillion BTU)',//,31X,13(I4,6X),/)
543   FORMAT (5X,A20,1X,13(F9.1,1X))
544   FORMAT (5X,A20,1X,13(F9.1,1X))
545   FORMAT (/,'1   UnBenchmarked End-Use Consumption by Year',/, &
              '    by Census Division and Fuel, Excluding ', &
                  'Nonbuilding Consumption and Nonutility ', &
                  'Generation',/,'    (Trillion BTU)', &
              //,31X,13(I4,6X),/)
546   FORMAT (3X,'National')
549   FORMAT (/,'1   Behavioral Rules (Least Cost, Same Fuel, Same', &
              ' Tech)',/,4X,'by Service, Decision, & Building Types',/, &
              20X,'PLC',3X,'PSF',3X,'PST',/)
550   FORMAT (3X,A12)
551   FORMAT (5X,A14,1X,3(F4.2,2X))
552   FORMAT (/,'1   Risk Proportions and Premiums', &
              //,5X,'Prop  Premium',/)
553   FORMAT (5X,F5.3,1X,F6.3)
554   FORMAT (/,3X,A20,/,5X,'Yearly New',11X,'------',2X,12(F7.0,1X),/, &
              5X,'Cum. Gross New to t ',1X,'------',2X,12(F7.0,1X),/, &
              5X,'Aged CBECS          ',1X,13(F7.0,1X),/, &
              5X,'Cum. Aged New to t-1',1X,'------',2X,12(F7.0,1X),/, &
              5X,'Surviving           ',1X,'------',2X,12(F7.0,1X),/, &
              5X,'Total               ',1X,13(F7.0,1X))
555   FORMAT (/,3X,'National',/,5X,'Yearly New',11X,'------',2X, &
              12(F7.0,1X),/, &
              5X,'Cum. Gross New to t ',1X,'------',2X,12(F7.0,1X),/, &
              5X,'Aged CBECS          ',1X,13(F7.0,1X),/, &
              5X,'Cum. Aged New to t-1',1X,'------',2X,12(F7.0,1X),/, &
              5X,'Surviving           ',1X,'------',2X,12(F7.0,1X),/, &
              5X,'Total               ',1X,13(F7.0,1X))
557   FORMAT (3X,'National')
558   FORMAT (/,'1   National Service Demand by Service', &
              /,'    (Trillion BTU Out)', &
              /,'    (Lighting: Billion Lumen Years)', &
              //,20X,12(I4,6X),/)
560   FORMAT (3X,'National')
561   FORMAT (/,'1   Equipment Investment Cost by Census Division,', &
              ' and Service',/,'    (Billion ', I4,'$)',   &
              //,43X,13(I4,4X),/)
562   FORMAT (5X,A15,A20,13(F7.4,1X))

 3000 FORMAT ('1  Fuel Consumption for District Services',/, &
              '   by Fuel, Service, Building, and Census Division',/, &
              '   (Trillion Btu)',//, &
              33X,13(I4,4X),/)

 3001 FORMAT (/,'1  Base year Fuel Shares',/, &
              '   by Fuel, Service, Building, and Census Division', &
              //, &
              18X,7(A14),/)

 3002 FORMAT (/,'1  Base year Average Efficiency',/, &
              '   by Fuel, Service, Building, and Census Division',/, &
              '   (Btu out over Btu in)',//, &
              18X,7(A14),/)

      RETURN
      END
!*****
! Floorspace Survival function

      REAL FUNCTION CMSurvRate*4 (KREG,KBLDTP,KBldgAge)
      IMPLICIT NONE
      include'parametr'
      include'ncntrl'
      include'comparm'
      include'comvars'

      INTEGER*4 KBLDTP,   & ! the NEMS_COMM building type index
                KREG     ! Census Division index

      REAL*4 KBldgAge    ! age (years) of floorspace of building
                         ! type KBLDTP in Census Division KREG
                         ! for which to calculate surviving
                         ! proportion.

!  CMAvgAge (KBLDTP) = average lifetime (years) of buildings
!                      of type KBLDTP.
!  CMGamma  (KBLDTP) = logistic survival function shape parameter
!                      describing survival of building type KBLDTP

!  CMAvgAge and CMGamma are declared in COMVARS, and input from
!  file KBLDG in the NEMS_COMM COMFloorspace subroutine.


       CMSurvRate =                1.0 / &
          ( 1.0 + (KBldgAge/CMAvgAge (KBLDTP))**CMGamma (KBLDTP) )

      RETURN
      END


!====Cost Trend Function
      REAL FUNCTION KEqCost*4 (r,t,v,y,ctype)

!  This function returns the projected cost of equipment identified
!  in the KTECH file by technology index t and vintage (model) index v,
!  for the calendar year y, where ctype indicates whether the requested
!  cost type is the unit installed capital cost or the annual operating
!  and maintenance cost.  Several required parameters, such as the
!  trend type (Mature, Adolescent, Infant, etc.), logistic shape
!  parameters, years of availability, etc., are obtained from the
!  COMVARS common block rather than passed as arguments.

      IMPLICIT NONE
      include 'parametr'  ! system-wide parameter constants
      include 'ncntrl'    ! variables set by integrating module
      include 'comparm'   ! commercial module parameters
      include 'comvars'   ! commercial module common variables

      INTEGER*4 r         ! Census division index
      INTEGER*4 t         ! Technology index
      INTEGER*4 v         ! Vintage (model) index
      INTEGER*4 y         ! Price forecast calendar year
      CHARACTER*3 ctype   ! Cost type requested ('CAP', 'O&M', or 'SUB')

      INTEGER*4 y0        ! Year of inflection on logistic cost curve
      INTEGER*4 y1        ! Starting year of logistic cost curve
      REAL*4 d            ! Proportional decline in equipment cost
      REAL*4 gamma        ! Logistic cost curve shape parameter

      integer*4 iforward(3) !   calculation of price effects on tech menu  ! PITC
      integer*4 ifwd          !   holding variable for applied effect        ! PITC
      integer*4 IFMAX         !   maximum forward effect                     ! PITC
      INTEGER*4 f             !   fuel index                                 ! PITC
      integer*4 iLastSTEOyr   !   calendar year for last STEO year           ! PITC
      common /pitc/iforward,IFMAX,ilastSTEOyr                                ! PITC

              ! Identify fuel used by this technology:
               DO 85 f= 1, CMnumMajFl
                IF (FuelbyTech (t,f) .EQ. 1) GOTO 87
   85          CONTINUE
   87         CONTINUE

!    Check for "close-in" technologies and trim advancement years if so  ! PITC
        If(TechAvailability(r,t,v,1).le.iLastSTEOyr+50)ifwd=iforward(f) ! PITC  !regionalize tech availability
        If(TechAvailability(r,t,v,1).le.iLastSTEOyr+10)                    & ! PITC  !regionalize tech availability
                                            ifwd=max(-5,iforward(f))  ! PITC
        If(TechAvailability(r,t,v,1).le.iLastSTEOyr+ 5)                    & ! PITC  !regionalize tech availability
                                            ifwd=max(-3,iforward(f))  ! PITC
        If(TechAvailability(r,t,v,1).le.iLastSTEOyr   )ifwd=0             ! PITC  !regionalize tech availability

!  If the specified equipment is not available for purchase in the
!  specified year, return the Commercial Module's maximum cost:
       IF ( (y .LT. TechAvailability(r,t,v,1)+ifwd ) .OR.    & ! first year  ! PITC  !regionalize tech availability
            (y .GT. TechAvailability(r,t,v,2))     )         & ! last year  !regionalize tech availability
       THEN
           KEqCost= MAXCOST
           RETURN
       END IF

!  In case of any error that might occur below, the cost returned
!  will be the Commercial Module's maximum cost:
       KEqCost= MAXCOST

!  Project the equipment cost based on the type of cost trend
!  appropriate for the maturity level of this technology:

       ! Mature technology:
       IF ( CostTrend(t,v) .EQ. "Mature" ) THEN
          ! Current implementation calls for costs to continue
          ! unchanged from the initial costs specified in KTECH.
        IF ( ctype .EQ. "CAP" ) THEN
          KEqCost= TechCost(r,t,v,1)           ! regionalize techcost
          RETURN
        ELSE
         IF (ctype .EQ. "O&M") THEN
          KEqCost= TechCost(r,t,v,2)           ! regionalize techcost
          RETURN
         ELSE
          IF (ctype .EQ. "SUB") THEN           !investsub
           KEqCost = TechCost(r,t,v,3)         ! regionalize techcost
           RETURN
          ELSE
           RETURN
          ENDIF ! SUB or other
         ENDIF  ! O&M
        ENDIF   ! CAP test
       ENDIF    ! Mature technology

       ! Adolescent technology
       IF ( CostTrend(t,v) .EQ. "Adolescent" ) THEN
          ! Current implementation calls for a logistic functional
          ! form, with the base year (2012) coinciding with the
          ! inflection point (the code actually uses the first year
          ! of availability as specified in KTECH). The remaining
          ! proportional cost decline is specified (CostParam3), as
          ! is a 'representative' year of introduction (CostParam1),
          ! and shape parameter (CostParam2), in the KTECH input file:

        y1= CostParam1 (t,v) + ifwd         ! representative year cost decline began
        y0= TechAvailability (r,t,v,1) + ifwd ! year of inflection of cost trend  !regionalize tech availability
         d= CostParam3 (t,v) ! total possible proportional decline in
                             ! equip cost from y0 onward
        gamma= CostParam2 (t,v) ! logistic curve shape parameter

        IF ( ctype .EQ. "CAP" ) THEN
          KEqCost= TechCost(r,t,v,1) * 2.0 * d            &         ! regionalize techcost
                / ( 1.0 + (FLOAT(y - y1)/FLOAT(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * TechCost(r,t,v,1)                   ! regionalize techcost
          RETURN
        ELSE
         IF (ctype .EQ. "O&M") THEN
          KEqCost= TechCost(r,t,v,2) * 2.0 * d           &          ! regionalize techcost
                / ( 1.0 + (FLOAT(y - y1)/FLOAT(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * TechCost(r,t,v,2)                   ! regionalize techcost
          RETURN
         ELSE
             IF (ctype .EQ. "SUB") THEN
              KEqCost = TechCost(r,t,v,3) * 2.0 * d              & ! regionalize techcost
                    / ( 1.0 + (FLOAT(y - y1)/FLOAT(y0 - y1))**gamma ) &
                    + ( 1.0 - d ) * TechCost(r,t,v,3)              ! regionalize techcost
              RETURN
               ELSE
               RETURN
             ENDIF ! SUB and other
           ENDIF ! O&M
       ENDIF  ! CAP ctype test
      ENDIF   ! Adolescent technology


       ! Infant technology
       IF ( CostTrend(t,v) .EQ. "Infant" ) THEN
          ! Current implementation calls for a logistic functional
          ! form for the cost trend:

        y1= TechAvailability (r,t,v,1) + ifwd ! year cost decline begins  !regionalize tech availability
        y0= CostParam1 (t,v) +ifwd          ! year of inflection of cost trend
         d= CostParam3 (t,v)                ! total possible proportional decline in
                                            ! equip cost from y1 onward
        gamma= CostParam2 (t,v)             ! logistic curve shape parameter

        IF ( ctype .EQ. "CAP" ) THEN
          KEqCost= TechCost(r,t,v,1) * d                          &  ! regionalize techcost
                / ( 1.0 + (FLOAT(y - y1)/FLOAT(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * TechCost(r,t,v,1)                    ! regionalize techcost
          RETURN
        ELSE
         IF (ctype .EQ. "O&M") THEN
          KEqCost= TechCost(r,t,v,2) * d                          &  ! regionalize techcost
                / ( 1.0 + (FLOAT(y - y1)/FLOAT(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * TechCost(r,t,v,2)                    ! regionalize techcost
          RETURN
         ELSE
          IF (ctype .EQ. "SUB") THEN
           KEqCost = TechCost(r,t,v,3) * d                        & ! regionalize techcost
                 / ( 1.0 + (FLOAT(y - y1)/FLOAT(y0 - y1))**gamma )&
                 + ( 1.0 - d ) * TechCost(r,t,v,3)                  ! regionalize techcost
           RETURN
          ELSE
           RETURN
          ENDIF ! SUB and other
         ENDIF  ! O&M
        ENDIF   ! CAP ctype test
       ENDIF    ! Infant technology

       RETURN
       END

!===============================================================
      SUBROUTINE CALC111D (RCDBG)  !111(d)
!===============================================================

      IMPLICIT NONE

      INCLUDE'parametr'! system-wide parameter constants
      INCLUDE'ncntrl'  ! control variables set by integrating module
      INCLUDE'comparm' ! commercial module parameters
      INCLUDE'comvars' ! commercial module common variables
      INCLUDE'emmparm' ! include needed for RPS credit price
      INCLUDE'uecpout' ! contains RPS credit price in 1987 mills/kWh - EPRPSPR(CURIYR) - needed to define variable in e111d include file to prevent automatic object error
      INCLUDE'e111d'
      INCLUDE'apq'
      INCLUDE'macout'
      INCLUDE'uefpout'
      INCLUDE'eusprc'
      INCLUDE'emission'

      INTEGER r, y, s, nt
      INTEGER RCDBG

      REAL*4 BASELINEBKWHCM  !Stores restart file values of electricity consumption by Census division and year
      COMMON/BASE111DCM/BASELINEBKWHCM(MNUMCR,MNUMYR)
      REAL*4 com111drensub                                            ! 111(d)ren - subsidies for PV and Wind
	  Integer iGenCapCostYR                                           ! 111(d)ren - moving capital cost year to common for calc111d subroutine
	  COMMON /EPA111drensub/ com111drensub(mnumyr,mnumcr,11),    &    ! 111(d)ren - last dimension is number of commercial techs, parameter not set here
                             iGenCapCostYR                            ! 111(d)ren

       y = CURIYR

       IF(CURCALYR .LT. 2017) THEN
         DO r = 1,MNUMCR-2
           SAVE111COM(r,y) = 0.
           COST111COM(r,y) = 0.
         ENDDO
         RETURN
       ENDIF

      WRITE(RCDBG,*) "Year, Division, COST111COM, SAVE111COM"
      !Calculate savings and initialize cost to zero
         DO r = 1,MNUMCR-2
           COST111COM(r,y) = 0.
           SAVE111COM(r,y) = BASELINEBKWHCM(r,y) - (QELCM(r,y) / 3412.)*10**3  !BASELINEBKWHCM(r,y) read from BLDBASE
           DO s = 1,CMnumMajServ
             COST111COM(r,y) = COST111COM(r,y) + Subsidy111dInvestCost(r,s,y)
           ENDDO
         ENDDO

       ! Before exit convert to billions of 1987 dollars & add 50% for approximate administrative costs (1.5 factor based on direct and indirect overhead costs that are higher than incentive costs)
       DO r = 1,MNUMCR-2
         COST111COM(r,y) = 1.5 * (COST111COM(r,y) * MC_JPGDP(-2) / MC_JPGDP(iKtechCostYr))
       ENDDO

	   !    Add subsidies for distributed generation and deflate from igencapcostyr (generally not the same as rsmeqp) as well as convert from $mill to $bill
       DO r=1,MNUMCR-2
        DO NT=1, 11  
         COST111COM(r,Y)=COST111COM(r,Y)+ com111drensub(y,r,nt)*( MC_JPGDP(-2)/MC_JPGDP(igencapcostyr-baseyr+1) )/1000.      !111(d)ren
        ENDDO
	   ENDDO
 
 ! This is a test write of the data to kdebug in 1987 dollars
       DO r = 1,MNUMCR-2
         WRITE(RCDBG,'(2i5,2f12.4)') CURCALYR, r, COST111COM(r,y), SAVE111COM(r,y)
       ENDDO

      RETURN
      END          !SUBROUTINE CALC111D  111(d)


!===============================================================
      SUBROUTINE CDISTGEN(ICURIYR,ICURITR,RCRPT,rcdbg)
!===============================================================
!  INCLUDES USER SPECIFIED NUMBER OF GENERAL TECHNOLOGIES:
!  -- REQUIRES 31 ANNUAL TECHNOLOGY VINTAGE CHARACTERIZATIONS
!  -- CAN INCLUDE A USER-SPECIFIED NUMBER OF TECHNOLOGIES (NTEK)

       IMPLICIT NONE
       INTEGER ICURITR
       INTEGER NTEK
       PARAMETER (NTEK=11)
       Integer MaxNiche
       PARAMETER (MaxNiche=4)

!----NEMS GLOBAL PARAMETERS, COMMERCIAL PARAMETERS
!       INTEGER MNUMYR,MNUMCR,MNUMBLDG,MNUMFUEL,FCRL
!       PARAMETER (MJUMPYR=41)
!       PARAMETER (MNUMCR=11)
!       PARAMETER (CMNUMBLDG=11)
       include'parametr'
       include'comparm' ! commercial module parameters
       include'comvars' ! commercial module common variables
       include'apq'
       include'emmparm'  ! include needed for RPS credit price
       include'uecpout'  ! contains RPS credit price in 1987 mills/kWh - EPRPSPR(CURIYR)
        !also contains clean energy target percentage of electricity as a fraction - EPRPSTGT(CURIYR) ceschp
!  Note: when doing elasticity runs, include PQ block instead of APQ  ! elastruns
!        MUST also make this change in the Main Comm routine          ! elastruns
!        include'pq'                                                  ! elastruns
       include'macout'
       include'uefpout'
       include'eusprc'
       include'ncntrl'
       include'cogen'
       include'commrep'    ! nems ftab report variables
       include'bldglrn'    ! shipments common block for distributed generation learing

!---COMMON BLOCK BLDGDGGS IS FOR COMMUNICATING WITH THE UTILITY MODULE
!   GRID ELECTRICITY SALES IN TRILLS, PRICES FOR GRID SALES IN $/MMBTU
       COMMON/BLDGDGSG/QRSDGSG(MNUMYR,MNUMCR),PRSDGSG(MNUMYR,MNUMCR), &
                       QCMDGSG(MNUMYR,MNUMCR)
       REAL*4 QRSDGSG,QCMDGSG,PRSDGSG,PCMDGSG

       REAL*4 CMCogenEL (MNUMCR-2,CMnumBldg, &
                CmnumMajFl+CMnumMinFl+CMnumRenew+CMnumOthrFl, &
                                 MNUMYR)
      ! Cogenerated Electricity Forecast.  For year CURIYR,
      ! Census Division r, Building Type b, and Fuel Type f,
      ! CMCogenEl (r,b,f,CURIYR) is the current
      ! year forecast for electricity cogeneration in Trillions of
      ! BTUs using fuel f.

!---COMMON BLOCK FOR TRANSERING DATA TO THE 111(d) SUBROUTINE
      REAL*4 com111drensub                                            ! 111(d)ren - subsidies for PV and Wind
	  Integer iGenCapCostYR                                           ! 111(d)ren - moving capital cost year to common for calc111d subroutine
	  COMMON /EPA111drensub/ com111drensub(mnumyr,mnumcr,11),    &    ! 111(d)ren - last dimension is number of commercial techs, parameter not set here
                             iGenCapCostYR                            ! 111(d)ren
							 
!---DISTGEN OUTPUTS (NO. OF INSTALLED UNITS, TOTAL ELEC. GENERATION
!      GENERATION FOR OWN USE, HOT WATER GAS CONSUMPTION AVOIDED AND
!      SUPPLIED BY FUEL CELLS TO NET AGAINST GAS HOT WATER DEMAND) AND
!      INVESTMENT CAPITAL EXPENDITURES
!        NTEK=1 FOR SOLAR
!        NTEK=2 FOR FUEL CELLS
!        NTEK=3 FOR CONVENTIONAL COGEN - Engine GAS
!        NTEK=4 FOR CONVENTIONAL COGEN - Steam Turbine COAL
!        NTEK=5 FOR CONVENTIONAL COGEN - Engine OIL
!        NTEK=6 FOR CONVENTIONAL COGEN - Steam Turbine MSW
!        NTEK=7 FOR CONVENTIONAL COGEN - Gas Turbine
!        NTEK=8 FOR Gas Micro Turbine
!        NTEK=9 FOR HYDRO
!        NTEK=10 FOR Wood
!        NTEK=11 FOR Wind

       COMMON/CMDGOUT/UNITS(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    TRILLS(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    capacity(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &     
                    TRILLSOWNUSE(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    fuelusage(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    HWBTU(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    SHBTU(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    INVEST(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK), &
                    Ifueltype(ntek)
       REAL*4 UNITS, TRILLS, capacity, TRILLSOWNUSE,TotUnits, &    
               fuelusage,HWBTU,shbtu,INVEST,TotCogenEl
       REAL*4 xCumPenSqft(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG,NTEK)
       REAL*4 xExistingPen
       REAL*4 xadjcost,learncost,cumship,xbeta(ntek),xc0(ntek)   
       REAL*4 xadjcost2 !economies of scale for DG
       Logical GlobalLearn                                        

!----LOCAL VARIABLES AND PARAMETERS INTERNAL TO CDISTGEN
!  DATA FOR PAYBACK COMPUTATION IN 30-YEAR CASHFLOW CALCULATION
       INTEGER IPAYBACK(30),ISIMPLEPAYBACK
       REAL*4 XINTRATE, XTERM, XINFLATION, XLIFE, XDOWNPAYPCT
       REAL*4 XTAXCREDIT(30), XTAXRATE
       REAL*4 XEQCOST(NTEK), XTAXCREDITPCT, XTAXCREDITMAXKW, XTAXCREDITMAX,XBASEYRFUELCOST
       REAL*4 XMAINTCOSTBASE, XVALESAVEBASE
       REAL*4 XDEGRADATION, XANNUALKWH, xFuelInput, XWATERHTGMMBTU
       REAL*4 XBTUWASTEHEAT, XEXCESSKWH, XSPACEHTGMMBTU, XfuelPRICE
       REAL*4 xRetailElecPR, xSalestoGridPR 

!  FOR 111(d) TAX CREDIT MODELING FOR PV AND WOND
	   REAL*4 comtxcrpct_div(mnumyr,mnumcr,ntek)                !111(d)ren
	   
!  FOR INTERCONNECTION LIMITATION !INXlimit
       REAL*4  XINX(MNUMCR-2),XINXDECAY(MNUMCR-2,MNUMYR)
       Integer XINXFY, XINXLY

!  FOR RPS MODELING
       REAL*4 xRetailElecPRnoRPS, xSalestoGridPrnoRPS, xRPS(ntek,MNUMYR)
       REAL*4 xRetailElecPRadjRPS, xSalestoGridPradjRPS !for Markey 2009 bill credit adjustment features
       Integer iRPSStartYear(ntek),iRPSPhaseOutYear(ntek), iRPSGrandFatherYear(ntek), iCalYR
       Integer iNumRPSCreditYrs(ntek,MNUMYR), iNumYrsatRPSBaseRate(ntek,MNUMYR), itemp1, itemp2
       ! For calculating credit factors to pass to Electric Generation Model
       REAL*4 xPVGenAdded(MNUMCR,MNUMYR),xWindGenAdded(MNUMCR,MNUMYR)
       REAL*4 xCompCredit, xCompGen, xCredit, xBonus
       REAL*4 xpreCESGenOwnUse,xpreCESGen,xpreCESQ,xgen,xgenownuse
       REAL*4 XVALESAVEBASEnoRPS, XVALESAVEBASEadjRPS

!  DATA FOR 30-YEAR CASHFLOW CALCULATION
       REAL*4 XOUTLAY(30), XINTAMT(30), XPRIN(30)
       REAL*4 XLOANBAL(30), XDEPR(30)
       REAL*4 XFUELCOST(30), XTAXDEDUCT(30)
       REAL*4 XMAINTCOST(30), XTOTALCOST(30)
       REAL*4 XVALESAVE(30),XKWH(30)
       REAL*4 XNETCASHFLOW(30), XPAYMENT, XDOWNPAY, BASIS
       REAL*4 XCUMNETFLOW(30)                         
       REAL*4 xtaxcreditclaimedtodate,XTOTTAXCREDIT   !total accumulated tax credits (up to total allowed) and total allowed credit
       Integer iTaxCreditCarryOver, iIntervalYrstoUse

!  DATA FROM DISTRIBUTED GEN INPUT FILE
       REAL*4 XDEGRAD(NTEK,MNUMYR),XELEFF(NTEK,MNUMYR), &
        XEQLIFE(NTEK,MNUMYR),XWHRECOVERY(NTEK,MNUMYR), &
        XINSTCOST(NTEK,MNUMYR),XCAPCOST(NTEK,MNUMYR), &
        XMAINTCST(NTEK,MNUMYR),XAVAIL(NTEK,MNUMYR),  &
        xIntervalCst(NTEK,MNUMYR),  &
        XTXCRPCT(NTEK,MNUMYR),xTXCrMaxPerKW(NTEK,MNUMYR),xTXCrMaxPerSys(NTEK,MNUMYR), &
        XKW(NTEK,MNUMYR),XOPERHOURS(NTEK),XLOSSFAC(NTEK,MNUMYR), &
        xKWMax(ntek), xkwMin(ntek), xScaleFac(ntek), ISKIP(NTEK),  &  !economies of scale for DG    !PVgen - remove XSOLARINS(MNUMCR)
        XALPHA(ntek),XPENPARM(ntek),XEXOGPEN(MNUMYR,MNUMCR,NTEK), &
        XBLDGSHARE(NTEK,CMNUMBLDG),xtxlife(ntek,MNUMYR),xdep(ntek,MNUMYR)
       INTEGER IFIRSTYR(NTEK,MNUMYR),ILASTYR(NTEK,MNUMYR), iIntervalYrs(NTEK,MNUMYR)
       INTEGER IFUELTYPE,NUMTECHS,NUMYEARS,NUMDIV,NVINT
       CHARACTER*10 AEQUIPNAME(NTEK,MNUMYR)

!    OTHER LOCAL VARIABLES
      INTEGER IYR,ICURIYR,NV,NT,IDIV,IBLD,NUMBLDG,CALYEAR,RCRPT,ILIFE
      Integer r,b,f,numerr,ios,rcdbg,count,i,iSize
      REAL*4  XMAXPEN,XSIMPLEPAYBACK,XPEN,XTEMP,xtaxlife
      REAL*4        xSqftPen,xtempSqft  
      LOGICAL LPRINT,LPRINT2,LPRINT3  !INCREASING OUTPUT DETAIL
      Logical ltempLprint,ltempLprint2,ltempLprint3
      INTEGER    FILE_MGR    ! FILE MANAGER
      INTEGER*4  INFILE      ! FILE HANDLE
      INTEGER*4  DGDB        ! FILE HANDLE FOR DG DATABASE FILE
      REAL*4     xunits, xtrills, xcapacity, xtrillsownuse, xfuelusage, xhwbtu, xshbtu, xinvest
      real XValue

!Loop cbecs12
!-----CBECS2012 - Incorporate the 6 CBECS size categories from CBECS 2012 and loop sizecats                    
!        avgsqft andd avgkwh data in whole numbers, flspccatshare adds to 1 across size cats within a bldg cat / census division
      REAL*4 cbecs12FlspcCatShare(CMNUMBLDG,9,6)                                                        
      REAL*4 cbecs12AVGSQFT(CMNUMBLDG,9,6)                                                              
      REAL*4 xelecavgkwh(CMNUMBLDG,9,6)
      REAL*4 xtest                                                                

!Internal Variables for IRR calcs
    REAL*4 xIRR, xDisc_L, xDisc_U, xNPV_L, xNPV_U, xNPV_new, xIRR_prev
    REAL*4 xNPV                             !real valued function

!Internal Niche Variables
    Integer iNiche, iRateLevel                   !9,4max,3
    Integer NumPVNiche(MNUMCR)                   !Dimension Div
    REAL*4 xSolarInsolation(MNUMCR,MaxNiche,3)     !Dimensions Div, MaxNiche, RateLevel
    REAL*4 xWindSpeed(MNUMCR,MaxNiche,3)           !Dimensions Div, MaxNiche, RateLevel
    REAL*4 xSqftShare(MNUMCR,MaxNiche,3)           !Dimensions Div, MaxNiche, RateLevel
    REAL*4 xRateScalar(MNUMCR,MaxNiche,3)          !Dimensions Div, MaxNiche, RateLevel
    REAL*4 xNGRateScalar(MNUMCR,MaxNiche,3)        !Dimensions Div, MaxNiche, RateLevel
    REAL*4 xRoofAreatoSqftRatio(MNUMCR,CMNUMBLDG,MaxNiche,3) !Dimensions Div, Bldg, MaxNiche, RateLevel
    REAL*4 SolarPVTechPotentialMW(CBECSyear-BASEYR+1:MNUMYR,MNUMCR)
    REAL*4 SolarPVAvailRoofArea(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG)
    REAL*4 SolarPVInstalledMW(CBECSyear-BASEYR+1:MNUMYR,MNUMCR)
    REAL*4 SolarPVUsedRoofArea(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG)
    REAL*4 WindTechPotentialMW(CBECSyear-BASEYR+1:MNUMYR,MNUMCR)
    REAL*4 WindAvailFS(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG)
    REAL*4 WindInstalledMW(CBECSyear-BASEYR+1:MNUMYR,MNUMCR)
    REAL*4 WindBldgCount(CBECSyear-BASEYR+1:MNUMYR,MNUMCR,CMNUMBLDG)
    REAL*4 xpctPVSuitable                           !Percentage of HH with a suitable south-facing roof
    REAL*4 xpctWindSuitable                         !Assumed percentage of HH for which wind could be appropriate 
    REAL*4 xSqftPerKW,xMpS

    REAL*4 xCalcKW,xCalcEqCost,xSizefromRoofArea,xSizefromAnnualKWH,xSizeMax,xSizeMin,xSizefromHeatBTU,xSolarIns    !PVgen - add xSolarIns

!------------------------------------------------------------
!   TEST FOR TRIGGER TO READ FILE AND BEGIN CALCULATIONS
!------------------------------------------------------------
      CALYEAR=ICURIYR + BASEYR - 1
      IF(ICURIYR.NE.CMFirstYr.OR.ICURITR.NE.1) GOTO 60  !proceed to calculations

!---Reads and Initializations for the First Call
!  Cogeneration Array Initialization
      CMCogenEl (1:MNUMCR-2,1:CMnumBldg,1:CMnumMajFl+CMnumMinFl+CMnumRenew+CMnumOthrFl,1:CMLastYr) = 0.0

!  READ OF KCOGEN DATA FILE
!   Read Base Year Cogen Data
!     Fuel 14 used for gas turbine generation instead of nuclear to enable
!     technology specific calculations for historical cogen data and to
!     establish base generation by technology.
        infile= FILE_MGR ('O','KCOGEN',.FALSE.)
        READ (infile,'(99(/))')  ! Skip over header
        IF(PRTDBGK.EQ.1)WRITE(RCDBG,*)'KCOGEN data set error trapping:'
!        WRITE(rcrpt,*)'KCOGEN data set error trapping:'
        count = 0
        NumErr = 0
!
       DO 10 iyr= 15, CMCogHstYr	!read in from 2004 kcogen
        DO 10 r= 1, MNUMCR-2
         DO 10 b= 1, CMnumBldg
         count = count + 1
         READ (infile,*,ERR=9,END=11,IOSTAT=IOS) &
            (CMCogenEl (r,b,f,iyr), &
              f= 1, CMnumMajFl+CMnumMinFl+CMnumRenew+CMnumOthrFl)
!          Dump data from kcogen for diagnositcs
!          Write (rcrpt,8) (CMCogenEl (r,b,f,iyr), &
!              f= 1, CMnumMajFl+CMnumMinFl+CMnumRenew+CMnumOthrFl)
          DO f= 1, CMnumMajFl+CMnumMinFl+CMnumRenew+CMnumOthrFl
           ! convert billions to trillions of BTUs:
            CMCogenEl (r,b,f,iyr)= &
             CMCogenEl (r,b,f,iyr) / 1000.
          END DO  ! f
         GO TO 10
 8       format(1x,15f8.1)
!
 9       CONTINUE ! Report read error and loop to next read
          NumErr = NumErr + 1
          IF(PRTDBGK.EQ.1) &
            WRITE(RCDBG,*) 'Comm_KCOGEN read err',IOS,' on record', &
                            count,'; skip record and continue read.'
 10    CONTINUE

       GO TO 12   ! EOF reached in KCOGEN as expected
 11    CONTINUE   ! EOF reached in KCOGEN prematurely; write msg
        NumErr = NumErr + 1
        IF(PRTDBGK.EQ.1) &
          WRITE(RCDBG,*) 'KCOGEN EOF reached prematurely at r =' &
                         ,r,' b =',b
!          WRITE(rcrpt,*) 'KCOGEN EOF reached prematurely at r =' &
!                         ,r,' b =',b
 12    CONTINUE
        infile = FILE_MGR ('C','KCOGEN',.FALSE.)
!
        IF(PRTDBGK.EQ.1) Then
          WRITE(RCDBG,*) NumErr,' errors detected.'
          WRITE(RCDBG,13)
!          WRITE(rcrpt,*) NumErr,' errors detected.'
!          WRITE(rcrpt,13)
 13       FORMAT(/,' KCOGEN data set error trapping complete!',/)
         ENDIF

!   End of read Cogen base year data.
!   Note -- the model will ignore any additional penetrations of conventional
!           cogen into buildings prior to and including the CMCogHstYr
!           in the file kgentk.  Enter zeros in kgentk during these years
!           for conventional technologies to avoid confusion.

!   READ THE KGENTK DATA FILE AND PRINT VALUES TO KRPT FOR VERIFICATION
      WRITE(rcrpt,*)'READING DISTRIBUTED GENERATION INPUT FILE'
      INFILE=FILE_MGR('O','KGENTK',.FALSE.) !OPEN THE DISTRIBUTED GEN DATA

!   Report Header
       WRITE (RCRPT,14) SCEN, DATE
 14    FORMAT (' Commercial Model Reports',/,'   Run Scenario= ',A10,/, &
         '   Run Datekey= ',A8,/)

! SKIP 20-LINE HEADER AND READ GENERAL CONTROL PARAMETERS AND INPUTS
      READ(INFILE,'(19(/))')
      READ(INFILE,*) NUMTECHS,NUMYEARS,NUMDIV,NUMBLDG
      WRITE(rcrpt,*) NUMTECHS,NUMYEARS,NUMDIV,NUMBLDG

!  -- DOLLAR YEAR FOR TECHNOLOGY CAPITAL AND MAINTENANCE COSTS
      READ(INFILE,'(//)')
      READ(INFILE,*) iGenCapCostYr
      WRITE(rcrpt,*) "iGenCapCostYr= ",igencapcostyr

!  -- Print Switches for Reporting Successively Greater Detail to KRPT
!     LPRINT TURNS on high level reports
!     Lprint2 give more details and traces execution
!     LPRINT3 PROVIDES DETAILS OF THE CASHFLOW CALCS FOR "VINTAGE" YEARS
      READ(INFILE, '(//)')
      READ(INFILE,*)LPRINT, LPRINT2, LPRINT3
      WRITE(rcrpt,*)LPRINT, LPRINT2, LPRINT3

!  -- FOR EACH OF THE TECHNOLOGIES ANNUAL OPERATING
!        HOURS (RELEVENT ONLY FOR FUEL USING GENERATION TECHNOLOGIES)
      READ(INFILE, '(//)')
      READ(INFILE,*) (XOPERHOURS(NT), NT=1,NUMTECHS)
!   Make sure to change this and format statements 87 and 94 when adding technologies
      WRITE(rcrpt,'(11F9.2)') (XOPERHOURS(NT), NT=1,NUMTECHS)

! -- Global Learning Switch 
!    Zero Represents Learning from Domestic Residential and Commercial Shipments
!    One Represents Learning "Globally" Adding Industrial and International Shipmnets (if tracked)
      READ(INFILE, '(//)')
      READ(INFILE,*) GlobalLearn
      WRITE(rcrpt,*) "Global Learning = ",GlobalLearn

! -- Learning Parameters
!    FOR EACH OF THE TECHNOLOGIES, Learning Betas (doubling parameter, 0=no learning)
      READ(INFILE, '(//)')
      READ(INFILE,*) (XBeta(NT), NT=1,NUMTECHS)
      WRITE(rcrpt,'(11F9.2)') (XBeta(NT), NT=1,NUMTECHS)
!    FOR EACH OF THE TECHNOLOGIES, Learning "c0" parameters (initial costs)
      READ(INFILE, '(//)')
      READ(INFILE,*) (Xc0(NT), NT=1,NUMTECHS)
      WRITE(rcrpt,'(11F9.2)') (Xc0(NT), NT=1,NUMTECHS)

! -- Interconnection limitations  !Inxlimit
      READ(INFILE, '(//)')
      READ(INFILE,*) (XINX(i),i=1,NUMDIV)        
      WRITE(rcrpt,'(9F7.3)') (XINX(I),I=1,NUMDIV)
!  -- The above are scalars for limiting the penetration based on interconnection index
      READ(INFILE,*) XINXFY, XINXLY
      WRITE(rcrpt,'(3I8)') XINXFY, XINXLY   
!  -- The above are the first and last years of the interconnection limits


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
      READ(INFILE, '(//)')
      READ(INFILE,*)XTAXRATE,XDOWNPAYPCT,XINTRATE,XTERM,XINFLATION
      WRITE(rcrpt,'(5F7.2)') XTAXRATE, XDOWNPAYPCT, XINTRATE, XTERM, &
         XINFLATION

!  POPULATE TECHNOLOGY ARRAY
      READ(INFILE, '(////)')
      READ(INFILE, *) NVINT

      AEQUIPNAME = "          "
      IFUELTYPE = 0.0
      IFIRSTYR = 0.0
      ILASTYR = 0.0
      XKW = 0.0
      XELEFF = 0.0
      XLOSSFAC = 0.0
      XDEGRAD = 0.0
      XEQLIFE = 0.0
      xtxlife = 0.0
      xdep = 0.0
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

      DO NT=1,NUMTECHS
!  -- XALPHA AND XPENPARM ISKIP CONTROL THE MAGNITUDE AND SHAPE OF THE
!       PENETRATION FUNCTION OF DISTRIBUTED GENERATION TECHNOLOGIES.
!       SEVERAL TECHNOLOGIES (CURRENTLY 4,5,6,8,9&10) ARE INCLUDED 
!       IN ORDER TO TIE OUT TO EIA'S 860B COGEN DATA AND ARE NOT 
!       MODELED FOR FURTHER PENETRATION IN NEMS.  ISKIP INDICATES
!       WHETHER FURTHER PENETRATION IS MODELED (0=NO FURTHER, 1=MODELED)
!       THE RPS START YEAR IS THE FIRST YEAR THAT RPS CREDITS ARE GIVEN UNDER THE LEGISLATION
!       THE RPS PHASE OUT YEAR MAY APPLY, IF NOT SET TO SOME ARBITRARY FUTURE DATE LIKE 2099
!       THE RPS GRANDFATHER YEAR IS THE FIRST YEAR THAT DG CONSTRUCTED BEFORE THE LEGISLATION CAN GET MULTIPLE CREDITS
      READ(INFILE,*) XALPHA(nt), XPENPARM(nt), xkwMin(nt), xkwmax(nt), xScaleFac(nt), ISKIP(nt), iRPSStartYear(nt), iRPSPhaseOutYear(nt), iRPSGrandFatherYear(nt)
      WRITE(rcrpt,*) XALPHA(nt), XPENPARM(nt), xkwMin(nt), xkwmax(nt), xScaleFac(nt), ISKIP(nt), iRPSStartYear(nt), iRPSPhaseOutYear(nt), iRPSGrandFatherYear(nt)

       DO NV=1,NVINT
!      THE CURRENT TECHNOLOGIES ARE:
!       NT=1 IS SOLAR PHOTOVOLTAIC TECHNOLOGY
!       NT=2 IS FUEL CELL TECHNOLOGY
!       NT=3 IS GAS ENGINE TECHNOLOGY
!       NT=4 IS CONVENTIONAL COAL TECHNOLOGY
!       NT=5 IS CONVENTIONAL OIL TECHNOLOGY
!       NT=6 IS CONVENTIONAL MSW TECHNOLOGY
!       NT=7 IS GAS TURBINE TECHNOLOGY
!       NT=8 IS MICRO TURBINE TECHNOLOGY
!       NT=9 IS HYDROPOWER TECHNOLOGY
!       NT=10 IS CONVENTIONAL WOOD TECHNOLOGY
!       NT=11 IS DISTRIBUTED WIND TURBINES (DWT)
!
!  --  AEQUIPNAME IS NAME FOR REPORTING PURPOSES
!  --  IFUELTYPE IS THE FUEL USED BY THE TECHNOLOGY (0 FOR SOLAR OR WIND)
!         THIS FUEL TYPE MUST COINCIDE WITH THE MAIN MODEL DEFINITION OF
!         FUELS
!  --  IFIRSTYR IS THE FIRST YEAR A TECHNOLOGY CAN BE PURCHASED
!  --  ILASTYR IS THE LAST YEAR A TECHNOLOGY CAN BE PURCHASED (DON'T
!         ALLOW TECHNOLOGIES TO "OVERLAP" OR "GAP" (E.G. VINTAGE 1 2003-2005,
!         VINTAGE 2, 2006-2009, VINTAGE 3, 2010-2013, VINTAGE 4, 2014....
!  --  XELEFF IS THE ELECTRICAL CONVERSION EFFICIENCY OF THE TECHNOLOGY
!  --  XLOSSFAC IS THE LOSS FACTOR FROM GENERATION TO END USE 'INCLUDES LINE LOSS,
!         INVERTER LOSSES, ETC....
!  --  XDEGRAD IS THE ANNUAL DEGRADATION IN THE EFFICIENCY OF SOLAR PV
!  --  XEQLIFE IS THE EQUIPMENT LIFE IN YEARS
!  --  XWHRECOVERY IS THE PERCENTAGE OF WASTE HEAT (FOR FUEL USING TECHNOLOGIES)
!         THAT CAN BE RECOVERED FOR WATER HEATING (ANYTHING IN EXCESS OF AVERAGE
!         WATER HEATING REQUIREMENTS IS ASSUMED WASTED)
!  --  XINSTCOST INSTALLATION COST PER KW IN iGenCapCostYr DOLLARS
!  --  XCAPCOST CAPITAL COST PER KW IN iGenCapCostYr DOLLARS
!  --  XMAINTCST ANNUAL MAINTENANCE COST IN iGenCapCostYr DOLLARS
!  --  xIntervalCst is the replacement cost for interters per kw
!  --  iIntervalYrs is the number of years to the first inverter replacement 
!  --  XAVAIL = 1.0-FORCED OUTAGE RATE
!  --  XTXCRPCT IS THE TAX CREDIT PERCENTAGE FOR THIS TECHNOLOGY
!  --  xTXCrMaxPerKW IS THE MAXIMUM DOLLAR AMOUNT (PER KW) OF THE TAX CREDIT
!  --  xTXCrMaxPerSys IS THE MAXIMUM DOLLAR AMOUNT (PER SYSTEM) OF THE TAX CREDIT

            READ(INFILE,*,End=50) AEQUIPNAME(NT,NV), IFUELTYPE(NT), &
                     IFIRSTYR(NT,NV),    ILASTYR(NT,NV),      &
                          XKW(NT,NV),    XELEFF(NT,NV),       &
                     XLOSSFAC(NT,NV),    XDEGRAD(NT,NV),      &
                      XEQLIFE(NT,NV),    xtxlife(nt,nv),      &
                         xdep(nt,nv),    XWHRECOVERY(NT,NV),  &
                    XINSTCOST(NT,NV),    XCAPCOST(NT,NV),     &
                    XMAINTCST(NT,NV),    xIntervalCst(NT,NV), & 
                 iIntervalYrs(NT,NV),    XAVAIL(NT,NV),       & 
                     XTXCRPCT(NT,NV),    xTXCrMaxPerKW(NT,NV),  xTXCrMaxPerSys(NT,NV),  &
         (comtxcrpct_div(nv,idiv,nt),idiv=1,9),     &                                     !111dren
                               xtemp,    itemp1, &
                              itemp2
                         
      WRITE(rcrpt,15) AEQUIPNAME(NT,NV),  IFUELTYPE(NT), &
                     IFIRSTYR(NT,NV),    ILASTYR(NT,NV),      &
                          XKW(NT,NV),    XELEFF(NT,NV),       &
                     XLOSSFAC(NT,NV),    XDEGRAD(NT,NV),      &
                      XEQLIFE(NT,NV),    xtxlife(nt,nv),      &
                         xdep(nt,nv),    XWHRECOVERY(NT,NV),  &
                    XINSTCOST(NT,NV),    XCAPCOST(NT,NV),     &
                    XMAINTCST(NT,NV),    xIntervalCst(NT,NV), & 
                 iIntervalYrs(NT,NV),    XAVAIL(NT,NV),       & 
                     XTXCRPCT(NT,NV),    xTXCrMaxPerKW(NT,NV), xTXCrMaxPerSys(NT,NV), &
					 (comtxcrpct_div(nv,idiv,nt),idiv=1,9),     &                                     !111dren
                               xtemp,    itemp1, &
                              itemp2

             ! Immediately populate the RPS variable mapping vintages to Icuriyr
             !  this allows interval characterizations for DG equipment
             do iyr=ifirstyr(nt,nv),ilastyr(nt,nv)
              xRPS(nt,iyr-1989)=xtemp
              iNumYrsatRPSBaseRate(nt,iyr-1989)=itemp1
              iNumRPSCreditYrs(nt,iyr-1989)=itemp2
             enddo  
              
            END DO   !Technology Vintages (NV)
            END DO   !Technology Types (NT)
 15   FORMAT(1X,A10,3I6,F9.0,f9.3,f9.1,f9.2,f9.0,F9.1,f9.0,f9.3,2f9.0,/,5x,f9.1,f9.0,i5,2f9.2,2f9.0,9f7.3,f5.0,2i3)

!  -- SOLAR INSOLATION, SQFT SHARES, AVERAGE ELEC RATES RELATIVE TO CENSUS DIV, ROOF TO SQFT RATIOS, 
!        WIND SPEED & NG RATE SCALERS
!      THESE VALUES ARE ESTIMATED FROM CBECS CLIMATE ZONES BY CENSUS DIVISION OVERLAYED 
!      ONTO A MAP OF PV SOLAR RADIATION (LATITUDED TILT) DEVELOPED BY NREL'S
!      ELECTRIC & HYDROGEN TECHNOLOGIES & SYSTEMS CENTER - MAY 2004
      READ(INFILE, '(///)')
      DO I=1,NUMDIV
       READ(INFILE,*) iDiv, iNiche
     Write(rcrpt,*) "iDiv, iNiche", idiv, iniche
     NumPVNiche(iDiv)= iNiche
      Do iNiche=1,NumPvNiche(iDiv)
          Do iRateLevel=1,3
              READ(INFILE,*) xSolarInsolation(iDiv,iNiche,iRateLevel),xSqftShare(iDiv,iNiche,iRateLevel), &
                xRateScalar(iDiv,iNiche,iRateLevel), (xRoofAreatoSqftRatio(iDiv,iBld,iNiche,iRateLevel),Ibld=1,11),  &
                xWindSpeed(iDiv,iNiche,iRateLevel),xNGRateScalar(iDiv,iNiche,iRateLevel)
              WRITE(rcrpt,16) iDiv,xSolarInsolation(iDiv,iNiche,iRateLevel),xSqftShare(iDiv,iNiche,iRateLevel), &
                xRateScalar(iDiv,iNiche,iRateLevel), (xRoofAreatoSqftRatio(iDiv,iBld,iNiche,iRateLevel),Ibld=1,11),  &
                xWindSpeed(iDiv,iNiche,iRateLevel),xNGRateScalar(iDiv,iNiche,iRateLevel)

          END DO
        END DO
      END DO

 16   FORMAT(1x,"Div= ",I3,3F10.4,11F7.2,2F10.4)

!   BUILDING SHARE DATA FOR THE EXOGENOUS PENETRATIONS
      READ(INFILE, '(//)',End=50)
      WRITE(rcrpt,*) (AEQUIPNAME(NT,1),NT=1,NUMTECHS)
      DO IBLD=1,NUMBLDG
             READ(INFILE,*,End=50) (XBLDGSHARE(NT,IBLD),NT=1,NUMTECHS)
      WRITE(rcrpt,17) BLDGNAME(IBLD),(XBLDGSHARE(NT,IBLD),NT=1,NUMTECHS)
!   Make sure to change format statement 94 when adding technologies
 17   FORMAT(A15,11F5.2)
      END DO

!CBECS Data
      READ(INFILE, '(//)',End=50)
      WRITE(rcrpt,*) "CBECS Data:  Floorspace shares by census div / size cat, bldg types across"
      READ(INFILE,*,End=50) (((cbecs12FlspcCatShare(ibld,idiv,isize),ibld=1,11),isize=1,6),idiv=1,9)
      WRITE(rcrpt,18)(((cbecs12FlspcCatShare(ibld,idiv,isize),ibld=1,11),isize=1,6),idiv=1,9)

      READ(INFILE, '(//)',End=50)
      WRITE(rcrpt,*) "CBECS Data:  Average building sqft by census div / size cat, bldg types across"
      READ(INFILE,*,End=50) (((cbecs12AVGSQFT(ibld,idiv,isize),ibld=1,11),isize=1,6),idiv=1,9)
      WRITE(rcrpt,19)(((cbecs12AVGSQFT(ibld,idiv,isize),ibld=1,11),isize=1,6),idiv=1,9)

      READ(INFILE, '(//)',End=50)
      WRITE(rcrpt,*) "CBECS Data:  Average electricity consumption by census div / size cat, bldg types across"
      READ(INFILE,*,End=50) (((xelecavgkwh(ibld,idiv,isize),ibld=1,11),isize=1,6),idiv=1,9)
      WRITE(rcrpt,19)(((xelecavgkwh(ibld,idiv,isize),ibld=1,11),isize=1,6),idiv=1,9)

 18   FORMAT(11F12.4)
 19   FORMAT(11F12.0)


! Check CBECS shares by size category
      do ibld=1, 11
        xtest=0.
        do iSize = 1 , 6
          do idiv=1 , 9
            xtest=xtest+cbecs12FlspcCatShare(ibld,idiv,isize)
          enddo
        end do
        If (xtest<8.999 .or. xtest>9.001) Write (RCRPT,*) "***Share by Size Class Issue ","Bldg ",ibld,xtest
      enddo

!   EXOGENOUS PENETRATION DATA RELATED TO FUNDED PROGRAMS AND DEMONSTRATIONS
      DO IDIV=1,NUMDIV
      DO NT=1,NUMTECHS
      READ(INFILE, '(//)',End=50)
               READ(INFILE,*,End=50) (XEXOGPEN(IYR,IDIV,NT),IYR=CMFirstYr,NUMYEARS)
      WRITE(rcrpt,*) 'TECHNOLOGY ',NT,'DIVISION ',IDIV
               WRITE(rcrpt,20) (XEXOGPEN(IYR,IDIV,NT),IYR=1,NUMYEARS)
 20   FORMAT(10F10.0)
      END DO
      END DO

      GOTO 51

       ! Serious Input Error Encountered
 50    WRITE(rcrpt,*) &
          'INPUT ERROR ON COMMERCIAL DIST GEN TECHNOLOGY DATA FILE'

 51    INFILE=FILE_MGR('C','KGENTK',.FALSE.)
!-----END OF READ OF KGENTK DATA FILE

!  INITIALIZE ACCUMULATING VARIABLES
!-----Assumptions for developing technical potential for PV and Wind
        xpctPVSuitable=.40       !Assumed percentage of PV suitable roof area
        xpctWindSuitable=.10     !Assumed percentage of floorspace for which wind could be appropriate 
!-----Initialize main accumulators and debug print switches
        ltempLprint=lprint
        ltempLprint2=lprint2
        ltempLprint3=lprint3
        !Initialize Accumulators 
        DO IYR=23,NUMYEARS			
        DO IDIV=1,NUMDIV
         SolarPVTechPotentialMW(IYR,IDIV)=0.
         WindTechPotentialMW(IYR,IDIV)=0.
         WindInstalledMW(Iyr,IDIV)=0.
         SolarPVInstalledMW(IYR,IDIV)=0.
         CGCPVCOM(IDIV,ICuriyr)=1.
         CGCWNCOM(IDIV,ICuriyr)=1.
        DO IBLD=1,NUMBLDG
         SolarPVAvailRoofArea(IYR,IDIV,IBLD)=0.
         WindAvailFS(IYR,IDIV,IBLD)=0.
         SolarPVUsedRoofArea(IYR,IDIV,IBLD)=0.
        DO NT=1,NUMTECHS
          QCMDGSG(IYR,IDIV)=0.
          UNITS(IYR,IDIV,IBLD,NT)=0.
          xCumPenSqft(IYR,IDIV,IBLD,NT)=0.
          TRILLS(IYR,IDIV,IBLD,NT)=0.
          CAPACITY(IYR,IDIV,IBLD,NT)=0.  
          HWBTU(IYR,IDIV,IBLD,NT)=0.
          SHBTU(iyr,idiv,ibld,nt)=0.
          FUELUSAGE(IYR,IDIV,IBLD,NT)=0.
          TRILLSOWNUSE(IYR,IDIV,IBLD,NT)=0.
		  com111drensub(iyr,idiv,nt)=0.  !111dren
        END DO
        END DO
        END DO
        END DO
		
!---End of Reads and Initializations for the First Call

!---Beginning of Calculations after First Call
 60    CONTINUE

   !First Initialize Current Year Accumulators to Allow for Multiple NEMS Interations  
        DO IDIV=1,NUMDIV
         SolarPVTechPotentialMW(icuriyr,IDIV)=0.
         WindTechPotentialMW(icuriyr,IDIV)=0.
         WindInstalledMW(icuriyr,IDIV)=0.
         SolarPVInstalledMW(icuriyr,IDIV)=0.
        DO IBLD=1,NUMBLDG
         SolarPVAvailRoofArea(icuriyr,IDIV,IBLD)=0.
         WindAvailFS(icuriyr,IDIV,IBLD)=0.
         SolarPVUsedRoofArea(icuriyr,IDIV,IBLD)=0.
        DO NT=1,NUMTECHS
          QCMDGSG(icuriyr,IDIV)=0.
          UNITS(icuriyr,IDIV,IBLD,NT)=0.
          xCumPenSqft(icuriyr,IDIV,IBLD,NT)=0.
          TRILLS(icuriyr,IDIV,IBLD,NT)=0.
          CAPACITY(icuriyr,IDIV,IBLD,NT)=0.  
          HWBTU(icuriyr,IDIV,IBLD,NT)=0.
          SHBTU(icuriyr,idiv,ibld,nt)=0.
          FUELUSAGE(icuriyr,IDIV,IBLD,NT)=0.
          TRILLSOWNUSE(icuriyr,IDIV,IBLD,NT)=0.
		  com111drensub(icuriyr,idiv,nt)=0.  !111dren
        END DO
        END DO
        END DO

!---BEGIN PROCESSING HISTORICAL COGEN DATA  !icuriyr <= CMCogHstYr
      if (icuriyr .le. CMCogHstYr) then   
!  POPULATE HISTORICAL COGEN DATA
!  Compute historical year Cogen Penetrations into Buildings based on data
!     in KCOGEN and using average capacities and hours of use from kgentk
        TotUnits=0.
        TotCogenEl=0.
        DO 90 r= 1, MNUMCR-2
         DO 90 b= 1, CMnumBldg
          DO 90 f= 1, CMnumMajFl+CMnumMinFl+CMnumRenew+CMnumOthrFl
!   Note: CMCogenEL is electricity generated by fuel in trillions of BTUs

!       Skip entries with no cogen
            If (CMCogenEL(r,b,f,icuriyr).eq.0.)goto 90

!        KGenTK has conventional technologies using Gas(2), Distillate(3)
!             coal(6), Wood (9), MSW (10),  and Hydro (11).
!     Fuel 14 used for gas turbine generation to split from gas engine
!     generation for technology specific calculations.

          if(f.eq.2)goto 70
          if(f.eq.3)goto 70
          if(f.eq.6)goto 70
          if(f.eq.9)goto 70
          if(f.eq.10)goto 70
          if(f.eq.11)goto 70
          if(f.eq.14)goto 70  ! gas turbine generation in fuel 14.
          goto 90 !skip over if not a traditional cogen fuel

 70       Continue

! Calculate Base Year Number of Buildings by Census Div,
!  Building Type and Fuel

!  KGENTK NOTE:
!        Retain technology positions (NT) 1 and 2 for PV and Fuel Cells
!         respectively.  Most traditional technologies follow these 2.
!         Microturbines are in technology position 8.  Existing Hydro 
!         and wood are in technology positions 9 and 10. For calculation of
!         conventional cogen from the base year file, skip over PV, wind and
!         fuel cells and don't allow micro turbines to pick up units either.

!  Map fuel in CMCogEl array into tradional cogen equipment types
        Do nt=3,6                                ! gas engines, coal, oil and MSW
         If (Ifueltype(nt).eq.f) goto 80
        END DO

		nt=7                                     ! gas turbines
        If (f .eq. 14) goto 80

        Do nt=9,10                               ! hydro and wood
         If (Ifueltype(nt).eq.f) goto 80
        END DO

!  If here, then ERROR, a traditional cogen technology in kgentk did not exist
       Write (6,*)"Error in Cogen Calcs f = ",f," nt = ",nt
        STOP

!  Units conversions:  CMCogenEl is in trills at the time of this calc
!  convert to the estimated number of buildings estimated to have cogen
!  
 80   continue
           DO NV=1,NVINT  !select appropriate vintage  
            !Go to next Vintage if the year is not in the current vintage interval
            IF(IFIRSTYR(NT,NV).GT.icuriyr+1989) GOTO 85
            IF(ILASTYR (NT,NV).LT.icuriyr+1989) GOTO 85

            UNITS(icuriyr,r,b,NT) = CMCogenEL(r,b,f,icuriyr)*10.**12  & !CMCogenEl in trills
             / ( xoperhours(NT)*xkw(nt,nv)*xlossfac(nt,nv)*xavail(nt,nv)*3412. )                     

            IF (icuriyr .eq. 15) units (icuriyr-1,r,b,nt) = 0.  ! zero for year prior to first year for xtemp calc
!  Output
      IF(LPRINT) &
       WRITE (RCRPT,81)icuriyr,nv,r,b,nt, units(icuriyr,r,b,nt), CMCogenEL(r,b,f,icuriyr)
      TotUnits=TotUnits+Units(icuriyr,r,b,nt)
      TotCogenEl=Totcogenel+cmcogenel(r,b,f,icuriyr)
 81   format(1x,"Cogen Base Data: icuriyr=",i3," vint= ",i3," r=", i3," b=",i3," nt=",i3," units= ", &
         f7.1," elec gen= ",f7.3)

! Select the appropriate vintage
 85   continue
      enddo

 90  Continue
       if (lprint)then
         WRITE (RCRPT,*) icuriyr+1989," Estimated Historical Cogen Total Units",TotUnits
         WRITE (RCRPT,*) icuriyr+1989," Historical Year Energy from Input(trills)",Totcogenel
       endif

      endif  
!---END OF PROCESSING OF HISTORICAL DG DATA  !icuriyr <= CMCogHstYr 

!-------------------------------------------------------------
!  BEGIN ECONOMIC PENETRATION MODELING FOR PROJECTED DG BUILDS
!-------------------------------------------------------------
!
!----MAIN LOOPS FOR TECHNOLOGIES AND VINTAGES
       DO NT=1,NUMTECHS
             !  Technology Definitions:
                   !      NT=1 IS SOLAR PHOTOVOLTAIC TECHNOLOGY
                   !      NT=2 IS FUEL CELL TECHNOLOGY
                   !      NT=3 IS GAS ENGINE TECHNOLOGY
                   !      NT=4 IS CONVENTIONAL COAL TECHNOLOGY
                   !      NT=5 IS CONVENTIONAL OIL TECHNOLOGY
                   !      NT=6 IS CONVENTIONAL MSW TECHNOLOGY
                   !      NT=7 IS GAS TURBINE TECHNOLOGY
                   !      NT=8 IS MICRO TURBINE TECHNOLOGY
                   !      NT=9 IS HYDROPOWER TECHNOLOGY
                   !      NT=10 IS CONVENTIONAL WOOD TECHNOLOGY
                   !      NT=11 IS DISTRIBUTED WIND TECHNOLOGY

        !Disable printing unless tech = PV        
        lprint= .FALSE.
        lprint2= .FALSE.
        lprint3= .FALSE.
        if ((NT==1.or.nt==11))lprint=ltemplprint
        if ((NT==1.or.nt==11).and.imod(calyear,10)==0)lprint2=ltemplprint2
        if ((NT==1.or.nt==11).and.imod(calyear,10)==0)lprint3=ltemplprint3
       DO NV=1,NVINT  

            !Go to next Vintage if the NEMS year is not in the current vintage interval
            IF(IFIRSTYR(NT,NV).GT.CALYEAR) GOTO 450  !450 is continue for next vintage
            IF(ILASTYR (NT,NV).LT.CALYEAR) GOTO 450

             !Print Requested Diagnostics
             IF(lprint2) THEN
              IF(NT.EQ.1) THEN
               WRITE(rcrpt,*)'BEGINNING CASHFLOW CALCS FOR SOLAR PV SYSTEMS'
              ELSE
               WRITE(rcrpt,*) &
                'BEGINNING CASHFLOW CALCS FOR FUEL CELL / OTHER SYSTEMS'
              ENDIF
             ENDIF

            !
            ! Update "Learned" Costs for PV, Fuel Cells, Wind and Micro Turbines
            !
            xadjcost=0.
            if(aequipname(nt,nv) .eq. "Fuel_Cell" )then
                  cumship=CFuelCell_MW(icuriyr-1)+ RFuelCell_MW(icuriyr-1)! buildings shipments only
                  ! Not Enabled ! If (globallearn)cumship=CFuelCell_MW(icuriyr-1)+ RFuelCell_MW(icuriyr-1) & 
                  ! +UFuelcell_MW(icuriyr-1)+ IntnlFuelCell_MW(icuriyr-1)+ IFuelCell_MW(icuriyr-1)
                  xadjcost = LearnCost (xcapcost(nt,nv), xbeta(nt), xc0(nt), cumship, rcrpt)
            elseif(aequipname(nt,nv) .eq. "Solar_PV" )then
                  if(globallearn) then 
                       ! globallearn=true adds electric generator PV installations to cumulative shipments
                       cumship=CPV_MW(icuriyr-1) +RPV_MW(icuriyr-1) +UPV_MW(icuriyr-1)
                    else
                       cumship=CPV_MW(icuriyr-1) +RPV_MW(icuriyr-1)
                  endif

                  ! Not Enabled ! If(globallearn.eq.1) cumship=CPV_MW(icuriyr-1) +RPV_MW(icuriyr-1)+ UPV_MW(icuriyr-1) &
                  ! + IntnlPV_MW(icuriyr-1)+IPV_MW(icuriyr-1)
                  xadjcost = LearnCost (xcapcost(nt,nv), xbeta(nt), xc0(nt), cumship, rcrpt)
            elseif(aequipname(nt,nv) .eq. "Wind" )then
                  cumship=CWind_MW(icuriyr-1) +RWind_MW(icuriyr-1)
                  ! Not Enabled ! If(globallearn) cumship=CWind_MW(icuriyr-1) +RWind_MW(icuriyr-1)+ UWind_MW(icuriyr-1) &
                  ! + IntnlWind_MW(icuriyr-1)+IWind_MW(icuriyr-1)
                  xadjcost = LearnCost (xcapcost(nt,nv), xbeta(nt), xc0(nt), cumship, rcrpt)
            elseif(aequipname(nt,nv) .eq. "Micro_Tur" )then
                  cumship=CMicroTur_MW(icuriyr-1) +RMicroTur_MW(icuriyr-1)
                  ! Not Enabled ! If(globallearn) cumship=CMicroTur_MW(icuriyr-1) +RMicroTur_MW(icuriyr-1)+ UMicroTur_MW(icuriyr-1) &
                  ! + IntnlMicroTur_MW(icuriyr-1) + IMicroTur_MW(icuriyr-1)
                  xadjcost = LearnCost (xcapcost(nt,nv), xbeta(nt), xc0(nt), cumship, rcrpt)
            else  !no learning for other technologies
                  xadjcost=xcapcost(nt,nv)
           endif
            ! End of Update Learned Costs

            !
            ! Initialize Technology Specific Variables for Cash Flow Model
            !

            !  Print "Menu" Costs vs Learned Costs for Technologies with Learning
            IF (aequipname(nt,nv) .eq. "Fuel_Cell"   .or.    &
                aequipname(nt,nv) .eq. "Solar_PV"    .or.    &
                aequipname(nt,nv) .eq. "Wind"        .or.    &
                aequipname(nt,nv) .eq. "Micro_Tur") THEN    
             WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' results_for_technology_learning.'
             WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' initial_cost ',xc0(nt)
             WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' learning_beta ',xbeta(nt)
             WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' cumulative_shipments_(MW)',CumShip
             if(aequipname(nt,nv) .eq. "Solar_PV") WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' residential_shipments_(MW)',RPV_MW(icuriyr-1)
             if(aequipname(nt,nv) .eq. "Solar_PV") WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' commercial_shipments_(MW)',CPV_MW(icuriyr-1)
             if(aequipname(nt,nv) .eq. "Solar_PV") WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' generator_shipments_(MW)',UPV_MW(icuriyr-1)
             WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' kgentk_default_installed_cost ',     &
                  (xcapcost(nt,nv)+xinstcost(nt,nv))
             WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' learning-adjusted_capital_cost ',xadjcost
             WRITE(rcrpt,*)icuriyr+baseyr-1, '  ',aequipname(nt,nv),' learning-adjusted_installed_cost ',  &
                  (xadjcost+xinstcost(nt,nv))
            END IF

! For each technology, loop over Census Divisions, Building Types
            DO IDIV=1,NUMDIV
            !  Set the grid sales price (passed from electric generator model) and convert to
            !   same base year as capital costs.  The internal NEMS energy pricies are in 1987 $/mmBtu 
            !   converted to the dollar year of the DG capacity costs and to per kWh basis using .003412  
            !   Note: MC_JPGDP(-2) is the deflator for 1987.
            xSalestoGridPRnoRPS=PELME(IDIV,ICURIYR)*.003412*MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2) 

            DO IBLD=1,NUMBLDG
             ! Initialization to output accumulating variables before commencing IDIV, IBLD calculations
             xunits=0.
             xtrills=0.
             xcapacity=0.
             xtrillsownuse=0.
             xfuelusage=0.
             xhwbtu=0.
             xshbtu=0.
             xinvest=0.
             xSqftPen=0.
             DO iSize = 1, 6                                                                    !Loop cbecs12
              Do iNiche=1,NumPVNiche(iDiv)                                                      !Add Insolation Niches
              Do iRateLevel=1,3                                                                 !High, Mid and Low Avg Rates 

              xRetailElecPRnoRPS= PELCM(IDIV,ICURIYR)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)  
              !   Note: the retail price will be overwritten in the PV section to convert to the air conditioning price

              ! Skip over unobserved bldg/size combos
              If(cbecs12FlspcCatShare(IBLD,iDiv,iSize).eq.0.) goto 400                                 !Loop cbecs12 

              If(lprint2) WRITE(rcrpt,91) AEQUIPNAME(NT,NV),IDIV,IBLD,iSize,iniche, &
               xSolarInsolation(iDiv,iNiche,iRateLevel),xRateScalar(iDiv,iNiche,iRateLevel)
 91           Format(1x,A15,", Div ",i2," Bldg ",i2," SizeCat ",i2," Niche ",i2," Insol ", F6.2," RateMult ", F6.2)
              IF(IFUELTYPE(NT).NE.0)GOTO 150                         !Skip Over Set Up Calculations for Solar & Wind

! Solar Calculations
              If(nt==11) goto 100 !Wind
              !------------------------------ 
              ! Setup for Solar Calculations 
              !------------------------------
              ! Overwrite the retail price for PV, instead valuing at average electricity price for air conditioning (end use 2).
              !  This usage of the air conditioning price is to reflect average summer prices, when PV output is at its highest.
              !  Units are in gencapcostyr$/kWh including RPS credit (passed from NEMS in mills per kWh)
              xRetailElecPR=( PELCMOUT(IDIV,ICURIYR,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                     +.001*EPRPSPR(ICURIYR)*xRPS(nt,icuriyr)) & 
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2) 
              ! For grid sales add RPS credits for this technology 
              xSalestoGridPR=( PELME(IDIV,ICURIYR)*.003412+.001*EPRPSPR(ICURIYR)*xRPS(nt,icuriyr) )  &
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)
              xRetailElecPRnoRPS=PELCMOUT(IDIV,ICURIYR,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2) !gencapcostyr$/kWh without RPS credit
              ! For the analysis of Markey-type RPS credits which adjust after some number of years, revert credit to 1.0                     
              xSalestoGridPRadjRPS=( PELME(IDIV,ICURIYR)*.003412+.001*EPRPSPR(ICURIYR)*1.0 )  &
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)
              xRetailElecPRadjRPS=( PELCMOUT(IDIV,ICURIYR,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                     +.001*EPRPSPR(ICURIYR)*1.0 ) & 
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2) 

              !Just in Case RPS Credits come through in years before they should be credited to commercial:
              If (CALYEAR .lt. iRPSStartYear(NT)) then
                xRetailElecPR=xRetailElecPRnoRPS
                xSalestoGridPR=xSalestoGridPRnoRPS
              Endif
              
              ! Account for total roof area for potential Solar
              SolarPVAvailRoofArea(ICuriyr,IDIV,IBLD)= SolarPVAvailRoofArea(ICuriyr,IDIV,IBLD) + &
               (SurvFloorTotal(IDIV,IBLD,ICURIYR)+CMNewFloorSpace(IDIV,IBLD,ICURIYR)) &
               * cbecs12FlspcCatShare(IBLD,iDiv,iSize)*xSqftShare(iDiv,iNiche,iRateLevel)  &
               * xRoofAreatoSqftRatio(IDiv,iBld,iNiche,iRateLevel)*xpctPVSuitable

              ! Calculation of KWH supplied for solar
              !  The quantity "77.*(.14/xeleff)*xkw" represents the estimated module square footage.
              !  Thus the kWh supplied is:
              !      annualkwh=eff*insolation*sqftperkw*systemkw*lossfac 
              !  where lossfac represents average AC output per DC KW (inverter losses, orientation effects, etc.)
              !  Note: future efficiency gains will result in a smaller collector footprint for a given kW capacity.
              !  Solar insolation is in kWh/m2/day convert to annual per sqft (365.25/10.8)
              xSqftperKW=(77.*0.14/XELEFF(NT,NV)) 

              ! Optimize capacity for up to 100% of kWh requirements, or roof area and subject to global max
              !  Note: For flat roofs, roughly double the roof area is needed for latitude mounting.  Assuming
              !    75% of commercial roofs are flat means that available roof area must be deflated by 175% to allow for 
              !    proper spacing (0.25*1.0 + 0.75*2.0)
              if (CALYEAR > 2000) then
                 xSolarIns=xSolarInsolation(iDiv,INiche,IRateLevel)                               !PVgen
                 ! transform solar insolation to account for unknown orientations                 !PVgen
                 xSolarIns= -1.0686 + 1.4421*xSolarIns - 0.0645*xSolarIns**2                      !PVgen
                 xSizefromRoofArea = (cbecs12AVGSQFT(ibld,iDiv,isize)* xRoofAreatoSqftRatio(IDiv,iBld,iNiche,iRateLevel) &
                             * xpctPVSuitable/xSqftperKW)/1.75 
                 xSizefromAnnualKWH=XELECAVGKWH(IBLD,idiv,iSize) &
                             /(xeleff(nt,nv)*xSolarIns*365.25/10.8*xSqftperKW*XLOSSFAC(NT,NV))    !PVgen
                 xSizeMax=xkwmax(nt) !set absolute maximum size                      !economies of scale for DG
                 xSizeMin=xkwmin(nt) !set absolute minimum size                      !economies of scale for DG
                 xcalcKW=float(ifix(max(min(xSizefromRoofArea,xSizefromAnnualKWH,xSizeMax),xSizeMin)))
                 xadjcost2=exp(log(xadjcost)-xscalefac(nt)*log(xcalckw/xkw(nt,nv)))  !economies of scale for DG
               else  
                 xcalcKW=xkw(nt,nv)  !set size to menu capacity
                 xadjcost2=xadjcost  !learning for DG 
              endif

              XANNUALKWH=XELEFF(NT,NV)*xSolarIns*365.25/10.8*xSqftperKW*xCalcKW*XLOSSFAC(NT,NV)   !PVgen

              ! Compute annual Solar generation and compare to building use, value own-use at the retail price 
              !  and grid sales at the grid price
              XEXCESSKWH=XANNUALKWH-XELECAVGKWH(IBLD,idiv,iSize)
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASE=XANNUALKWH*xRetailElecPr                            
              ELSE
                XVALESAVEBASE= XEXCESSKWH*xSalestoGridPR+XELECAVGKWH(IBLD,idiv,iSize)*xRetailElecPR
              ENDIF
              ! Recompute value of energy savings with RPS credit of 1 for years where credit can switch (ala Markey 2009 Bill Provisions)
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEadjRPS=XANNUALKWH*xRetailElecPradjRPS                  
              ELSE
                XVALESAVEBASEadjRPS=XEXCESSKWH*xSalestoGridPRadjRPS+XELECAVGKWH(IBLD,idiv,iSize)*xRetailElecPRadjRPS  
              ENDIF
              ! Recompute value of energy savings without RPS credit for potential phase outs
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEnoRPS=XANNUALKWH*xRetailElecPrnoRPS                  
              ELSE
                XVALESAVEBASEnoRPS=XEXCESSKWH*xSalestoGridPRnoRPS+XELECAVGKWH(IBLD,idiv,iSize)*xRetailElecPRnoRPS  
              ENDIF

             ! Zero out variables not relevant to Solar
             XBASEYRFUELCOST=0.
             XFUELINPUT=0.
             XWATERHTGMMBTU=0.
             XspaceHTGMMBTU=0.
             XBTUWASTEHEAT=0.
             XfuelPRICE=0.
             ! End Setup for Solar Calculations
             GOTO 200 ! Jump to Cash Flow Model

! Wind Calculations
 100          Continue
              !----------------------------------- 
              ! Setup for Wind Calculations Begin
              !-----------------------------------
              ! Wind generation is valued at the average commercial electricity price.
              ! Convert to same year dollars as generation capital costs and then use 
              !  inflation to maintain nominal dollars.
              ! These include any RPS credit (passed from NEMS in mills per kWh)
              xRetailElecPR=( PELCM(IDIV,ICURIYR)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                     +.001*EPRPSPR(ICURIYR)*xRPS(nt,icuriyr) )                                   & 
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)  
              ! For grid sales add RPS credits for this technology 
              xSalestoGridPR=( PELME(IDIV,ICURIYR)*.003412+.001*EPRPSPR(ICURIYR)*xRPS(nt,icuriyr) )  &
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)
              ! For the analysis of Markey-type RPS credits which adjust after some number of years, revert credit to 1.0                     
              xSalestoGridPRadjRPS=( PELME(IDIV,ICURIYR)*.003412+.001*EPRPSPR(ICURIYR)*1.0 )  &
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)
              xRetailElecPRadjRPS=( PELCMOUT(IDIV,ICURIYR,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                     +.001*EPRPSPR(ICURIYR)*1.0 )                                           & 
                     *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2) 

              !Just in Case RPS Credits come through in years before they should be credited to commercial:
              If (CALYEAR .lt. iRPSStartYear(NT)) then
                xRetailElecPR=xRetailElecPRnoRPS
                xSalestoGridPR=xSalestoGridPRnoRPS
              Endif

              ! Account for total floorspace for potential wind penetration
              WindAvailFS(ICuriyr,IDIV,IBLD)= WindAvailFS(ICuriyr,IDIV,IBLD) + &
               (SurvFloorTotal(IDIV,IBLD,ICURIYR)+CMNewFloorSpace(IDIV,IBLD,ICURIYR)) &
               * cbecs12FlspcCatShare(IBLD,iDiv,iSize)*xSqftShare(iDiv,iNiche,iRateLevel)  &
               * xpctWindSuitable

              ! CALCULATION OF KWH SUPPLIED FOR WIND
              !  Wind speed is in m/s 
              !  xeleff represents relative efficiency of future technologies relative to today's models
              !  xMpS is the wind speed in meters per second and capacity factor is a cubic function of 
              !  wind speed (.0645 -0.0670*xMpS +.0210*xMpS**2 -.0011*xMpS**3).
              xMpS=xWindSpeed(iDiv,iNiche,iRateLevel)

              ! Optimize capacity for up to 100% of kWh requirements
              if (CALYEAR > 2000) then
                 xSizefromAnnualKWH=XELECAVGKWH(IBLD,idiv,iSize) &
                             /( (xeleff(nt,nv)/xeleff(nt,1))*(.0645 -0.0670*xMpS +.0210*xMpS**2 -.0011*xMpS**3)*8760.*XLOSSFAC(NT,NV) )
                 xSizeMax=xkwmax(nt) !set absolute maximum size                      !economies of scale for DG
                 xSizeMin=xkwmin(nt) !set absolute minimum size                      !economies of scale for DG
                 xcalcKW=float(ifix(max(min(xSizefromAnnualKWH,xSizeMax),xSizeMin)))
               else  
                 xcalcKW=xkw(nt,nv)  !set size to menu capacity
              endif

              XANNUALKWH=XELEFF(NT,NV)/xeleff(nt,1)* &
               (.0645 -0.0670*xMpS +.0210*xMpS**2 -.0011*xMpS**3)*xcalcKW*8760.*XLOSSFAC(NT,NV)  

              ! Compute annual Wind generation and compare to buliding use, value own-use at the retail price 
              !  and grid sales at the grid price
              XEXCESSKWH=XANNUALKWH-XELECAVGKWH(IBLD,idiv,iSize)
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASE=XANNUALKWH*xRetailElecPr                            
              ELSE
                XVALESAVEBASE= XEXCESSKWH*xSalestoGridPR+XELECAVGKWH(IBLD,idiv,iSize)*xRetailElecPR
              ENDIF
              ! Recompute value of energy savings with RPS credit of 1 for years where credit can switch (ala Markey 2009 Bill Provisions)
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEadjRPS=XANNUALKWH*xRetailElecPradjRPS                  
              ELSE
                XVALESAVEBASEadjRPS=XEXCESSKWH*xSalestoGridPRadjRPS+XELECAVGKWH(IBLD,idiv,iSize)*xRetailElecPRadjRPS  
              ENDIF
              ! Recompute value of energy savings without RPS credit for potential phase outs
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEnoRPS=XANNUALKWH*xRetailElecPrnoRPS                  
              ELSE
                XVALESAVEBASEnoRPS=XEXCESSKWH*xSalestoGridPRnoRPS+XELECAVGKWH(IBLD,idiv,iSize)*xRetailElecPRnoRPS  
              ENDIF

             ! Zero out variables not relevant to Wind
             XBASEYRFUELCOST=0.
             XFUELINPUT=0.
             XWATERHTGMMBTU=0.
             XspaceHTGMMBTU=0.
             XBTUWASTEHEAT=0.
             XfuelPRICE=0.
             xadjcost2=xadjcost
             ! End Setup for Wind Calculations
             GOTO 200 ! Jump to Cash Flow Model

!Fuel-Fired DG
 150        Continue   
            !  Set up for Fuel-Fired Distributed Generation Technologies
            !    COMPUTE ANNUAL FUEL COST -- NET OF IMPUTED WATERHEATING COSTS
            !     FUEL PRICES CONVERTED TO DOLLAR YEARS IN KGENTK FOR CONSISTENCY
            IF(IFUELTYPE(NT).EQ.2)XfuelPRICE=PNGCM(IDIV,ICURIYR) &
               *MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)*xNGRateScalar(iDiv,iNiche,iRateLevel)
             IF(IFUELTYPE(NT).EQ.3)XfuelPRICE=PDSCM(IDIV,ICURIYR)*MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)
             IF(IFUELTYPE(NT).EQ.6)XfuelPRICE=PCLCM(IDIV,ICURIYR)*MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)
            !  ASSIGN THE COAL PRICE TO MSW and Wood/Biomass -- zero for Hydro
             IF(IFUELTYPE(NT).EQ.10)XfuelPRICE=PCLCM(IDIV,ICURIYR)*MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)
             IF(IFUELTYPE(NT).EQ.11)XfuelPRICE=0.0
             IF(IFUELTYPE(NT).EQ.9)XfuelPRICE=PCLCM(IDIV,ICURIYR)*MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)

            !    COMPUTE ANNUAL KWH GENERATION
            !    AVERAGE EUIS FOR GAS WATER AND SPACE HEATING FROM CBECS
            !    USE GAS EUIS -- FUEL INDEX = 2
            !    WATER HEATING INDEX, S=3, Space heating index, s=1
            !    COMEUI(R,B,S,F)
            XWATERHTGMMBTU=cbecs12AVGSQFT(ibld,iDiv,isize)/1000.*COMEUI(IDIV,IBLD,3,2)    !Loop cbecs12 - adjust for avg FS units change
            XSPACEHTGMMBTU=cbecs12AVGSQFT(ibld,iDiv,isize)/1000.*COMEUI(IDIV,IBLD,1,2)    !Loop cbecs12
            if(lprint2) WRITE(rcrpt,*)'WATER&SPACE HTG MMBTU ', &
              BLDGNAME(IBLD),XWATERHTGMMBTU,XSPACEHTGMMBTU  
            ! Optimize capacity for up to 60% of kWh requirements, or 100% of heat requirements with global max and min
            if (CALYEAR > CmCogHstYr+1989) then
               xSizefromHeatBTU= ( XELEFF(NT,NV)/.003412*(xwaterhtgmmbtu + xspacehtgmmbtu)*1.00/XWHRECOVERY(NT,NV) ) / &
                     ( XOPERHOURS(NT)*XAVAIL(NT,NV)*(1.-XELEFF(NT,NV)*XLOSSFAC(NT,NV)) )
               xSizefromAnnualKWH=XELECAVGKWH(IBLD,idiv,iSize)*.6/(XOPERHOURS(NT)*XAVAIL(NT,NV)*XLOSSFAC(NT,NV))
               xSizeMax=xkwmax(nt)  !set absolute maximum size                       !economies of scale for DG
               xSizeMin=xkwmin(nt)  !set absolute minimum size                       !economies of scale for DG
               xcalcKW=float(ifix(min(max(xSizefromHeatBTU,xSizefromAnnualKWH,xSizeMin),xSizeMax)))     
               xadjcost2=exp(log(xadjcost)-xscalefac(nt)*log(xcalckw/xkw(nt,nv)))    !economies of scale for DG
             else  
               xcalcKW=xkw(nt,nv)  !set size to menu capacity
               xadjcost2=xadjcost                                                    !economies of scale for DG
            endif

            XANNUALKWH=XOPERHOURS(NT)*XAVAIL(NT,NV)*xcalckw*XLOSSFAC(NT,NV)

            !    COMPUTE FUEL INPUT IN MMBTU (Million Btu)

            xfuelinput=.003412 * xcalckw* XOPERHOURS(NT) * XAVAIL(NT,NV) / XELEFF(NT,NV)

            XBTUWASTEHEAT= (xfuelinput-.003412 * XANNUALKWH)* XWHRECOVERY(NT,NV)

            IF(XBTUWASTEHEAT .gt. XWATERHTGMMBTU)  Then
                !  provides all water heating and some space heating
                !        -- determine how much space heating can be provided
                xtemp=xwaterhtgmmbtu+xspacehtgmmbtu
                if(xbtuwasteheat .lt. xtemp) xspacehtgmmbtu=xbtuwasteheat-xwaterhtgmmbtu
             else
                !  provides only a portion of water heating
                XWATERHTGMMBTU=XBTUWASTEHEAT
                xspacehtgmmbtu=0.
            endif

           !   add space heating credit against fuel costs
           XBASEYRFUELCOST = (xfuelinput-XWATERHTGMMBTU-xspacehtgmmbtu) * XfuelPRICE
           XEXCESSKWH=XANNUALKWH-XELECAVGKWH(IBLD,iDiv,iSize)
           IF(XEXCESSKWH.LT.0.) THEN
              XVALESAVEBASE=XANNUALKWH*xRetailElecPrnoRPS 
            ELSE
              XVALESAVEBASE= XEXCESSKWH*xSalestoGridPRnoRPS+XELECAVGKWH(IBLD,iDiv,iSize)*xRetailElecPRnoRPS 
           ENDIF

             ! store energy savings without RPS credit and recompute with RPS credit for NG-fired techs   ceschp
             ! general formula for credit is generation * carbon intensity minus CES target percentage * own-use gen

           IF(IFUELTYPE(nt).eq.2) THEN                                          !ceschp
                XVALESAVEBASEnoRPS=XVALESAVEBASE                                !ceschp

            IF(XEXCESSKWH.LT.0.) THEN                                           !ceschp
               XVALESAVEBASE=XVALESAVEBASEnoRPS + .001*EPRPSPR(ICURIYR)&        !ceschp - add credit price from uefdout include,
               * xRPS(nt,icuriyr) *(( XANNUALKWH * (1.0 -      &   !ceschp - times credit multiplier * difference of (generation times difference between 1.0 and
               ((53.06/0.82)*(xfuelinput/XANNUALKWH))) -       &   !ceschp - carbon intensity for this tech) => intensity here in kg/kWh => 53.06 kg CO2/mmBtu NG
                (EPRPSTGT(ICURIYR) * XANNUALKWH)))                 !ceschp - and (clean energy target percentage times generation for own use)

              ! Recompute value of energy savings with RPS credit of 1 for years where credit can switch (ala Markey 2009 Bill Provisions)
               XVALESAVEBASEadjRPS=XVALESAVEBASEnoRPS+.001*EPRPSPR(ICURIYR)&     !ceschp - add credit price from uefdout include,
               * 1.0 *(( XANNUALKWH * (1.0 -                   &   !ceschp - times credit of 1.0 * difference of (generation times difference between 1.0 and
               ((53.06/0.82)*(xfuelinput/XANNUALKWH))) -       &   !ceschp - carbon intensity for this tech) => intensity here in kg/kWh => 53.06 kg CO2/mmBtu NG
                (EPRPSTGT(ICURIYR) * XANNUALKWH)))                 !ceschp - and (clean energy target percentage times generation for own use)
            ELSE
               XVALESAVEBASE=XVALESAVEBASEnoRPS + .001*EPRPSPR(ICURIYR)&         !ceschp - add credit price from uefdout include,
               * xRPS(nt,icuriyr) *(( XANNUALKWH * (1.0 -      &    !ceschp - times credit multiplier * difference of (generation times difference between 1.0 and
               ((53.06/0.82)*(xfuelinput/XANNUALKWH))) -       &    !ceschp - carbon intensity for this tech) => intensity here in kg/kWh => 53.06 kg CO2/mmBtu NG
                (EPRPSTGT(ICURIYR) * (XANNUALKWH - XEXCESSKWH))))   !ceschp - and (clean energy target percentage times generation for own use)

              ! Recompute value of energy savings with RPS credit of 1 for years where credit can switch (ala Markey 2009 Bill Provisions)
               XVALESAVEBASEadjRPS=XVALESAVEBASEnoRPS+.001*EPRPSPR(ICURIYR)&     !ceschp - add credit price from uefdout include,
               * 1.0 *(( XANNUALKWH * (1.0 -                   &    !ceschp - times credit of 1.0 * difference of (generation times difference between 1.0 and
               ((53.06/0.82)*(xfuelinput/XANNUALKWH))) -       &    !ceschp - carbon intensity for this tech) => intensity here in kg/kWh => 53.06 kg CO2/mmBtu NG
                (EPRPSTGT(ICURIYR) * (XANNUALKWH - XEXCESSKWH))))   !ceschp - and (clean energy target percentage times generation for own use)

            ENDIF ! check for grid sales  ceschp

            IF (XVALESAVEBASE .lt. XVALESAVEBASEnoRPS) XVALESAVEBASE = XVALESAVEBASEnoRPS          !ceschp - check for negative credits
            IF (XVALESAVEBASEadjRPS .lt. XVALESAVEBASEnoRPS) XVALESAVEBASE = XVALESAVEBASEnoRPS    !ceschp - check for negative credits

           ENDIF ! check for NG-fired tech to account for clean energy credits in energy savings    ceschp

200    CONTINUE
       ! Several of the technologies are included only to tie to the EIA 860B data, 
       !  Zero our IRR for these techs to prevent further penetration and skip most calcs, 
       !  then jump over cash flow and continue to account for historical capacity, etc...
       xtemp=0.
       if (icuriyr.le.CMCogHstYr .and. nt.ne.1 .and. nt.ne.2 .and. nt.ne.8 .and. nt.ne.11) goto 350
       if (icuriyr.gt.CMCogHstYr .and. iskip(NT).eq.0) goto 350  !also skip over technologies included for 860B, but not modeled for further penetration

!-----------------------------------------
! CALCULATE CASH FLOWS BY TECHNOLOGY TYPE
!-----------------------------------------
!       ASSUMES EQUIPMENT PURCHASE AND LOAN ORIGINATION IN YEAR 1,  
!       INSTALLATION COMPLETE IN YEAR 2,
!       TAX RETURN FOR YEAR 2 IN YEAR 3.
!   CALCULATE ANNUAL LEVELIZED PAYMENT
            xcalceqcost = (xadjcost2+XINSTCOST(NT,NV))*xcalckw
            XMAINTCOSTBASE=XMAINTCST(NT,NV)*xcalckw
            XTAXCREDITPCT=XTXCRPCT(NT,NV)

            XTAXCREDITMAX=xTXCrMaxPerSys(NT,NV)
            XLIFE=XEQLIFE(NT,NV)
            XDEGRADATION=XDEGRAD(NT,NV)
            XTAXLIFE=XTXLIFE(NT,NV)
            XDOWNPAY=XDOWNPAYPCT*xcalceqcost
            XPAYMENT=XINTRATE/(1.-(1.+XINTRATE)**(-1.*XTERM)) &
               *(xcalceqcost-XDOWNPAY)            
            ! first reset the tax credit percentage to potentially something less if the max credit per kw is capped
            if(xtaxcreditpct>0. .and. xtaxcreditmaxkw>0.) then
              xtaxcreditpct=min(xtaxcreditpct,xtaxcreditmaxkw/xcalceqcost)
            endif
            XTOTTAXCREDIT=xcalceqcost*XTAXCREDITPCT
            if(xtaxcreditmax .gt. 0.)XTOTTAXCREDIT=min(xtottaxcredit,XTAXCREDITMAX)    
            XTOTTAXCREDIT=XTOTTAXCREDIT+xCalcEqCost*comtxcrpct_div(nv,idiv,nt)                     ! 111dren			
            basis = xcalceqcost - XTOTTAXCREDIT*.5
            iIntervalYrstoUse=iIntervalYrs(NT,NV)
 
! Initialize Cash Flow Starting Values         
            XCUMNETFLOW(1:30)=0.
            XTAXCREDIT(1:30)=0. 
            XOUTLAY(1:30)=0.
            XVALESAVE(1:30)=0.
            XKWH(1:30)=0.
            XPRIN(1:30)=0.0
            XDEPR(1:30)=0.
            XMAINTCOST(1:30)=0.
            XINTAMT(1:30)= 0. 
            XFUELCOST(1:30)=0.

! Now Set the Year 1 Variables
            XOUTLAY(1)=XDOWNPAYPCT*xcalceqcost
            XLOANBAL(1)=xcalceqcost*(1.-XDOWNPAYPCT)
            XNETCASHFLOW(1)=-XOUTLAY(1)
            XCUMNETFLOW(1)=-XOUTLAY(1)
            iTaxCreditCarryOver=1
            xtaxcreditclaimedtodate=0.

 ! Loop Years 2 to 30 to Construct 30-year Cash Flows
            DO IYR=2,30

            ! Taxcreditc/o:
            ! Check for usable carryover tax credits for the standard commercial investment tax credit 
            if(iTaxCreditCarryOver.eq.1 .and. iyr.ge.3) then
                 !Investment Tax Credit limited to 25k per year, and can carry over.
                  xtaxcredit(iyr) = min(XTOTTAXCREDIT - xtaxcreditclaimedtodate,25000.)
                  xtaxcredit(iyr) = max(xtaxcredit(iyr),0.) 
                  xtaxcreditclaimedtodate=xtaxcreditclaimedtodate+xtaxcredit(iyr)
                  iTaxCreditCarryOver=0
                  !check to see if further tax carryover is appropriate
                  if (XTOTTAXCREDIT > xtaxcreditclaimedtodate) iTaxCreditCarryOver=1
            endif

            ! Calculate Principal, Interest and Loan Balance
            IF(FLOAT(IYR).LE.XTERM+1.)XOUTLAY(IYR)=XPAYMENT
            XINTAMT(IYR)=XLOANBAL(IYR-1)*XINTRATE
            IF(FLOAT(IYR).LE.XTERM+1.)XPRIN(IYR)=XPAYMENT-XINTAMT(IYR)
            XLOANBAL(IYR)=XLOANBAL(IYR-1)-XPRIN(IYR)

            ! ACCELDEPR:
            ! Compute accelerated depreciation and update basis
            IF (FLOAT(IYR).LE.(XTAXLIFE+1.) ) THEN                                  
                  XDEPR(IYR)= BASIS * XDEP(NT,NV)/100. / XTAXLIFE                  !DECLINING BALANCE AMOUNT
                       IF ( XDEPR(IYR).LE.(BASIS / (XTAXLIFE-IYR+2.)) ) THEN       !STRAIGHT LINE AMOUNT
                        XDEPR(IYR)= BASIS /(XTAXLIFE-IYR+2.)                       !STRAIGHT LINE AMOUNT
                       END IF  
                  BASIS = BASIS - XDEPR(IYR)
            ENDIF 

            ! Calculate nominal dollar valued fuel costs 
            IF(FLOAT(IYR).LE.(XLIFE+1.)) &
                     XFUELCOST(IYR)=XBASEYRFUELCOST*(1.+XINFLATION)**(IYR-2)

            !Inverters:
            ! Calculate both annual as well as discrete maintenance costs.  Initially designed to accomodate 
            !   discrete  Solar inverter replacements, and is now used for wind.
            IF(iyr.gt.2 .and. IMod(Iyr-2,iIntervalYrstoUse).eq.0 .and. Iyr.ne.29 .and. Iyr.ne.30) then                
                IF(FLOAT(IYR).LE.(XLIFE+1.)) &
                   XMAINTCOST(IYR)=(XMAINTCOSTBASE+xIntervalCst(NT,NV)* xCalcKW)*(1.+XINFLATION)**(IYR-2)   
                !Adjust the interval years for the next (if needed) discrete replacement
                !  The use of IYR reflects "average" progress in extending inverter lives
                iIntervalYrstoUse=2*iIntervalYrstoUse+IYR        
                ! diagnostic only -- if (lprint3) write(rcrpt,*) 'interval adjustment***  orig ', iyr, 'new ',iintervalyrstouse    
             Else                                                                                       
                IF(FLOAT(IYR).LE.(XLIFE+1.)) XMAINTCOST(IYR)=XMAINTCOSTBASE*(1.+XINFLATION)**(IYR-2)                             
            ENDIF

            ! Compute nominal dollar valued energy savings and apply degradation (if nonzero in kgentk) to the amount.                                                                           
            IF(FLOAT(IYR).LE.(XLIFE+1.)) then
               XVALESAVE(IYR)=XVALESAVEBASE * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)
            ! Overwrite for RPS Credit Adjustments and Sunset the RPS in applicable
             IF(nt.eq.1 .or. nt.eq.11 .or. ifueltype(nt).eq.2) then     ! add NG-fired chp ceschp
               !Sunset
               IF(CALYEAR+iyr-1 .gt. iRPSPhaseOutYear(NT)) &
                   XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)
               !Adjust  
               IF(CALYEAR .ge. iRPSStartYear(NT) .and. iyr .gt. iNumYrsatRPSBaseRate(nt,icuriyr)) &
                 XVALESAVE(IYR)=XVALESAVEBASEadjRPS * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)
               !Remove Credits if Num Credit Years < 30  
               If(CALYEAR .ge. iRPSStartYear(NT) .and. iyr .gt. iNumRPSCreditYrs(nt,icuriyr)) &
                 XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)                
               !Remove Credits if CALYear is before start of RPS credits 
               If(CALYEAR .lt. iRPSStartYear(NT) ) &
                 XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)                
             ENDIF ! nt=1 or nt=11, PV or Wind -or NG-fired CHP ceschp
            ENDIF

           ! Refine tax accounting: 
           ! The tax deductibles over the baseline scneario are reduced by the saved costs since they would 
           !   have otherwise been deductible expenses.
           XTAXDEDUCT(IYR)=XTAXRATE*(XINTAMT(iyr-1)+XMAINTCOST(IYR-1) &
              + XDEPR(IYR-1)+(xfuelcost(iyr-1) - xvalesave(IYR-1)))+ XTAXCREDIT(IYR)

           ! Compute annual generation in kWh and apply degregadation (if nonzero in kgentk).     
           IF(FLOAT(IYR).LE.(XLIFE+1.)) XKWH(IYR)=XANNUALKWH * (1.-XDEGRADATION)**(IYR-2)

           ! Compute net cash flow for the year as well as cumulative for the project
           XNETCASHFLOW(IYR)=-XOUTLAY(IYR)-XFUELCOST(IYR)-XMAINTCOST(IYR)+XTAXDEDUCT(IYR)+XVALESAVE(IYR)
           XCUMNETFLOW(IYR)=XCUMNETFLOW(IYR-1)+XNETCASHFLOW(IYR)

           END DO  ! Year Loop for 30-year Cash Flows

!-------------
! Compute IRR
!-------------
!    compute NPV at starting points for interpolation
        xIRR=0.
        i=0
        xDisc_L=.0001
        XNPV_L=XNPV(xNetCashFlow,xDisc_L,30,rcrpt)
        If(XNPV_L.le.0) then
        !  IF(lprint3) WRITE(rcrpt,*) 'IRR is Zero!'
          Goto 300 ! Penetration funciton calculation
        Endif

        xDisc_U=.3
        XNPV_U=XNPV(xNetCashFlow,xDisc_U,30,rcrpt)
           DO i=1,100
            xIRR_prev=xIRR
            !Generate interpolated guess of next point
            xIRR=xDisc_L-XNPV_L*(xDisc_U-xDisc_L)/(XNPV_U-XNPV_L)
            XNPV_new=XNPV(xNetCashFlow,xIRR,30,rcrpt)
!  debug if (lprint3) WRITE(rcrpt,291) XNPV_l, xdisc_l, XNPV_u, xdisc_u, XNPV_new, xirr
            !Test for doneness
            If (abs(XNPV_new)<1.0) goto 300         !Calculate penetration
            If (abs(xIRR_prev-xIRR)<.0005) goto 300 !Calculate penetration
            IF (XNPV_new.gt.0.) THEN
              XNPV_L=XNPV_new
              xDisc_L=xIRR
             ELSE
              XNPV_U=XNPV_new
              xDisc_U=xIRR
            ENDIF
           ENDDO
           IF(lprint3) WRITE(rcrpt,*) '***IRR Did Not Converge!  IRR= ',xirr, 'NPV= ', XNPV_new

 300   CONTINUE  !This is the statment to jump to to begin penetration calculations
!------------------------------------
! CALCULATE PENETRATION BASED ON IRR
!------------------------------------
      !Convert IRR to "Payback" Years
      if (xIRR.gt.0.) then 
        xsimplepayback=min(29.,log(2.)/log(1.+xIRR))
       else
        xsimplepayback=29.
      endif
      XMAXPEN=XPENPARM(nt)/XSIMPLEPAYBACK

      ! switchable printing of cash flow results 
      IF(lprint3.AND.IBLD.EQ.7.AND.IDIV.EQ.9.and.isize==2) then      
          WRITE(rcrpt,307) 
          WRITE(rcrpt,308) XELECAVGKWH(IBLD,idiv,iSize), Xfuelinput, XWATERHTGMMBTU, xspacehtgmmbtu, XBTUWASTEHEAT, xfuelprice, xRetailElecPR, xSalestoGridPR
          WRITE(rcrpt,309) 
          DO IYR=1,30
            WRITE(rcrpt,310)IYR-1,XOUTLAY(IYR),XDEPR(IYR),xtaxcredit(iyr), XTAXDEDUCT(IYR),XFUELCOST(IYR), &
             XMAINTCOST(IYR), XVALESAVE(IYR),XNETCASHFLOW(IYR),XCUMNETFLOW(IYR),XKWH(IYR)
          END DO
          WRITE(rcrpt,*) 'IRR= ',xirr, 'Payback= ',xsimplepayback, 'Iters= ', i
      Endif 
 307  Format(1x,'AvgkWhUsage Fuel Input(mmBtu) WH(mmBtu) SH(mmBtu) WasteHt(mmBtu) DGFuelCost($real/mmBtu) RetailElecPr(real$/kWh) GridSalesPR(real$/kWh)')
 308  FORMAT(1X,5F14.0,F10.3,2F12.5)
 309  Format(1x,'YEAR    OUTLAY    XDEPR TAXCREDIT TAXDEDUCT  FUELCOST MAINTCOST     ESAVE NETCASHFL       CUM       KWH ')
 310  FORMAT(1X,I4,10F10.0)


        IF(ICURIYR+1989.eq.XINXFY) THEN                                            !Inxlimit
         XINXDECAY(IDIV,ICURIYR)=XINX(IDIV)                                        !Inxlimit
        ELSE 
          XINXDECAY(IDIV,ICURIYR)=MIN(1.,(XINX(IDIV)+((1.-XINX(IDIV))*(FLOAT((ICURIYR+1989)-XINXFY)/FLOAT(XINXLY-XINXFY)))))   !Inxlimit
        END IF
        IF(ICURIYR+1989.lt.XINXFY) XINXDECAY(IDIV,ICURIYR)=1.
!        if (nt.eq.1 .and. idiv.eq.1) Write(rcrpt,*) "inxlimit", XINXDECAY(IDIV,ICURIYR)

!  Projected Penetration
      XValue=float(icuriyr-CMCogHstYr)
      if(XValue.gt.29.0) XValue=29.0
     ! XPEN=min(.75,XMAXPEN-XMAXPEN/(1.+XMAXPEN*EXP(XALPHA(nt)*(FLOAT(ICURIYR-CMCogHstYr)-XSIMPLEPAYBACK))))
       XPEN=min(.75,XMAXPEN-XMAXPEN/(1.+XMAXPEN*EXP(XALPHA(nt)*(XValue-XSIMPLEPAYBACK))))*XINXDECAY(IDIV,ICURIYR)           !Inxlimit
 
! Turn off endogenous builds until ksteoyr listed in comvars include
       If(NT.eq.1 .and. icuriyr.lt.ksteoyr) xpen=0.0

!  Compute penetration rate assumption to apply to existing ("surviving") floorspace
!   Penetration into existing is based on penetration into new construction with an assumed upper bound
      xExistingPen=min(xPen/10.0,0.005)                                         ! increase existing pen AEO2013
! Note CMNewFloorSpace & SurviveFloorTotal are in 10**6 sqft so must convert average size buildings too...
!  XTEMP represents total added DG units                                                                 ! PENETRATION IN:
      XTEMP = XPEN * CMNewFloorSpace(IDIV,IBLD,ICURIYR)  / (cbecs12AVGSQFT(ibld,iDiv,isize)/10.**6)    & ! NEW CONSTRUCTION  
        + XBLDGSHARE(NT,IBLD) * (XEXOGPEN(ICURIYR,IDIV,NT)-XEXOGPEN(ICURIYR-1,IDIV,NT))/xCalcKW        & ! ADDED EXOGENOUS UNITS  !convert to kw
        + xExistingPen * max(SurvFloorTotal(IDIV,IBLD,ICURIYR)-xCumPenSqft(ICURIYR-1,IDIV,IBLD,NT),0.) &
            / (cbecs12AVGSQFT(ibld,iDiv,isize)/10.**6)                                                   ! EXISTING CONSTRUCTION
      xtemp = xtemp * cbecs12FlspcCatShare(IBLD,iDiv,iSize) * xSqftShare(iDiv,iNiche,iRateLevel) *XINXDECAY(IDIV,ICURIYR)    ! Scale down to niche as a share of total !Inxlimit
      xtemp = float(ifix(100.*xtemp+.5))/100.                                                            ! Eliminate fractional units < .01 (allow fractions for historical calibration)
      IF(lprint2) WRITE(rcrpt,311) CALYEAR,xIRR,XPEN,xtemp 
 311  format(" Year",I5,"           IRR ",F6.3,"       ProjectedPen ",F6.3,"         No. of Units ",f6.1)

!--------------END OF PENETRATION CALCULATIONS 
      ! Now that new units are determined, calculate associated estimates of generation in trills, capacity,
      !   own use generation, fuel usage, offsets to energy consumption for hot water & space heating
      !   and investment.
      !
 350    continue
      ! The following block of code resets xtemp to allow no additional penetration for years with 
      !   historical cogen data for the traditional techs or for techs fueled by non-forecast fuels (e.g., coal)
      !
        IF(ICURIYR.LE.CMCogHstYr .and. (nt.ne.1 .and. nt.ne.2 .and. nt.ne.8 .and. nt.ne.11)) then
             xtemp = (units(icuriyr,idiv,ibld,nt) - units(icuriyr-1,idiv,ibld,nt))       &
             *cbecs12FlspcCatShare(IBLD,iDiv,iSize)*xSqftShare(iDiv,iNiche,iRateLevel)    
        Endif

!Debug or Test to Krpt     If (NT == 1 .and. IDIV == 1 .and. IBLD==1) WRITE (RCRPT,*) 'Size Calcs ', calyear, iratelevel, iniche, isize, 'units ',xtemp, 'avg size ',xcalckw

        xunits = xunits + xtemp
        xtrills = xtrills + XTEMP*XANNUALKWH*3412./10.**12
        xcapacity = xcapacity + xtemp*xCalcKW  
        IF(XANNUALKWH.GT.XELECAVGKWH(IBLD,idiv,iSize)) THEN
          xtrillsownuse = xtrillsownuse + XTEMP*XELECAVGKWH(IBLD,idiv,iSize)*3412./10.**12
         ELSE
         !  BUILDING CONSUMES ALL OF ITS OWN GENERATION
          xtrillsownuse = xtrillsownuse + XTEMP*XANNUALKWH*3412./10.**12
        ENDIF
        xfuelusage = xfuelusage + XTEMP*xfuelinput/10.**6  !trills
        xhwbtu = xhwbtu +XTEMP*XWATERHTGMMBTU/10.**6       !trills
        xshbtu = xshbtu + XTEMP*XspaceHTGMMBTU/10.**6      !trills
        xinvest = xinvest + XTEMP*xcalceqcost/10.**6       !$millions

        if(nt.eq.11) WindTechPotentialMW(Icuriyr,iDiv)= WindTechPotentialMW(Icuriyr,iDiv)            &
                + (CMNewFloorSpace(IDIV,IBLD,ICURIYR) + SurvFloorTotal(IDIV,IBLD,ICURIYR))*10.**6    &
                * cbecs12FlspcCatShare(IBLD,iDiv,iSize) / cbecs12AVGSQFT(ibld,iDiv,isize)            &
                * xSqftShare(iDiv,iNiche,iRateLevel) * xpctWindSuitable*(xCalcKW/1000.)

400    continue  !this continue statement is to allow skipping over size categories that do not apply to builing type / division combos

       End Do  !Rate Levels (iRateLevel)
       End Do  !Climate Zone Niches (iNiche)

      END DO   !CBECS Size Categories (iSize)

!-----------------------------
! SET DISTGEN OUTPUTS TO NEMS
!-----------------------------

!      WRITE(rcrpt,*) 'KWH ',XANNUALKWH
!      WRITE(rcrpt,*) 'XTEMP ', XTEMP, 'XPEN ',XPEN
!      WRITE(rcrpt,*) 'EXOG PEN ' , XEXOGPEN(ICURIYR,IDIV,NT)

       UNITS(ICURIYR,IDIV,IBLD,NT)=UNITS(ICURIYR-1,IDIV,IBLD,NT) + xunits

!      Calculate square footage of builings the number of units represents        
       do iSize=1,6
         xCumPenSqft(ICURIYR,IDIV,IBLD,NT)= xCumPenSqft(ICURIYR,IDIV,IBLD,NT) + xunits*cbecs12FlspcCatShare(IBLD,iDiv,iSize)*(cbecs12AVGSQFT(ibld,iDiv,isize)/10.**6)
       enddo 
       xCumPenSqft(ICURIYR,IDIV,IBLD,NT)=xCumPenSqft(ICURIYR,IDIV,IBLD,NT)+xCumPenSqft(ICURIYR-1,IDIV,IBLD,NT)       

       TRILLS(ICURIYR,IDIV,IBLD,NT)=TRILLS(ICURIYR-1,IDIV,IBLD,NT) + xtrills

       capacity(icuriyr,idiv,ibld,nt)=capacity(icuriyr-1,idiv,ibld,nt) + xcapacity  !capacity is computed in kw, convert to mw later

       ! Account for Installed Capacities, Covered Area and Maximum Potentials for PV and Wind
       If(NT.eq.1) then
             !  Note: For flat roofs, roughly double the roof area is needed for latitude mounting.  Assuming
             !    75% of commercial roofs are flat means that available roof area must be deflated by 175% to allow for 
             !    proper spacing (.25*1.0 + .75*2.0) for latitude-tilt modules
               SolarPVInstalledMW(ICuriyr,IDIV)=SolarPVInstalledMW(ICuriyr,IDIV)+xcapacity/1000. 
         SolarPVUsedRoofArea(ICuriyr,IDIV,IBLD)=SolarPVUsedRoofArea(ICuriyr,IDIV,IBLD)+ &
                 (xcapacity*xSqftperKW/10**6)/1.75  !divide by 10**6 to convert to millions of sqft for comparison to CM Floorspace variables
             SolarPVTechPotentialMW(ICuriyr,IDIV)= SolarPVTechPotentialMW(ICuriyr,IDIV)  &
               + (SolarPVAvailRoofArea(ICuriyr,IDIV,IBLD)*10**6/(xSqftperKW*1000.))/1.75 
       Endif
       If(NT.eq.11) then
               WindInstalledMW(ICuriyr,IDIV)=WindInstalledMW(ICuriyr,IDIV)+xcapacity/1000. 
         WindBldgCount(ICuriyr,IDIV,IBLD)=WindBldgCount(ICuriyr,IDIV,IBLD)+ xunits
       Endif

       TrillsOwnUse(ICURIYR,IDIV,IBLD,NT) = TRILLSOWNUSE(ICURIYR-1,IDIV,IBLD,NT) + xtrillsownuse
       FuelUsage(ICURIYR,IDIV,IBLD,NT) = fuelusage(ICURIYR-1,IDIV,IBLD,NT) + xfuelusage

       HWBTU(ICURIYR,IDIV,IBLD,NT)=HWBTU(ICURIYR-1,IDIV,IBLD,NT) + xhwbtu

       SHBTU(ICURIYR,IDIV,IBLD,NT)=SHBTU(ICURIYR-1,IDIV,IBLD,NT) + xshbtu

       !  COMPUTE ANNUAL CAPITAL INVESTMENT COSTS ($millions)
       INVEST(ICURIYR,IDIV,IBLD,NT)=xinvest
	      
	   com111drensub(icuriyr,idiv,nt)=com111drensub(icuriyr,idiv,nt)+xinvest*comtxcrpct_div(nv,idiv,nt)                     ! 111dren			
       IF(ICURIYR.eq.CMFirstYr) Invest(ICURIYR,IDIV,IBLD,NT)=0.

         !Add in previous year installed capacity by division in order to accumulate total to date
         If(NT.eq.1) SolarPVUsedRoofArea(ICuriyr,IDIV,IBLD)=SolarPVUsedRoofArea(ICuriyr,IDIV,IBLD) & 
          + SolarPVUsedRoofArea(ICuriyr-1,IDIV,IBLD)

        END DO  ! Building Types (iBldg)

         !Add in previous year installed capacity by division in order to accumulate total to date
         If(NT.eq.1)SolarPVInstalledMW(ICuriyr,IDIV)=SolarPVInstalledMW(ICuriyr,IDIV)+SolarPVInstalledMW(ICuriyr-1,IDIV)
         If(NT.eq.11)WindInstalledMW(ICuriyr,IDIV)=WindInstalledMW(ICuriyr,IDIV)+WindInstalledMW(ICuriyr-1,IDIV)
         
!-----------------------------------------------------------------------------------------
! More RPS Calcs to Transfer the Composite Bonus Credits to the Electricity Market Module
!-----------------------------------------------------------------------------------------
         If(NT.eq.1) then
         xtrills=0.
         do ibld=1,NUMBLDG
          xtrills=xtrills+TRILLS(ICURIYR,IDIV,IBLD,NT)-TRILLS(ICURIYR-1,IDIV,IBLD,NT)
         enddo
         xPVGenAdded(IDIV,ICuriyr)= xtrills
         xCompCredit=0.
         xCompGen=0.
         ! Accumulate Credits
         xCredit=1.0 !Minimum credit multiplier is 1.
         Do iyr=CMFirstYr,ICuriyr
            iCalYR=iyr+1989 
            xBonus=0.
            xCompGen=xCompGen+xPVGenAdded(IDIV,iyr)
            if(calyear .lt. iRPSStartYear(NT) .and. icalyr .ge. iRPSGrandFatherYear(NT)) then  !no bonus for equipment in service before grandfather year
                xbonus=xRPS(NT,Icuriyr)-1.0  !give the current year credit to renewable DG placed in service before the legislation
            else    
              if(ICuriyr .lt. iyr+iNumYrsatRPSBaseRate(NT,iyr) .and.  calyear .lt. iRPSPhaseOutYear(NT)) then
                    xBonus=xRPS(NT,iyr)-1.0  !Always at least 1.0 from the input file
               else           
                     if(ICuriyr .lt. iyr+iNumRPSCreditYrs(NT,iyr) .and.  calyear .lt. iRPSPhaseOutYear(NT)) &
                     xBonus=xRPS(NT,ICuriyr)-1.0 !Give the current year credit if beyond the number of base credit years (ala Markey)
               endif      
            endif
         xCompCredit=xCompCredit+xPVGenAdded(IDIV,iyr)*(xCredit+xBonus)
!         If (LPrint .and. IDIV < 2) WRITE (RCRPT,*) 'RPS Calcs', iyr, xPVGenAdded(IDIV,iyr), xcredit+xbonus
         Enddo
         If(xCompGen .gt. 0.) CGCPVCOM(IDIV,ICuriyr)=xCompCredit/xCompGen
         If (LPrint .and. idiv < 10) WRITE (RCRPT,'("RPS Calcs PV",3i6,2F12.5)') icuriyr, curcalyr, idiv, CGCPVCOM(IDIV,ICuriyr), EPRPSPR(ICURIYR)
         Endif !nt=1

         If(NT.eq.11) then
         xtrills=0.
         do ibld=1,NUMBLDG
          xtrills=xtrills+TRILLS(ICURIYR,IDIV,IBLD,NT)-TRILLS(ICURIYR-1,IDIV,IBLD,NT)
         enddo
         xWindGenAdded(IDIV,ICuriyr)= xtrills
         xCompCredit=0.
         xCompGen=0.
         ! Accumulate Credits
         xCredit=1.0 !Minimum credit multiplier is 1.
         Do iyr=CMFirstYr,ICuriyr
            iCalYR=iyr+1989         
            xBonus=0.
            xCompGen=xCompGen+xWindGenAdded(IDIV,iyr)
            if(calyear .lt. iRPSStartYear(NT) .and. icalyr .ge. iRPSGrandFatherYear(NT)) then  !no bonus for equipment in service before grandfather year
                xbonus=xRPS(NT,Icuriyr)-1.0  !give the current year credit to renewable DG placed in service before the legislation
            else    
              if(ICuriyr .lt. iyr+iNumYrsatRPSBaseRate(NT,iyr) .and.  calyear .lt. iRPSPhaseOutYear(NT)) then
                     xBonus=xRPS(NT,iyr)-1.0  !Always at least 1.0 from the input file
               else           
                     if(ICuriyr .lt. iyr+iNumRPSCreditYrs(NT,iyr) .and.  calyear .lt. iRPSPhaseOutYear(NT)) &
                     xBonus=xRPS(NT,ICuriyr)-1.0 !Give the current year credit if beyond the number of base credit years (ala Markey)
               endif      
            endif
         xCompCredit=xCompCredit+xWindGenAdded(IDIV,iyr)*(xCredit+xBonus)
!         If (LPrint .and. IDIV < 2) WRITE (RCRPT,*) 'RPS Calcs Wind', iyr, xWindGenAdded(IDIV,iyr), xcredit+xbonus        
         Enddo
         If(xCompGen .gt. 0.) CGCWNCOM(IDIV,ICuriyr)=xCompCredit/xCompGen
         If (LPrint .and. idiv < 10) WRITE (RCRPT,'("RPS Calcs WN",3i6,2F12.5)') icuriyr, curcalyr, idiv, CGCWNCOM(IDIV,ICuriyr), EPRPSPR(ICURIYR)
        Endif !nt=11
        
        END DO  ! Census Divisions (iDiv)

 450     CONTINUE  !This is the cycling statement for filtering out inappropriate vintages

      END DO    ! Technology Vintage (NV)
      END DO    ! Technology Type (NT)

!-------------------------------------
! Begin NEMS Link Calcs and DB Output
!-------------------------------------
          DO IDIV=1,NUMDIV
          DO IBLD=1,NUMBLDG
          DO NT=1,NUMTECHS
          QCMDGSG(ICURIYR,IDIV)=QCMDGSG(ICURIYR,IDIV) &
               +TRILLS(ICURIYR,IDIV,IBLD,NT) &
               -TRILLSOWNUSE(ICURIYR,IDIV,IBLD,NT)
          END DO  ! TECHNOLOGIES
       IF(lprint3) THEN
         WRITE(rcrpt,*)'GridSales Div=',IDIV,QCMDGSG(ICURIYR,IDIV)
       ENDIF
          END DO  ! BUILDING TYPES
          END DO  ! DIVISIONS

!   Cogen Output Variables to NEMS
!     Note: dropping old distinction between planned and uplanned
!           adding an array dimension for sales vs own use

!
!   LOAD Arrays for the Electric Utility Module
!        Calculate Base Year Cogen Capacity by Census Div,
!             Building Type and Fuel

! Learning
! Initialize learning variables
       CFuelCell_MW(icuriyr)=0.
       CPV_MW(icuriyr)=0.
       CMicroTur_MW(icuriyr)=0.
       CWind_MW(icuriyr)=0.

        Do r=1,numdiv

! Map variables provided to the utility model, generation, own use, grid sales, 
!     capacity, nonutility consumption of fuel for congeneration, to NEMS GDS:
!
!   The index f is the cogen fuel numbering scheme for electricity module (not
!     all of the EMM fuels -see cogen common block- are consumed by the commercial sector):
!           f=1,  coal corresponds to nt=4, ifuel=6
!           f=2,  oil corresponds to nt=5, ifuel=3
!           f=3,  natural gas corresponds to nt=2,3,7 and 8, ifuel=2
!           f=4,  hydro corresponds to nt=9, ifuel=11
!           f=5,  geothermal, unused by comm
!           f=6,  MSW corresponds to nt=6, ifuel=10
!           f=7,  biomass corresponds to nt=10, ifuel=9
!           f=8,  solar corresponds to nt=1
!           f=9,  other gas, unused by comm, ifuel=13
!           f=10, other, unused by comm, ifuel=12
!           f=11, wind, nt=11
!           f=12, solar thermal, unused by comm
!           NOTE:  this scheme is NOT currently flexible and
!                  depends upon the mapping of fuels and positions in kcogen

       ! Total Generation by division, year, fuel 
       CGCOMMGEN (r,iCURIYR,1:12,1:2)= 0.0 !grid sales=1, own use=2
       
       ! Fuel Consumption by division, year, fuel
       CGCOMMQ (r,iCURIYR,1:12)= 0.0 !new 

       ! Capacity Additions (by division, year, fuel)
       cgcommcap(r,icuriyr,1:12)=0. ! new

           Do b=1,numbldg

!   Cogen Capacity MW
!   varyxkw
!   Allow DG purchased capacities to vary by year
!   NEMS index relative to 1990 = 1 is year 11                                       
          
           CGCOMmCAP (r,icuriyr,1)=                                 & 
           CGCOMmCAP (r,icuriyr,1) + capacity(icuriyr,r,b,4)/1000.  !coal
                                                                           
           CGCOMmCAP (r,icuriyr,2)=                                 & 
           CGCOMmCAP (r,icuriyr,2) + capacity(icuriyr,r,b,5)/1000.  !oil 
                                                                           
           CGCOMmCAP (r,icuriyr,3)=                                 &
           CGCOMmCAP (r,icuriyr,3) + capacity(icuriyr,r,b,2)/1000.  & !gas fuel cells
                 +  capacity(icuriyr,r,b,3)/1000.                   & !engine gas    
                 +  capacity(icuriyr,r,b,7)/1000.                   & !turbine gas   
                 +  capacity(icuriyr,r,b,8)/1000.                     !microturbine gas

           CGCOMmCAP (r,icuriyr,6)= &
           CGCOMmCAP (r,icuriyr,6) + capacity(icuriyr,r,b,6)/1000.    !conv MSW

           CGCOMmCAP (r,icuriyr,4)= &
           CGCOMmCAP (r,icuriyr,4) + capacity(icuriyr,r,b,9)/1000.    !hydro

           CGCOMmCAP (r,icuriyr,7)= &
           CGCOMmCAP (r,icuriyr,7) + capacity(icuriyr,r,b,10)/1000.   !wood

           CGCOMmCAP (r,icuriyr,8)= &
           CGCOMmCAP (r,icuriyr,8) + capacity(icuriyr,r,b,1)/1000.    !solar pv

           CGCOMmCAP (r,icuriyr,11)= &
           CGCOMmCAP (r,icuriyr,11) + capacity(icuriyr,r,b,11)/1000.  !wind

!------------------------------------
! CALCULATE OUTPUTS TO NEMS AND COMM
!------------------------------------
      !  varyxkw
      !  Learning
      CFuelCell_MW(icuriyr)=CFuelCell_MW(icuriyr)+capacity(icuriyr,r,b,2)/1000. 
      CPV_MW(icuriyr)=CPV_MW(icuriyr)+capacity(icuriyr,r,b,1)/1000.             
      CMicroTur_MW(icuriyr)=CMicroTur_MW(icuriyr)+capacity(icuriyr,r,b,8)/1000. 
      CWind_MW(icuriyr)=CWind_MW(icuriyr)+capacity(icuriyr,r,b,11)/1000.
      ! own use
           CGCOMmGEN(r,icuriyr,1,2)= CGCOMmGEN(r,icuriyr,1,2) +        & ! Coal
             (TrillsOwnUse(icuriyr,r,b,4)) * (1000./3.412) ! GWH/trill

           CGCOMmGEN (r,icuriyr,2,2)= CGCOMmGEN (r,icuriyr,2,2) +      & ! Oil
             (TrillsOwnUse(icuriyr,r,b,5)) * (1000./3.412) ! GWH/trill

           CGCOMmGEN (r,icuriyr,3,2)= CGCOMmGEN (r,icuriyr,3,2) +      & ! Nat. Gas
             (TrillsOwnUse(icuriyr,r,b,2) + TrillsOwnUse(icuriyr,r,b,3) &
              + TrillsOwnUse(icuriyr,r,b,7) + TrillsOwnUse(icuriyr,r,b,8)) &
                                                 * (1000./3.412) ! GWH/trill

           CGCOMmGEN (r,icuriyr,6,2)= CGCOMmGEN(r,icuriyr,6,2) +       & ! MSW
             (TrillsOwnUse(icuriyr,r,b,6)) * (1000./3.412) ! GWH/trill

           CGCOMmGEN (r,icuriyr,4,2)= CGCOMmGEN(r,icuriyr,4,2) +       & ! hydro
             (TrillsOwnUse(icuriyr,r,b,9)) * (1000./3.412) ! GWH/trill

           CGCOMmGEN (r,icuriyr,7,2)= CGCOMmGEN(r,icuriyr,7,2) +       & ! wood
             (TrillsOwnUse(icuriyr,r,b,10)) * (1000./3.412) ! GWH/trill

           CGCOMmGEN (r,icuriyr,8,2)= CGCOMmGEN (r,icuriyr,8,2) +      & ! Solar PV
             TrillsOwnUse(icuriyr,r,b,1)  * (1000./3.412) ! GWH/trill

           CGCOMmGEN (r,icuriyr,9,2)= CGCOMmGEN (r,icuriyr,9,2) +      & ! Other Gaseous Fuels
             CMCogenEl (r,b,13,icuriyr) * 0.02 * (1000./3.412) ! GWH/trill
               !2% own-use generation based on average 2004-2012
               !generation for AEO2014

           CGCOMmGEN (r,icuriyr,10,2)= CGCOMmGEN (r,icuriyr,10,2) +    & ! Other Fuels
             CMCogenEl (r,b,12,icuriyr) * 0.68 * (1000./3.412) ! GWH/trill
               !68% own-use generation based on average 2004-2012
               !generation for AEO2014

           CGCOMmGEN (r,icuriyr,11,2)= CGCOMmGEN (r,icuriyr,11,2) +    & ! Wind
              TrillsOwnUse(icuriyr,r,b,11) * (1000./3.412) ! GWH/trill

! temporarily CGCommGen (,,1) holds the total generation --> convert to grid sales later

         IF (icuriyr.le.CMCogHstYr) THEN
           CGCOMmGEN (r,icuriyr,1,1)= CGCOMmGEN (r,icuriyr,1,1) +      & ! Coal
             CMCogenEl (r,b,6,icuriyr) * (1000./3.412) ! GWH/trill
         ELSE
           CGCOMmGEN (r,icuriyr,1,1)= CGCOMmGEN (r,icuriyr,1,1) +      & ! Coal
             (Trills(icuriyr,r,b,4)) * (1000./3.412) ! GWH/trill
         ENDIF

         IF (icuriyr.le.CMCogHstYr) THEN
           CGCOMmGEN (r,icuriyr,2,1)= CGCOMmGEN (r,icuriyr,2,1) +      & ! Oil
             CMCogenEl (r,b,3,icuriyr) * (1000./3.412) ! GWH/trill
         ELSE
           CGCOMmGEN (r,icuriyr,2,1)= CGCOMmGEN (r,icuriyr,2,1) +      & ! Oil
             (Trills(icuriyr,r,b,5)) * (1000./3.412) ! GWH/trill
         ENDIF

           CGCOMmGEN (r,icuriyr,3,1)= CGCOMmGEN (r,icuriyr,3,1) +      & ! Nat. Gas
             (Trills(icuriyr,r,b,2) + Trills(icuriyr,r,b,3) &
              + Trills(icuriyr,r,b,7) + Trills(icuriyr,r,b,8) ) &
                   * (1000./3.412) ! GWH/trill

         IF (icuriyr.le.CMCogHstYr) THEN
           CGCOMmGEN (r,icuriyr,6,1)= CGCOMmGEN (r,icuriyr,6,1) +      & ! MSW
             CMCogenEl (r,b,10,icuriyr) * (1000./3.412) ! GWH/trill
         ELSE
           CGCOMmGEN (r,icuriyr,6,1)= CGCOMmGEN (r,icuriyr,6,1) +      & ! MSW
             (Trills(icuriyr,r,b,6) ) * (1000./3.412) ! GWH/trill
         ENDIF

         IF (icuriyr.le.CMCogHstYr) THEN
           CGCOMmGEN (r,icuriyr,4,1)= CGCOMmGEN (r,icuriyr,4,1) +      & ! hydro
             CMCogenEl (r,b,11,icuriyr) * (1000./3.412) ! GWH/trill
         ELSE
           CGCOMmGEN (r,icuriyr,4,1)= CGCOMmGEN (r,icuriyr,4,1) +      & ! hydro
             (Trills(icuriyr,r,b,9) ) * (1000./3.412) ! GWH/trill
         ENDIF

         IF (icuriyr.le.CMCogHstYr) THEN
           CGCOMmGEN (r,icuriyr,7,1)= CGCOMmGEN (r,icuriyr,7,1) +      & ! wood
             CMCogenEl (r,b,9,icuriyr) * (1000./3.412) ! GWH/trill
         ELSE
           CGCOMmGEN (r,icuriyr,7,1)= CGCOMmGEN (r,icuriyr,7,1) +      & ! wood
             (Trills(icuriyr,r,b,10) ) * (1000./3.412) ! GWH/trill
         ENDIF

           CGCOMmGEN (r,icuriyr,8,1)= CGCOMmGEN (r,icuriyr,8,1) +      & ! Solar PV
             Trills(icuriyr,r,b,1) * (1000./3.412) ! GWH/trill

         IF (icuriyr.le.CMCogHstYr) THEN
           CGCOMmGEN (r,icuriyr,9,1)= CGCOMmGEN (r,icuriyr,9,1) +      & ! Other Gaseous Fuels
             CMCogenEl (r,b,13,icuriyr) * (1000./3.412) ! GWH/trill
         ELSE
           CGCOMmGEN (r,icuriyr,9,1)= CGCOMmGEN (r,icuriyr,9,1) +      & ! Other Gaseous Fuels
             CMCogenEl (r,b,13,CMCogHstYr) * (1000./3.412) ! GWH/trill
         ENDIF

         IF (icuriyr.le.CMCogHstYr) THEN
           CGCOMmGEN (r,icuriyr,10,1)= CGCOMmGEN (r,icuriyr,10,1) +    & ! Other Fuels
             CMCogenEl (r,b,12,icuriyr) * (1000./3.412) ! GWH/trill
         ELSE
           CGCOMmGEN (r,icuriyr,10,1)= CGCOMmGEN (r,icuriyr,10,1) +    & ! Other Fuels
             CMCogenEl (r,b,12,CMCogHstYr) * (1000./3.412) ! GWH/trill
         ENDIF

           CGCOMmGEN (r,icuriyr,11,1)= CGCOMmGEN (r,icuriyr,11,1) +    & ! Wind
             + Trills(icuriyr,r,b,11) * (1000./3.412) ! GWH/trill


           ! Energy Consumption for Cogen Trills
           CGCommQ (r,icuriyr,1)= CGCommQ(r,icuriyr,1)           & ! coal
             + fuelusage(icuriyr,r,b,4)

           CGCommQ (r,icuriyr,2)= CGCommQ(r,icuriyr,2)           & ! oil
             + fuelusage(icuriyr,r,b,5)

           CGCommQ (r,icuriyr,3)= CGCommQ(r,icuriyr,3)           & ! nat gas
             + fuelusage(icuriyr,r,b,2) &
             + fuelusage(icuriyr,r,b,3) &
             + fuelusage(icuriyr,r,b,7) &
             + fuelusage(icuriyr,r,b,8)

           CGCommQ (r,icuriyr,6)= CGCommQ(r,icuriyr,6)           & ! MSW
             + fuelusage(icuriyr,r,b,6)

           CGCommQ (r,icuriyr,4)= CGCommQ(r,icuriyr,4)           & ! hydro
             + fuelusage(icuriyr,r,b,9)

           CGCommQ (r,icuriyr,7)= CGCommQ(r,icuriyr,7)           & ! wood
             + fuelusage(icuriyr,r,b,10)

           CGCommQ (r,icuriyr,8)= CGCommQ(r,icuriyr,8)           & ! Solar PV
             + Trills(icuriyr,r,b,1) * KFossilHR/3412.             ! report "fuel usage" as generation in trills
                                                                   !  - conversion to fossil fuel equivalent for renewables

           CGCommQ (r,icuriyr,11)= CGCommQ(r,icuriyr,11)         & ! Wind
             + Trills(icuriyr,r,b,11) * KFossilHR/3412.            ! report "fuel usage" as generation in trills
                                                                   !  - conversion to fossil fuel equivalent for renewables

           END DO        ! end b loop for building types

           !remove own-use from total generation to calculate grid sales
           CGCOMmGEN (r,icuriyr,1,1)= CGCOMmGEN (r,icuriyr,1,1) - CGCOMmGEN (r,icuriyr,1,2)
           CGCOMmGEN (r,icuriyr,2,1)= CGCOMmGEN (r,icuriyr,2,1) - CGCOMmGEN (r,icuriyr,2,2)
           CGCOMmGEN (r,icuriyr,3,1)= CGCOMmGEN (r,icuriyr,3,1) - CGCOMmGEN (r,icuriyr,3,2)
           CGCOMmGEN (r,icuriyr,6,1)= CGCOMmGEN (r,icuriyr,6,1) - CGCOMmGEN (r,icuriyr,6,2)
           CGCOMmGEN (r,icuriyr,4,1)= CGCOMmGEN (r,icuriyr,4,1) - CGCOMmGEN (r,icuriyr,4,2)
           CGCOMmGEN (r,icuriyr,7,1)= CGCOMmGEN (r,icuriyr,7,1) - CGCOMmGEN (r,icuriyr,7,2)
           CGCOMmGEN (r,icuriyr,8,1)= CGCOMmGEN (r,icuriyr,8,1) - CGCOMmGEN (r,icuriyr,8,2)
           CGCOMmGEN (r,icuriyr,9,1)= CGCOMmGEN (r,icuriyr,9,1) - CGCOMmGEN (r,icuriyr,9,2)
           CGCOMmGEN (r,icuriyr,10,1)= CGCOMmGEN (r,icuriyr,10,1) - CGCOMmGEN (r,icuriyr,10,2)
           CGCOMmGEN (r,icuriyr,11,1)= CGCOMmGEN (r,icuriyr,11,1) - CGCOMmGEN (r,icuriyr,11,2)
                                               
           ! Calculate clean energy credits to send to EMM, includes all PV, wind generation;
           ! NG-fired chp according to formula =>(generation*(1.0 - carbon intensity/0.82))-(CES target percentage*own use generation) ceschp
          KCHPCESGEN(r,icuriyr) = 0.0                                           !ceschp initialize regional credits 
          IF (calyear.ge.iRPSGrandFatherYear(3)) THEN        !Check for year of installation for NG chp systems that can earn ces credits ceschp

           !ceschp - populate NG gen, fuel use variables for ineligible systems
            xpreCESGenOwnUse = CGCOMmGEN(r,iRPSGrandFatherYear(3)-baseyr,3,2)             !ceschp
            xpreCESGen = xpreCESGenOwnUse+CGCOMmGEN(r,iRPSGrandFatherYear(3)-baseyr,3,1)  !ceschp
            xpreCESQ = CGCOMmQ(r,iRPSGrandFatherYear(3)-baseyr,3)                         !ceschp

           !calculate xgen, xgenownuse, xfuelusage to represent "credit eligible" NG chp
           xgen = CGCOMmGEN(r,icuriyr,3,1)+CGCOMmGEN(r,icuriyr,3,2)-xpreCESGEN  !ceschp - in GWh => million kWh
           xgenownuse = CGCOMmGEN(r,icuriyr,3,2)-xpreCESGenOwnUse               !ceschp - in GWh => million kWh
           xfuelusage = CGCOMmQ(r,icuriyr,3) - xpreCESQ                         !ceschp - in trills

           !calculate credits (in million kWh) for NG-fired CHP based on generation, fuel use by eligible NG chp systems - no RPS multiplier in calc
           IF (xgen .gt. 0.0)                                        &          !ceschp - check for NG generation
              KCHPCESGEN(r,icuriyr) = (xgen * (1.0 - ((53.06/0.82) * &          !ceschp - difference of (gen times difference between 1.0 and carbon 
              (xfuelusage/xgen))) - (EPRPSTGT(icuriyr)*xgenownuse))             !ceschp - intensity (tons per MWh)/0.82) and CES target % times own use gen
           IF (KCHPCESGEN(r,icuriyr) .lt. 0.0)KCHPCESGEN(r,icuriyr)= 0.0        !ceschp - check for negative credits
           !add in PV and wind generation - all generation counts as clean energy
           KCHPCESGEN(r,icuriyr) = KCHPCESGEN(r,icuriyr) +         &            !ceschp - start with credits for NG chp just calculated
             CGCOMmGEN (r,icuriyr,8,1)+CGCOMmGEN (r,icuriyr,8,2) + &            !ceschp - add grid sales and own use PV generation
             CGCOMmGEN (r,icuriyr,11,1)+CGCOMmGEN (r,icuriyr,11,2)              !ceschp - add grid sales and own use wind generation

           !USED FOR CES SERVICE REPORTS
!           WRITE (RCRPT,'("CES Credit Calcs",2i6,9F12.5)') &
!           icuriyr,r,KCHPCESGEN(r,ICuriyr),xgen,xgenownuse,xfuelusage,xpreCESGen,xpreCESGenOwnUse,xpreCESQ,EPRPSTGT(ICURIYR),EPRPSPR(ICURIYR)
         ENDIF   !ceschp - check for start of calculations - NG-fired chp systems installed starting in this year eligible for ces credits 

        END DO   ! End r loop for divisions

       KCHPCESGEN (MNUMCR,iCURIYR)= 0.0        !ceschp - initialize national credit total for ces

       DO r= 1, MNUMCR-2                                                        !ceschp - compute national level total for
         KCHPCESGEN(MNUMCR,iCURIYR) = KCHPCESGEN(MNUMCR,iCURIYR) +     &        !ceschp    ced credits in million kWh
               KCHPCESGEN(r,iCURIYR)                                            !ceschp
       END DO  ! r   ceschp

       DO f= 1, 12
        CGCOMmQ     (MNUMCR,iCURIYR,f)=   0.0
        CGCOMmGEN   (MNUMCR,iCURIYR,f,1)= 0.0
        CGCOMmGEN   (MNUMCR,iCURIYR,f,2)= 0.0
        CGCOMmCAP   (MNUMCR,iCURIYR,f)=   0.0

        DO r= 1, MNUMCR-2
         CGCOMmQ (MNUMCR,iCURIYR,f)= CGCOMmQ (MNUMCR,iCURIYR,f) + CGCOMmQ (r,iCURIYR,f)
         CGCOMmGEN (MNUMCR,iCURIYR,f,1)= CGCOMmGEN (MNUMCR,iCURIYR,f,1) + CGCOMmGEN (r,iCURIYR,f,1)
         CGCOMmGEN (MNUMCR,iCURIYR,f,2)= CGCOMmGEN (MNUMCR,iCURIYR,f,2) + CGCOMmGEN (r,iCURIYR,f,2)
         CGCOMmCAP (MNUMCR,iCURIYR,f)= CGCOMmCAP (MNUMCR,iCURIYR,f) + CGCOMmCAP (r,iCURIYR,f)
        END DO ! r
       END DO  ! f

       ! Map cogen results to COMMREP variables for FTAB,
       ! and convert units from Trills to Quads:
       CMUSCogen (1,iCURIYR)= CGCOMmQ (MNUMCR,iCURIYR,1) / 1000.0
       CMUSCogen (2,iCURIYR)= CGCOMmQ (MNUMCR,iCURIYR,2) / 1000.0
       CMUSCogen (3,iCURIYR)= CGCOMmQ (MNUMCR,iCURIYR,3) / 1000.0
       CMUSCogen (4,iCURIYR)= CGCOMmQ (MNUMCR,iCURIYR,6) / 1000.0

        IF(lprint) THEN
       !SUMMARY PRINTING FOR THE YEAR
        WRITE(rcrpt,*) 'TECHNOLOGY CLASS:  Photovoltaics'
        WRITE(rcrpt,*) ' DIV  BLDG  UsedRoofArea  MaxAvailableRoof   CommercialFS'
        WRITE(rcrpt,72) ((IDIV,IBLD, SolarPVUsedRoofArea(ICuriyr,IDIV,IBLD), &
                           SolarPVAvailRoofArea(ICuriyr,IDIV,IBLD), &
                           SurvFloorTotal(IDIV,IBLD,ICURIYR) &
                           +CMNewFloorSpace(IDIV,IBLD,ICURIYR), &
                           IDIV=1,NUMDIV),IBLD=1,NUMBLDG)

        WRITE(rcrpt,*) ' DIV    InstalledMW maxrecnicalMW  PctTechMW'
        WRITE(rcrpt,73) (IDIV, SolarPVInstalledMW(ICuriyr,IDIV), &
                           SolarPVTechPotentialMW(ICuriyr,IDIV), &
                           SolarPVInstalledMW(ICuriyr,IDIV)/SolarPVTechPotentialMW(ICuriyr,IDIV)*100., &
                           IDIV=1,NUMDIV)

        WRITE(rcrpt,*) 'TECHNOLOGY CLASS:  Wind (DWT)'
        WRITE(rcrpt,*) ' DIV    InstalledMW maxrecnicalMW  PctTechMW'
        WRITE(rcrpt,73) (IDIV, WindInstalledMW(ICuriyr,IDIV), &
                           WindTechPotentialMW(ICuriyr,IDIV), &
                           WindInstalledMW(ICuriyr,IDIV)/WindTechPotentialMW(ICuriyr,IDIV)*100., &
                           IDIV=1,NUMDIV)
        endif

        IF(lprint2) THEN
          DO NT=1,NUMTECHS
           WRITE(rcrpt,*) 'TECHNOLOGY CLASS:  ',AEQUIPNAME(NT,1)
           WRITE(rcrpt,*) ' DIV  BLDG  UNITS       INVESTMENT(mill)'
           WRITE(rcrpt,68) ((IDIV,IBLD,UNITS(ICURIYR,IDIV,IBLD,NT), &
                       INVEST(ICURIYR,IDIV,IBLD,NT),IDIV=1,NUMDIV), &
                       IBLD=1,NUMBLDG)

           WRITE(rcrpt,*) ' DIV   BLDG    TRILLS       TRILLSOWNUSE'
           WRITE(rcrpt,68) ((IDIV,IBLD,TRILLS(ICURIYR,IDIV,IBLD,NT), &
                       TRILLSOWNUSE(ICURIYR,IDIV,IBLD,NT),IDIV=1,NUMDIV), &
                       IBLD=1,NUMBLDG)

           WRITE(rcrpt,*) ' DIV   BLDG   fuelusage  HWSAVINGS   shsav'
           WRITE(rcrpt,64) ((IDIV,IBLD,fuelusage(ICURIYR,IDIV,IBLD,NT), &
                       HWBTU(ICURIYR,IDIV,IBLD,NT),shbtu(icuriyr,idiv,ibld,nt), &
                       IDIV=1,NUMDIV), IBLD=1,NUMBLDG)
          END DO
        ENDIF

 64     FORMAT(1X,2I4,3F15.1)
 67     FORMAT(1X,I4,F15.0)
 68     FORMAT(1X,2I4,2F15.1)
 72     Format(1x,2I4,f15.1,2F15.1)
 73     Format(1x,I4,2F15.1,F10.4,"%")

!-------------------------------------------
! PRINT THE DISTRIBUTED GENERATION DATABASE
!-------------------------------------------
        IF (ICURIYR.EQ.LASTYR .AND. FCRL.EQ.1) THEN
          DGDB = FILE_MGR ('O','KDGENOUT',.TRUE.)
          WRITE(DGDB,69)
          DO NT=1,NUMTECHS
           DO IYR=CMFirstYr,LASTYR
            DO IDIV=1,NUMDIV
             DO IBLD=1,NUMBLDG
                 xtrills=TRILLS(IYR,IDIV,IBLD,NT)-TRILLSOWNUSE(IYR,IDIV,IBLD,NT)
                 xunits=UNITS(IYR,IDIV,IBLD,NT)-UNITS(IYR-1,IDIV,IBLD,NT)
                 if(xunits>0. .and. iyr>cmfirstyr) then
                  xcalckw=(capacity(iyr,idiv,ibld,nt)-capacity(iyr-1,idiv,ibld,nt))/xunits
                 else
                  xcalckw=xkw(NT,iyr+1989-ifirstyr(nt,1)+1)	!kj
                 endif
                  WRITE(DGDB,71) AequipName(NT,iyr+1989-ifirstyr(nt,1)+1),IYR+(BASEYR-1), &	!kj
                   IDIV,BLDGNAME(IBLD), &
                   UNITS(IYR,IDIV,IBLD,NT), xcalckw , &    !xcalckw is not the stock average, it is the average for added in the year
                   xcalckw*xunits,                    &
                   TRILLS(IYR,IDIV,IBLD,NT),xtrills,  &
                   HWBTU(IYR,IDIV,IBLD,NT),shbtu(iyr,idiv,ibld,nt), &
                   fuelusage(IYR,IDIV,IBLD,NT), &
                   INVEST(IYR,IDIV,IBLD,NT)
             END DO
            END DO
           END DO
          END DO
         DGDB = FILE_MGR ('C','KDGENOUT',.TRUE.)
 69      Format(1x,'Tech,Year,Division,BldgType,#Units,AvgKWCap,TotKWAdded,GEN(tBtu),GridSales,HWOut,SHOut,FuelInp,Invest($mill)')
 71      FORMAT(1X,a14,',',I5,',',I5,',',A14,',',F12.2,',',F12.2,7(',',F15.5))
        ENDIF

            RETURN     ! SEND CONTROL BACK TO COMM
       END

!====================================
!     Learning Cost Function
!====================================
      REAL FUNCTION LearnCost*4 (MaxCost,Beta,c0,CumShip,report)
      ! This function returns the projected cost of equipment based on cumulative
      !  shipment estimates (from the prior year)


      IMPLICIT NONE

      REAL*4 MaxCost     ! maximum cost set equal to default projections
      REAL*4 Beta        ! the learning cost function shape parameter
      REAL*4 c0          ! first unit cost
      REAL*4 CumShip     ! cumulative shipments through the previous year

      INTEGER*4 report   ! link to krpt output file handle

      If (cumship .le. 1.) LearnCost=MaxCost
      If (cumship .le. 1.) Return
      If (c0 .eq. 0.) LearnCost=MaxCost
      If (c0 .eq. 0.) Return

      LearnCost=min( MaxCost, exp( log(c0) - Beta*log(CumShip) ) )
      Return

      END

!====================================
!     Net Present Value Function
!====================================
      Real FUNCTION XNPV*4 (xNetCashFlow,xDisc,iYrs,report)
      ! This function returns the NPV of a Cash Flow of iYrs years

      IMPLICIT NONE
      REAL*4 xNetCashFlow(iyrs)   ! the array of CF entries
      REAL*4 xDisc       ! the discount rate to use
      Integer iyrs       ! the number of years for the cash flow
      Integer report     ! when debugging, results printed to this unit
      Integer i
        XNPV=0.
        Do i=iyrs,2,-1
          XNPV= (XNPV + xNetCashFlow(i))/(1+xDisc)
           !debug only: Write (report,*) i,xDisc,xNetCashFlow(i),XNPV
          Enddo
            XNPV=XNPV+xNetCashFlow(1)
         !debug only: write (report,*)"NPV= ",XNPV
       RETURN
       END
