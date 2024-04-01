* Give code name of soil data file to match the water balance PADDY:
SCODE = 'PADDY'
*---------------------------------------------------------------*
* 1. Various soil and management parameters
*---------------------------------------------------------------*
WL0MX = 100.    ! Bund height (mm)
NL = 7      ! Number of soil layers (maximum is 10) (-)
TKL = .1,.1,0.05,.15,.2,.2,.2
ZRTMS = 1.0   ! Maximum rooting depth in the soil (m)
*---------------------------------------------------------------*
* 2. Puddling switch: 1=PUDDLED or 0=NON PUDDLED
*---------------------------------------------------------------*
SWITPD = 0 ! Non puddled
*SWITPD = 1  ! Puddled
* If PUDDLED, supply parameters for puddled soil
NLPUD = 2 ! Number of puddled soil layers, including the plow sole (-)
! (NLPUD cannot exceed the total number of soil layers NL)
WCSTRP =0.395827,0.395827,0.26, 0.411403,0.424184,0.429259,0.429499
* Soil water tension of puddled soil layer at which cracks reach
* break through the plow sole (pF):
PFCR = 6.
*---------------------------------------------------------------*
* 3. Groundwater switch: 0=DEEP (i.e., not in profile), 1=DATA
* (supplied), 2=CALCULATE
*---------------------------------------------------------------*
*SWITGW = 0 ! Deep groundwater
*SWITGW = 2 ! Calculate groundwater
SWITGW = 1 ! Groundwater data
* If DATA, supply table of groundwater table depth (cm; Y-value)
* as function of calendar day (d; X value):
ZWTB =   1.,500.,
366.,500.
* If CALCULATE, supply the following parameters:
ZWTBI = 100. ! Initial groundwater table depth (cm)
MINGW = 100. ! Minimum groundwater table depth (cm)
MAXGW = 100. ! Maximum groundwater table depth (cm)
ZWA   = 1.0  ! Receding rate of groundwater with no recharge (cm d-1)
ZWB   = 0.5  ! Sensitivity factor of groundwater recharge (-)
*---------------------------------------------------------------*
* 4. Percolation switch
* Value for SWITVP can not be 1 (CALCULATE) for nonpuddled soil
*---------------------------------------------------------------*
SWITVP = -1 ! Fixed percolation rate
*SWITVP = 0 ! Percolation as function of the groundwater depth
*SWITVP = 1 ! Calculate percolation
*SWITVP = 2 ! Fixed percolation rate as function of time
* If SWITVP = -1, supply fixed percolation rate (mm d-1):
FIXPERC = 1.03
* If SWITVP = 0, supply table of percolation rate (mm d-1; Y-value)
* as function of water table depth (cm; X value):
*PERTB =   0., 3.,
*         200., 3.
* If SWITVP = 2, give percolation rate (mm/d) as function of calendar day
PTABLE =
1.,  1.0,   !First number is calendar day, second is percolation rate)
50., 1.0,
100., 20.0,
366., 20.0
*---------------------------------------------------------------*
* 5. Conductivity switch: 0=NO DATA, 1=VAN GENUCHTEN or 2=POWER
*  OR 3= SPAW  function used
*---------------------------------------------------------------*
SWITKH = 0 ! No data
*---------------------------------------------------------------*
* 6. Water retention switch: 0=DATA; 1=VAN GENUCHTEN. When DATA, data
* have to be supplied for saturation, field capacity,
* wilting point and at air dryness
*---------------------------------------------------------------*
SWITPF = 0 ! Data
*---------------------------------------------------------------*
*7.Soil physical properties, these parameters will be used when model
*runs under actual water or nitrogen condition, or even both. Otherwise
*these parameter will not be used.
CLAYX =0.248,0.248,0.46, 0.318,0.362,0.378,0.378
SANDX =0.506,0.506,0.28, 0.456,0.422,0.41,0.408
BD =1.346,1.346,1.58,1.89,1.382,1.45,1.446
*Soil organic carbon and nitrogen content in kg C or N/ha
SOC =6329.784,6329.784,1494.11, 5974.43,5506.776,4449.2,3985.352
SON =234.21,263.11,66.14,265.56,262.56,247.43,242.58
SNH4X =3.133477232,3.133477232,0.7628,3.0512,3.184116944,2.7723884,2.521824
SNO3X =1.04449241066667,1.04449241066667,0.254,1.017,1.06137231466667,0.924129466666667,0.840608
SPH =5.926,5.926,5.18,5.812,5.872,5.904,5.92
*---------------------------------------------------------------*
* 7. Soil hydrological properties. Required type of data input
* according to setting of conductivity and water retention switch
*---------------------------------------------------------------*
* Saturated hydraulic conductivity, for each soil layer
* (cm d-1) (always required!):
KST =6.833053,6.833053,0.35,1.031488,1.785245,1.472011,1.47285
* Saturated volumetric water content, for each soil layer
* (m3 m-3)(always required!):
WCST =0.395827,0.395827,0.26,0.411403,0.424184,0.429259,0.429499
WCFC =0.257104,0.257104,0.19,0.305411,0.335348,0.345957,0.346194
WCWP =0.109006,0.109006,0.13,0.140696,0.186638,0.196013,0.196007
WCAD = 0.041094,0.050921,0.055146,0.099286,0.114151,0.11415, 0.10415
*---------------------------------------------------------------*
* 8. Initialization conditions, and re-initialization
*---------------------------------------------------------------*
WL0I = 10.   ! Initial ponded water depth at start of simulation (mm)
* Initial volumetric water content at the start of simulation,
* for each soil layer (m3 m-3):  USE ALWAYS FIELD CAPACITY, OR 0.5 TIMES WCST
WCLI =0.37603565,0.37603565,0.259,0.39083285,0.4029748,0.40779605,0.42090902
RIWCLI = 'NO'
*---------------------------------------------------------------*
* 9. Initialization of soil thermal conditions
*---------------------------------------------------------------*
SATAV = 18.0       !Soil annual avaerage temperature of the first layers
SOILT =25.,24.,24.,23.,22.,21.,20.
WCLINT = 1,1,1,
2,2,2,
3,3,3,
4,4,4,
5,5,5,
6,6,6,
7,7,7

*WL0_FRC = 0
WL0_FRC = 2
WL0_OBS = 
2012.0, 13.0, 15.0, 
2012.0, 21.0, 12.0, 
2012.0, 27.0, -150.0, 
2012.0, 40.0, 2.0,
2012.0, 70.0, 2.0,
2012.0, 89.0, -200.0

WCL1_FRC = 0
*WCL1_FRC = 2
WCL1_OBS = 
2012.0, 11.0, 0.38, 
2012.0, 31.0, 0.39, 
2012.0, 90.0, 0.36, 
2012.0, 110.0, 0.27

WCL2_FRC = 0
WCL2_OBS = 
2012.0, 11.0, 0.385, 
2012.0, 31.0, 0.395, 
2012.0, 90.0, 0.365, 
2012.0, 109.0, 0.30

WCL3_FRC = 0
WCL3_OBS = 
2012.0, 11.0, 0.261, 
2012.0, 31.0, 0.260, 
2012.0, 90.0, 0.249, 
2012.0, 110.0, 0.208

WCL4_FRC = 0
WCL4_OBS = 
2012.0, 11.0, 0.412, 
2012.0, 31.0, 0.395, 
2012.0, 90.0, 0.412, 
2012.0, 110.0, 0.395