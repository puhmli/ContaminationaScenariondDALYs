#dairy chain struture
#-----------------------------------------------------------------------------------------
#feed
##number of feed mill 30
#total production of compound feed in 2018 3586000 tons
##production of compound feed for dairy cow 350,000 kg/day
3586000*(15000/16963)/(300*30)
#-----------------------------------------------------------------------------
#farm
#number of dairy cow per farm 100
#intake of compound feed per cow 5kg/day
##number of dairy farm 15000, in 2018 16,963 dairy farms
##number of trucks 4farms/truck
#30 FEED MILLS; 15000 DAIRY FARMS; 100 DAIRY COWS PER FARM; 5KG COMPOUND FEED/COW/DAY
#EVERY FARM RECEIVE FEED EVERY 14 DAYS, ONE FEED MILL DISTRIBUTE COMPOUND FEED TO 50 FARMS PER DAY
#-----------------------------------------------------------------------------





#contamination scenario and DALYs____AFB1/M1

#_____________________________________________________________________________________________________________________
#######Aflatoxin concentration simulation from compound feeds through dairy farms to dairy trucks##### 
###### EU maximum level: 0.05 ug/kg for raw milk; AfB1 in all feed materials was set at 0.02 mg/kg, as well as for compound feed for 0.005 mg/kg
###An average herd size of 100 cows was assumed, with an average total daily feed intake (and standard deviation) of 18.7 (1.3) kg DM/cow, of which 4.3 (0.2) kg DM/cow is compound feed.
#_______________________________________________________________________________________________________________________________
maize_afb1 <-50.2  #ug/kg incidents in 2013
maize_CF <-0.205 #rate of maize in Compound feed
afb1_CF<-maize_afb1*maize_CF 
#--------------------------------------------------------------------------------------------------------------
#####afb1_CF AFB1 concentration in compound feed *********
#---------------------------------------------------------------------------------------------------------------
afb1_CF <- 10.3 #ug/kg eu limits deterministic
#------------------------------------------------------------------------------------------------------
########total daily intake of Afb1 by dairy cow; 0.88 is the dry matter percentage in compound feed.
#####daily intake of compound feed is 5 kg (2016)
#-------------------------------------------------------------------------------------------------------------
I_CF <- 5
TDI_Afb1 <- I_CF* afb1_CF
#--------------------------------------------------------------------------------------------------------------
###### AFM1 in farm milk; transfer equation used is from EFSA opinion (ng_AfM1/kg_milk)
#----------------------------------------------------------------------------------------------------------
CM_AFM1 <- TDI_Afb1*0.787+10.95
#----------------------------------------------------------------------------------------------------------
###UNITS change to ug_AfM1/kg_milk
#---------------------------------------------------------------------------------------------
CM_AFM1_ug <- CM_AFM1/1000
#-------------------------------------------------------------------------------------------------------
##AFM1 in truck milk #background =0.037 ug/kg
AFM1_MT <- (CM_AFM1_ug+0.037*3)/4
#------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
#######	DALYs_AM= HCC *((tf*dwf * pf) + (tnf * dwnf * pnf) + (YLL*pf))
###HCC = HCC a- * (1-Phbv)+ HCCa+ * Phbv
####HCCa- = Rfa- * EXP_AM * RR; HCCa+ = Rfa+  * EXP_AM *RR
####EXP_AM = C_AFM * Cons_Milk /BW
#--------------------------------------------------------------------------------------------------------
######Decision limit AFM1 0.05 ug/kg for raw milk; AfB1 in all feed materials was set at 0.02 mg/kg, in compund feed is0.005 mg/kg
#####extra exposure assessment due to contaminated milk
#DELTA_EXP_AFLA_MILK  ng/kg
#--------------------------------------------------------------------------------------------------------
prob_con_afl <- 0.04 #CONTAMINATION FRACTION IN RETAIL
cons_milk <- 350 #consumption of milk per day per person male 514 female 376 updation
BW <- (74+85)/2
#afm1 in truck (51.4+37*3)/4
AFM1_MT<-(51.4+37*3)/4
#extra exposure assessment of afm1 due to inatke of contaminated milk
DELTA_EXP_AFLA_MILK <-(40.6)*prob_con_afl*cons_milk/BW #40.6 is the conctamination concentration in retail milk
Rfa_ng <- 0.01
Rfa_ps <- 0.3
Phbv <- 0.5/100
RR_afm1_afb1<- 0.1
HCC_neg <-Rfa_ng*DELTA_EXP_AFLA_MILK*RR_afm1_afb1
HCC_pos <- Rfa_ps*DELTA_EXP_AFLA_MILK*RR_afm1_afb1
HCC <- HCC_neg*(1-Phbv)+HCC_pos*Phbv
#--------------------------------------------------------------------------------------------------------
####duration for fatal cancer 0.5 years;Duration of disease of non-fatal cancer 5 years
#--------------------------------------------------------------------------------------------------------
tf <- 0.5
tnf <- 5
dwf <-0.508
dwnf <- 0.294
pf <- 0.92
pnf <-0.08
YLL <- 80.62-63.7

#--------------------------------------------------------------------------------------------------------
##############DALYsmean  /100,000 illneass
#--------------------------------------------------------------------------------------------------------
DeltaDALYs_AM= HCC *((tf*dwf * pf) + (tnf * dwnf * pnf) + (YLL*pf))
#----------------------------------------------------------------------

#contamination scenario and DALYs____dioxins
#----------------------------------------------------------------------
########Dioxin  concentration simulation from compound feeds through dairy farms to dairy trucks##############
####animal feed####### 0.75 pg WHO-TEQ g-1 in feed legal maximum limit

#----------------------------------------------------------------------
###Df Dioxins concentration in animal feed (pg WHO-TEQ g-1)
#----------------------------------------------------------------------
df <-  0.75 
#----------------------------------------------------------------------
######Co Carry-over rate of dioxins from animal feed to bovine milk Exponential 40 %
#----------------------------------------------------------------------
C0 <- 0.40
#----------------------------------------------------------------------
#####Dwm Dioxins concentration in pasteurised bovine milk Df *Co * FAT%  pg WHO-TEQ g-1 milk fat
#----------------------------------------------------------------------
Dwm <- df*C0/0.04
#----------------------------------------------------------------------
################exposure of dioxin in all food#####
#----------------------------------------------------------------------
expdioallfood <- 1.3
####Ww Mean weight (women; 18-64 years), Fixed value () kg
#----------------------------------------------------------------------
Ww <- 74
####Wm Mean weight (men; 18-64 years) Fixed value () kg
#----------------------------------------------------------------------
Wm<- 85
#----------------------------------------------------------------------
#mean body weight
BW<-(74+85)/2
#----------------------------------------------------------------------
#milk consumption per person per day 350 g
#----------------------------------------------------------------------
########extra exposure assessment due to conatminated milk
##decision limit for dioxin in milk 2 pg WHO-TEQ g-1 milk fat; in feed 0.75 pg WHO-TEQ g-1 in feed legal maximum limit
#----------------------------------------------------------------------
fat <-0.04
DL_dio_m<- 2
DL_dio_f<- 0.75
prob_con_dio <- 0.04 #contamination fraction
DELTA_EXPDIOMILK <- (2.25) *fat*cons_milk*prob_con_dio/BW #2.25 is the contamination concentration in truck and retail milk
#----------------------------------------------------------------------
######################################DALYs##############################################################################
#####################incidence rate ###1.Hypothyroidy due to prenatal exposure 2.hypothyroidy due to postnatal exposure
#####3.male infertility due to prenatal exposure
#####Disability weight (DW)
#----------------------------------------------------------------------
DW_Hpre <-0.019
DW_Hpos <- 0.019
DW_Infer <- 0.056
#----------------------------------------------------------------------
##### Duration / assume that: the average life in netherlands is 80
#----------------------------------------------------------------------
DRD_Hpre <- 80-0
DRD_Hpos <- 80-20
DRD_Infer <- 45-20
#----------------------------------------------------------------------
######incidence rate
#----------------------------------------------------------------------
IR_Hpre <- 0.0109686
IR_Hpos_0 <- 0.40951; IR_Hpos_100 <-1.29498; IR_Hpos =  IR_Hpos_100
IR_Infer <- 0.0109686
#----------------------------------------------------------------------
#########DALYs milk without contamination
#----------------------------------------------------------------------
DALYs_all <-  IR_Hpre * DW_Hpre * DRD_Hpre+IR_Hpos * DW_Hpos * DRD_Hpos +  IR_Infer * DW_Infer * DRD_Infer
#----------------------------------------------------------------------
######extra DALYs
#----------------------------------------------------------------------
expdioallfood <- 1.3
delta_DALYs_milk <- DELTA_EXPDIOMILK/expdioallfood * DALYs_all
#DELTA_EXP_AFLA_MILK  ng/kg
#--------------------------------------------------------------------------------------------------------
