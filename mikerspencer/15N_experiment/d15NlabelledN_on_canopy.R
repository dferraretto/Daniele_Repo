###########################################################################
##  LABEL    TRACE   TO    SOLUTION
###########################################################################

# ECOSYSTEM PARAMETERS [MUTABLE] ----------------------------------------------------

averagemonthlyrainfall = 63 # precipitation (mm) on April 2014, month of the highest Ndep registered in Griffin

mRF.Ngm2 = 1978/10000 # total N/m2 (g) on April 2014, see table 1. To calculate 15N, it might be the case to refer to the yearly natural max uptake (d15N=5)
yRF.Ngm2 = 13063/10000 # total N/m2 (g)
canopyCover = 0.9 # % of forest soil covered by the canopy
treeCanopym2 = 10000/1883*canopyCover # = 4.78m2, area of the average tree canopy. Being a specific tree, would it be more correct to try to calculate the real surface?
Nmass.on.tree = treeCanopym2*mRF.Ngm2 # less than 1g of N per tree,
  
#CALCULATIONS - NATURAL ABUNDANCE RF POOLS [DERIVED] -------------------------------------------

rfNgm2 = mRF.Ngm2 # total N/m2 (mg) on April 2014
NAmr=MrNitrogen(atmpct_NA)
waterNatomsm2 = waterNgm2/NAmr*avogadro

#CALCULATIONS - NATURAL ABUNDANCE water POOLS: INITIAL ISOTOPES [DERIVED] -------------------------------------------
# Here we assume that the pool SIZE does not change but the 15N content can due to nutrition from the litter

i_water15Natomsm2 = waterNatomsm2*atmpct_NA/100
i_waterd15N = atmpct2d(atmpct_NA,RstdN)

# TARGET ISOTOPES  [MUTABLE]---------------------------------------------------------

t_waterd15N = 50 # what is the target d15N? NB: Dave suggest a much higher d15N

# TARGET ISOTOPES  [DERIVED]---------------------------------------------------------

t_wateratmpct=d2atmpct(t_waterd15N,RstdN) # target atom% 15N in water
t_water15Natomsm2 = waterNatomsm2*t_wateratmpct/100 # target atoms/m2 if the fixed size water N pool has the desired d15N

t_water15Natomsm2.gained = t_water15Natomsm2-i_water15Natomsm2 # 15N atoms which must be gained from the litter

a_15Natomsm2 = (t_water15Natomsm2.gained/uptake*100)/decomp*100 # given the decomp. and uptake. rate, how many atoms need to be available?
a_Natomsm2 = a_15Natomsm2/(ll_atmpct15N/100) # total N atoms to be added in labelled litter
a_Ngm2=a_Natomsm2*ll_Mr/avogadro # grams of N per m2 needed to be added in labelled litter

# OUTPUT ------------------------------------------------------------------
a_glitterm2 = a_Ngm2*1000/ll_Nmgg # how much litter needs to be added per m2 to achieve desired d15N given water parameters.
a_glitterm2

# 15N MASS     ------------------------------------------------------------

15N = x
Rst = 0.0036765 # air standard. R = H/L isotope ratio. Rst = k!
b = Ntot
delta=((NRsample/NRst)-1)*1000

Rsa=((delta/1000)+1)*Rst # Rsa = H/(1 à H)

Fractionation=(delta+1000)/(delta+1000+(1000/Rst))
atmprc= 100*Fractionation

