#LABELLED LITTER DELIVERY CALCULATIONS
#this is a translation of the excel sheet to make checking the math easier




#ISOTOPE NOTATION CONVERSION FUNCTIONS-------------------------------------------

d2atmpct<-function(delta,Rstd){
  (delta+1000)/((delta+1000+1000/Rstd))*100 # fractionation? derived from delta definition
}
atmpct2d<-function(atmpct,Rstd){
  ((1000*atmpct)/(100*Rstd-Rstd*atmpct))-1000
}
MrNitrogen<-function(atmpct){
  (atmpct/100*15)+((100-atmpct)/100*14)
}

# ECOSYSTEM PARAMETERS [MUTABLE] ----------------------------------------------------

grass_gm2= 200 # grass biomass per m2, upper limit from Mirco's majadas document
grassNmgg = 20 # mg per g of N in grass
atmpct_NA = 0.36765 # atom % at natural abundance of grass biomass
decomp = 80 # percentage of litter N which is released by litter over the time period of the experiment
#NB: above we have assumed that decomposition is an transformation from litter-N to plant-accessable mineral N.
uptake = 20 # percentage of this released litter N which is obtained by plants 

#ISOTOPE CONSTANTS [FIXED] -------------------------------------------------------------

avogadro = 6.0221409E+23 # Avogadro's number 
RstdN = 0.0036764 # Rstandarad for 15N calculations 

# LITTER CREATION PARAMETERS [MUTABLE] -------------------------------------

ll_Nmgg = 80 # mg per g of N per dry biomass litter
ll_d15N = 5000 # what is the d15N achievable in the labelled litter? this depends on the d15N of the fertilizer
ll_atmpct15N = d2atmpct(ll_d15N,RstdN) # converted to atmpct
#ll_atmpct15N = 5 # alternatively replace this with a desired atmpct; a higher atmpct may be possible

ll_Mr = MrNitrogen(ll_atmpct15N)


###########################################################################
## 1. LABEL TRACE TO GRASSES -----------------------------------------------
###########################################################################

#CALCULATIONS - NATURAL ABUNDANCE GRASS POOLS [DERIVED] -------------------------------------------

grassNgm2 = grass_gm2*grassNmgg/1000 # calculated grass N pool (g) per m2
NAmr=MrNitrogen(atmpct_NA)
grassNatomsm2 = grassNgm2/NAmr*avogadro

#CALCULATIONS - NATURAL ABUNDANCE GRASS POOLS: INITIAL ISOTOPES [DERIVED] -------------------------------------------
# Here we assume that the pool SIZE does not change but the 15N content can due to nutrition from the litter

i_grass15Natomsm2 = grassNatomsm2*atmpct_NA/100
i_grassd15N = atmpct2d(atmpct_NA,RstdN)

# TARGET ISOTOPES  [MUTABLE]---------------------------------------------------------

t_grassd15N = 20 # what is the target d15N?

# TARGET ISOTOPES  [DERIVED]---------------------------------------------------------

t_grassatmpct=d2atmpct(t_grassd15N,RstdN) # target atom% 15N in grass
t_grass15Natomsm2 = grassNatomsm2*t_grassatmpct/100 # target atoms/m2 if the fixed size grass N pool has the desired d15N

t_grass15Natomsm2.gained = t_grass15Natomsm2-i_grass15Natomsm2 # 15N atoms which must be gained from the litter

a_15Natomsm2 = (t_grass15Natomsm2.gained/uptake*100)/decomp*100 # given the decomp. and uptake. rate, how many atoms need to be available?
a_Natomsm2 = a_15Natomsm2/(ll_atmpct15N/100) # total N atoms to be added in labelled litter
a_Ngm2=a_Natomsm2*ll_Mr/avogadro # grams of N per m2 needed to be added in labelled litter

# OUTPUT ------------------------------------------------------------------
a_glitterm2 = a_Ngm2*1000/ll_Nmgg # how much litter needs to be added per m2 to achieve desired d15N given grass parameters.
a_glitterm2

#Cores
core_dia = 15/100
core_area= pi*(core_dia/2)^2

core_litter=5*core_area

20*48

##############################################################################
# 2: UNDER A TREE  -----------------------------------------------------------
##############################################################################

# Could a label from this litter be detected in the trees? 
# this is more complicated as under the trees there is also a grass layer, and trees have a large biomass which is mostly wood with a high C:N ratio 
# we will assume that the grass below the trees is the same as the grass on its own, but trees take an additional portion of the available N
# templar 2012 meta analysis does not have a mixed system. Her grassland uptake is 30 % of mineral tracers, while in forests trees get 5% aboveground, and 5-10% belowground
# we will crudely say that trees obtain 5% of available N, which probably is an underestimate. 

uptake_t=5


# TREE BIOMASS AND AREA CALCULATIONS --------------------------------------

canopycover = 20 # approximately 20% canopy cover
treesha=25 # upper estimate of trees per ha

canopyarea=(10000/canopycover)/treesha # canopy area of each tree

# assume that available root expoloratory area has a fixed ratio with canopy cover
rcratio = 1 # assume this is 1:1- this is very hard to predict in the dehasa
rarea = canopyarea*rcratio

#Tree pools
#values from Majadas site descriptions
AerialWDBm=2.64 # Aerial woody biomass in kg dry matter per m2
AerialWDBha=AerialWDBm*10000 # per ha
BGWDm=0.996 # belowground woody biomass in kg dry matter per m2
BGWDBha=BGWDm*10000 # per ha

tree_AerialWDBkg = AerialWDBha/treesha   # per tree aerial woody biomass 
tree_BGWDBkg = BGWDBha/treesha # per tree belowground woody biomass

fpct = 1 # percentage of aboveground biomass which is foliage. This is VERY crudely estimated from Poorter 2012 NP ,d
tree_Leafkg = fpct/100*tree_AerialWDBkg # dry mass foliage

# TREE C AND N POOL CALCULATIONS ------------------------------------------------

QilexCpct=47.5 # from Migliavacca document, we will use this for all 3 pools for now, should probably adjust this for foliage at least

tree_WoodCkg = tree_AerialWDBkg*QilexCpct/100
tree_RootCkg = tree_BGWDBkg*QilexCpct/100
tree_LeafCkg = tree_Leafkg*QilexCpct/100

#CN RATIOS
WoodCN=500 # 500:1 WOOD CN is used in Ndep partitioning
FoliarNmgg = 14 # N in mg per g leaves from Mirco document, apparrently quite constrained
tree_LeafNkg=tree_Leafkg*FoliarNmgg/1000 #N pool in leaves
tree_LeafCN=tree_LeafCkg/tree_LeafNkg

tree_WoodNkg=tree_WoodCkg/WoodCN
tree_RootNkg=tree_RootCkg/WoodCN


# Tree Isotope Calculations -----------------------------------------------
#Assume tree is a single pool of N, with natural abundance atom% and split by the ratio of treeWoodNkg:TreeRootNkg:treeLeafNkg
#If anything, this should underestimate leaf 15N uptake as we should expect most N gained to be allocated to growing leaves.
#Assume no gain in biomass, and the entire foliar pool is the same N pool, which gains extra N randomly (rather than leaf building)
#Will later do a model where n is only added to foliage and roots

tree_Nkg=tree_WoodNkg+tree_RootNkg+tree_Leafkg
tree_Natoms=tree_Nkg*1000/NAmr*avogadro
i_tree_15Natoms=tree_Natoms*atmpct_NA/100

# TARGET ISOTOPES  [MUTABLE]---------------------------------------------------------

t_treed15N = t_grassd15N # assume this is the same as before.

# TARGET ISOTOPES  [DERIVED]---------------------------------------------------------

t_treeatmpct=d2atmpct(t_treed15N,RstdN) # target atom% 15N in trees
t_tree15Natoms = tree_Natoms*t_treeatmpct/100 # target 15N atoms per tree

t_tree15Natoms.gained = t_tree15Natoms-i_tree_15Natoms

# work in by units of 'tree root area'; how much per tree
a_15Natoms_treerootarea = (t_tree15Natoms.gained/uptake_t*100)/decomp*100 
a_Natoms_treerootarea = a_15Natoms_treerootarea/(ll_atmpct15N/100)
a_Ng_treerootarea=a_Natoms_treerootarea*ll_Mr/avogadro # grams of N per needed in litter per tree root area

# OUTPUT ------------------------------------------------------------------
a_glittertreerootarea = a_Ng_treerootarea*1000/ll_Nmgg # how much litter needs to be added per m2 to achieve desired d15N given grass parameters.
a_glittertreerootarea # grams labelled litter needed per tree root area to supply 1 tree with enough 15N to change d15N to 20 permil



#################################################################################
# 3. Tree Isotope Calculations v2: N demand between pools differs
#################################################################################

tree_WoodNkg
tree_LeafNkg
tree_RootNkg

#Assume tree is a single pool of N, with natural abundance atom% and split by the ratio of treeWoodNkg:TreeRootNkg:treeLeafNkg
#If anything, this should underestimate leaf 15N uptake as we should expect most N gained to be allocated to growing leaves.
#Assume no gain in biomass, and the entire foliar pool is the same N pool, which gains extra N randomly (rather than leaf building)
#Assume the foliage is the main beneficiary of tree N uptake, and a set amount of N goes to the foliage.
#Assume wood gains little N as less maintenance

#These are not based on literature - find some replacements!
N_leaffrac=60 # this % of N uptake is going to building new foliage; i.e. foliage recieves more N than the size of its N pool 
N_rootfrac=35 # this % of N uptake is going to the roots
N_woodfrac=5 # this % of N uptake is going to the stem


tree_LeafN_atoms=tree_LeafNkg*1000/NAmr*avogadro
tree_RootN_atoms=tree_RootNkg*1000/NAmr*avogadro


i_Leaf_15Natoms=tree_LeafN_atoms*atmpct_NA/100
i_Root_15Natoms=tree_RootN_atoms*atmpct_NA/100


# TARGET ISOTOPES  [DERIVED]---------------------------------------------------------

t_Leaf15Natoms = tree_LeafN_atoms*t_treeatmpct/100 # target 15N atoms in the foliage
t_Root15Natoms = tree_RootN_atoms*t_treeatmpct/100 # target 15N atoms in the foliage

t_Leaf15Natoms.gained = t_Leaf15Natoms-i_Leaf_15Natoms # how many 15N atoms the leaves need to gain to reach the target atmpct
t_Root15Natoms.gained = t_Root15Natoms-i_Root_15Natoms # how many 15N atoms the roots need to gain to reach the target atmpct
# work in by units of 'tree root area'; how much per tree

t_Leaf_15Natoms.gainedtotal =t_Leaf15Natoms.gained/N_leaffrac*100 # total N atoms needed given uneven partitioning
t_Root_15Natoms.gainedtotal =t_Root15Natoms.gained/N_rootfrac*100 

a_Leaf_15Natoms_treerootarea = (t_Leaf_15Natoms.gainedtotal/uptake_t*100)/decomp*100 
a_Root_15Natoms_treerootarea = (t_Root_15Natoms.gainedtotal/uptake_t*100)/decomp*100 

a_Leaf_Natoms_treerootarea = a_Leaf_15Natoms_treerootarea /(ll_atmpct15N/100)
a_Leaf_Ng_treerootarea=a_Leaf_Natoms_treerootarea*ll_Mr/avogadro # grams of N per needed in litter per tree root area to raise leaf d15N to target

# OUTPUT ------------------------------------------------------------------
a_Leaf_glittertreerootarea = a_Leaf_Ng_treerootarea*1000/ll_Nmgg # how much litter needs to be added per m2 to achieve desired d15N given grass parameters.
a_Leaf_glittertreerootarea # grams labelled litter needed per tree root area to supply 1 tree with enough 15N to change d15N of leaves to 20permil

a_Leaf_glitterm2=a_Leaf_glittertreerootarea/rarea
a_Leaf_glitterm2

a_Leaf_glittertreerootarea

tlitter = 2000 # total biomass litter possible to create
tlitter/a_Leaf_glittertreerootarea




