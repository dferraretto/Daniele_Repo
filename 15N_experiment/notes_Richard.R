########################################################################
#
#         Application of 15N CNU to targer branches                    #
#
########################################################################

# Required data: 15N content in needles, twigs, branches
branches = 3 # number of trated branches

15N.perha = 54 # grams of 15N applied in Nair experiment to saplings
15N.per.tree = 54/1883

# number of collections = 13

# number of needles = 25. First collection was a sigle bud, then from the whole biomass and finally (destructive sampling) the number was increased to 100.

# branch and twigs no of collections = 2

15Nrecovery = 15Nexcess*N*100/15Nadded 

# where:
15N excess = 15N.treatment - 15N.control

# Now, I won't have the chance to measure 15N over the whole plant (bark, roots, soil) so I will stick at the parts in direct contact with the treatment
# and mesure the content in there: how much was found by Richard in needles and twigs?
# Richard's needles 15Nrecovery (CNU) = 7/13% (+-3.04)
# Richard's twigs 15Nrecovery (CNU) = 
# Richard's branches 15Nrecovery (CNU) = 20.77 (+-2.86) which scaled makes a 60%

# Open questions: is 15 days enough to take my samples?

# From Richard numbers: I measure what I find in needles and branches as I want to estimate the N interception on a (very) short term. The more I wait, the more I expect the N
# to migrate towards other compartments. Maybe I should even consider to take my samples after one day or a couple of days max?


