# 15N recovery calculations


a = 25.29# 15N added (mg)

b = 151.45# N mass of the pool in question (mg)

c = 660.95# d15N sample as from labs

d = 0.0036765 # R standard for 15N/14N

N.15.rec = (b/a)*((c*d+d)/1000)/(1+((c*d+d)/1000))*100
