# ice quality measurements using modified Gold's equation from 
# Weyhenmeyer et al 2022 (DOI: 10.1038/s41467-022-32633-1)
# https://www.nature.com/articles/s41467-022-32633-1


#Function for allowable load (P)----

load <- function(a, h, perc_wht){
  
  #Use Eq.2 from Weyhenmeyer et al., 2022 to solve for allowable load
  p <- ((a*h^2)/2)*(1+((100-perc_wht)/100))
  
  #Print result. Result is allowable load on ice in kg.
  print(p)
}

#Test
#load(a = 3.5, h = 10, perc_wht = 0)

#Function for ice thickness (H)----

ice_thickness <- function(a, p, perc_wht){
  
  #Rearrange equation to solve for h
  h <- sqrt((2/a)*(p/(1+((100-perc_wht)/100))))
  
  #Print result. Result is ice thickness in cm
  print(h)
}

#Test
#ice_thickness(a = 3.5, p = 350, perc_wht = 0)

#What is the bearing capacity for a person walking?

#Bearing capacity (p) with the high A value of 30 for pure black ice
p_black_ha1 <- ((17.5*10^2)/2)*(1+((100-0)/100)) #3000kg

#Bearing capacity (p) with the low A value of 3.5 for pure black ice
p_black_la1 <- (3.5*10^2/2)*(1+((100-0)/100)) #350kg (more reasonable, but still high for a person)

#Modified Gold's equation for 100% black ice

#Revised equation to solve for thickness with the high A value
req_thickness_b_ice1 <- sqrt((2/3.5)*(350/(1+((100-0)/100)))) #13cm

#Modified Gold's equation for 50% black ice and 50% white ice
req_thickness_h_ice1 <- sqrt((2/3.5)*(350/(1+((100-50)/100)))) #15cm

#Modified Gold's equation for 100% white ice
req_thickness_w_ice1 <- sqrt((2/3.5)*(350/(1+((100-100)/100)))) #18.4cm

###########################

#What is the bearing capacity for 4 people walking?
#NOTE: Doubling the capacity from above, 
#because 350kg is much more than 1 person's weight

#Modified Gold's equation for 100% black ice

#Revised equation to solve for thickness with the high A value
req_thickness_b_ice2 <- sqrt((2/3.5)*(700/(1+((100-0)/100)))) #14.1cm

#Modified Gold's equation for 50% black ice and 50% white ice
req_thickness_h_ice2 <- sqrt((2/3.5)*(700/(1+((100-50)/100)))) #16.3cm

#Modified Gold's equation for 100% white ice
req_thickness_w_ice2 <- sqrt((2/3.5)*(700/(1+((100-100)/100)))) #20cm


###########################

#What is the bearing capacity for a snowmobile without a rider?

#Bearing capacity (p) with the high A value of 30 for pure black ice
p_black_ha2 <- ((17.5*13^2)/2)*(1+((100-0)/100)) #5070kg

#Bearing capacity (p) with the low A value of 3.5 for pure black ice
p_black_la2 <- ((3.5*13^2)/2)*(1+((100-0)/100)) #591.5kg (seems more reasonable for a snowmobile. On the low end of snowmobile weight)

#Modified Gold's equation for 100% black ice

#Revised equation to solve for thickness with the high A value
req_thickness_b_ice3 <- sqrt((2/3.5)*(591.5/(1+((100-0)/100)))) #13cm

#Modified Gold's equation for 50% black ice and 50% white ice
req_thickness_h_ice3 <- sqrt((2/3.5)*(591.5/(1+((100-50)/100)))) #15cm

#Modified Gold's equation for 100% white ice
req_thickness_w_ice3 <- sqrt((2/3.5)*(591.5/(1+((100-100)/100)))) #18.4cm

########################

#Snowmobile with a rider: weight used - 950kg

#Revised equation to solve for thickness with the high A value
req_thickness_b_ice4 <- sqrt((2/3.5)*(950/(1+((100-0)/100)))) #16.5cm

#Modified Gold's equation for 50% black ice and 50% white ice
req_thickness_h_ice4 <- sqrt((2/3.5)*(950/(1+((100-50)/100)))) #19cm

#Modified Gold's equation for 100% white ice
req_thickness_w_ice4 <- sqrt((2/3.5)*(950/(1+((100-100)/100)))) #23.3cm

########################

#Snowmobile with a rider: weight used - 950kg (offers some wiggle room for equipment)

#Revised equation to solve for thickness with the high A value
req_thickness_b_ice5 <- sqrt((2/3.5)*(1900/(1+((100-0)/100)))) #23.3cm

#Modified Gold's equation for 50% black ice and 50% white ice
req_thickness_h_ice5 <- sqrt((2/3.5)*(1900/(1+((100-50)/100)))) #26.9cm

#Modified Gold's equation for 100% white ice
req_thickness_w_ice5 <- sqrt((2/3.5)*(1900/(1+((100-100)/100)))) #33cm

########################

#Snowmobile with a rider + big game (grey seal - 400kg (male weight)): weight used - 1350kg (offers some wiggle room for equipment)

#Revised equation to solve for thickness with the high A value
req_thickness_b_ice5 <- sqrt((2/3.5)*(1350/(1+((100-0)/100)))) #19.6cm

#Modified Gold's equation for 50% black ice and 50% white ice
req_thickness_h_ice5 <- sqrt((2/3.5)*(1350/(1+((100-50)/100)))) #22.7cm

#Modified Gold's equation for 100% white ice
req_thickness_w_ice5 <- sqrt((2/3.5)*(1350/(1+((100-100)/100)))) #27.8cm

#######################

#2 Snowmobiles with two riders + 2 grey seals - 400kg (male weight)): weight used - 2700kg (offers some wiggle room for equipment)

#Revised equation to solve for thickness with the high A value
req_thickness_b_ice6 <- sqrt((2/3.5)*(2700/(1+((100-0)/100)))) #27.8cm

#Modified Gold's equation for 50% black ice and 50% white ice
req_thickness_h_ice6 <- sqrt((2/3.5)*(2700/(1+((100-50)/100)))) #32.1cm

#Modified Gold's equation for 100% white ice
req_thickness_w_ice6 <- sqrt((2/3.5)*(2700/(1+((100-100)/100)))) #39.3cm





req_thickness_w_ice_test <- sqrt((2/3.5)*(100/(1+((100-100)/100)))) #33cm

