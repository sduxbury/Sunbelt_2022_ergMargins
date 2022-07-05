#####################################################################
#     Begin by illustrating the problem of scaling in a simple p1 model
#       for a TRUE binary outcome
######################################################################

set.seed(21093)
  x1 = rnorm(1000)           # assign some continuous variables 
 x2 = rnorm(1000)
  z = 1 + 2*x1 + 3*x2        # linear combination with
pr = 1/(1+exp(-z))         # pass through an inv-logit function
 y = rbinom(1000,1,pr)      # bernoulli response variable
 
 #now feed it to glm:
df = data.frame(y=y,x1=x1,x2=x2)

##estimate one accurate model
glm( y~x1+x2,data=df,family="binomial")

#now compare to inaccruate model
glm( y~x1,data=df,family="binomial") ##because x1 and x2 are uncorrelated, the change in coefficients cannot be attributed to omitted confounding variable bias








##############################
# Get set up by installing dependencies
##############################

install.packages(c("ergMargins","statnet"))
library(statnet)
library(ergMargins)


##for this example, we'll use the faux mesa high data provided by statnet
data("faux.mesa.high")
faux.mesa.high




#######################
#   Effect size interpretation
#######################

###specify a simple model p1 model to allow for fast estimation

model1<-ergm(faux.mesa.high~edges+
                         nodecov("Grade")+
                         nodefactor("Race")+
                         nodefactor("Sex"))


summary(model1)


##now we'll calculate the AME
ergm.AME(model1, #ergMargins requires that we provide the model object
         var1="nodecov.Grade") #we also need to provide the name of the variable--NOTE that this needs to be the same of the variable as listed in the "summary" output
ergm.AME(model1,var1="nodefactor.Race.Hisp") #AME for hispanics
ergm.AME(model1,var1="nodefactor.Race.NatAm") #AME for native americans
ergm.AME(model1,var1="nodefactor.Race.Other") #AME for "other#
ergm.AME(model1,var1="nodefactor.Race.White") #AME for whites
ergm.AME(model1,var1="nodefactor.Sex.M") #AME for men


##ergMargins also lets you calculate the marginal effect at the MEAN, which can be computationally faster
  #Note that this method is only available for main effects and interactions; it does not apply to mediation analysis
ergm.MEM(model1, 
         var1="nodecov.Grade") 









#########################
#     Interpreting Interactions
##########################



###Include 2 homophily variables into the interaction
model2<-ergm(faux.mesa.high~edges+
               nodecov("Grade")+
               nodefactor("Race")+
               nodefactor("Sex")+
               nodematch("Sex")+
               absdiff("Grade"))


summary(model2)


###to interpret interactions, we again use ergm.AME but we include the "var2" and "inter" options to allow for moderation
ergm.AME(model2,
         var1="nodefactor.Sex.M", #var1 is the main effect variable
         var2="nodefactor.Sex.M", #var 2 is the moderating variable. In undirected networks, we specify the moderator and the main effect to be the same variable
         inter="nodematch.Sex")  #inter is the namem of the interaction

###interpreting output:
  #the AME when var2 = 0 is the effect of being male (compared to female) when the alter is female
  #the AME when var2 = 1 is the effect of being male (compared to female) when teh alter is male
  #The "second difference" is the *change* in the AME when the alter is male instead of female
    #the second differnece is your test of the interaction effect. If it is not significant, then there is no evidence that the interaction is significant


##for an absolute difference variable
ergm.AME(model2,var1="nodecov.Grade",var2="nodecov.Grade",inter="absdiff.Grade")

  ##interpreting results
  #The AME is the effect of a one-unit increase in Grade when alters grade is held at a specified level
  #The second difference is the *CHANGE* in AME when alters grade is increased by one level. Since there are multiple grade levels, we get unique values for each possible increase by one level
  #The "Aggregate output" is the AVERAGE second difference. It is often useful to sum over multiple continuous values when examining interactions with multiple levels
    #The aggregate output lets us state "on average, the effect of Grade increases by .001 when alters' grade is increased by one level."


##for some continuous interactions, the number of possible values will be too large to be reasonable.
  #in these cases, ergm.AME provides the "at.2" option to only examine specific levels. 
  #For example, we may want to know the effect of grade when alters' grade changes from the minimum (14) to the maximum (24)
  #we would use the code:
ergm.AME(model2,var1="nodecov.Grade",var2="nodecov.Grade",inter="absdiff.Grade",at.2 = c(14,24))
  #now we only get a comparison for the two values, simplifying our interpretation











########################################################
#   Estimating indirect effects in single mediator models
#######################################################


#now our goal is to test the hypothesis that triadic closure explains the effect of Race
model3<-ergm(faux.mesa.high~edges+
               nodecov("Grade")+
               nodefactor("Race")+
               nodefactor("Sex")+
               nodematch("Sex")+
               absdiff("Grade")+
               gwesp(.5,fixed=T),
             control=control.ergm(parallel = 4)) ##run model in parallel


summary(model3)


###test the change in coefficient with ergm.mma (short for ERGM marginal effects mediation analysis)
ergm.mma(restricted.model=model2, #the restricted model is the model WITHOUT the confounding/mediating variable
         full.model=model3,       #the full model is the model WITH the mediating variable
         direct.effect="nodefactor.Race.Hisp", #the direct effect is the varaible we want to compare
         mediator="gwesp.fixed.5")   #the mediator is the mediating vairable

#Interpreting results:
    #the Total effect is the AME for being Hispanic (black is referrent) before GWESP is controlled (e.g., the indirect effect + partial effect)
    #the Direct effect is the AME for being Hispanic after GWESP is controlled
    #the Indirect effect is the CHANGE in AME after GWESP is controlled.
    # the proprtion explained is how much of the total effect can be attributed to the mediating variable (GWESP)

  #from these results we could state:
    #On average, being Hispanic as compared to black is associated with .004 lower probability of 
      #forming friendships as a DIRECT result of being hispanic and is associated with an additional .006
      #lower probability of forming friendships as an INDIRECT result of reduced rates of triadic closure among hispanics.
      #This Indirect Effect explains .599 of the proportion of the total effect of being Hispanic.
      #In TOTAL, this means that, on average, Hispanics have .01 lower probability of forming friendships as compared to blacks.

##we'll do the same for the other race variables
  #native americans
ergm.mma(model2,model3, direct.effect = "nodefactor.Race.NatAm",mediator="gwesp.fixed.5")
  #"Other" race
ergm.mma(model2,model3, direct.effect = "nodefactor.Race.Other",mediator="gwesp.fixed.5")
  #Whites
ergm.mma(model2,model3, direct.effect = "nodefactor.Race.White",mediator="gwesp.fixed.5")






########################################################
#   Estimating indirect effects in moderated mediation and multiple mediator model
#######################################################

#a nice utility of ergMargins is that the procedures are identical if we are interested in groups of mediators
  #this means that we can use the same procedures to test indirect effects when the mediator or confounding variable is an interaction

  #say we want to know how much of the effect of race is explained by racial homophily

#we estimate the model
model4<-ergm(faux.mesa.high~edges+
               nodecov("Grade")+
               nodefactor("Race")+
               nodefactor("Sex")+
               nodematch("Sex")+
               nodematch("Race"))


summary(model4)

##and estimate the same mediation analysis comparing model 1 (no homophiy) and model 4
#Hispanics
ergm.mma(model1,model4, direct.effect = "nodefactor.Race.Hisp", mediator="nodematch.Race") 
ergm.mma(model1,model4, direct.effect = "nodefactor.Race.NatAm",mediator="nodematch.Race")
#"Other" race
ergm.mma(model1,model4, direct.effect = "nodefactor.Race.Other",mediator="nodematch.Race")
#Whites
ergm.mma(model1,model4, direct.effect = "nodefactor.Race.White",mediator="nodematch.Race")


##here we could interpret the resulting indirect effects as acting through racial homophily
  #by comparing the proportion explained, we see that the indirect effect of race acting through racial homophily is 
    #strongest for Hispanics and Native Americans, but weaker for whites and "other"








#######################################
#     Moderated Mediation
#######################################



##now we'll explore how to estimate the mediation analysis when the main effect is an interaction

  #the logic behind this procedure is to compute the second difference at each level of an interaction, 
    #then compare the second differences between models
    #for this reason, moderated mediation is the most computationally intensive


##here, we'll use the results from models 2 and 3 to evaluate the indirect effect of sex homophily acting through triadic closure

ergm.mod.mma(restricted.model=model2, #this provides the model without mediator
             full.model=model3, ##model with mediator
             var1="nodefactor.Sex.M", ##the interaction main effect
             var2="nodefactor.Sex.M", #the moderator (identical to the main effect in this case)
             inter="nodematch.Sex", #the interaction variable
             mediator="gwesp.fixed.5", #the mediating variable
             int.eff =TRUE) #this tells whether to report the mediated effect acting through all levels of interaction or only for the interaction effects. 
                              #When int.eff = TRUE, only the change in interaction effects are reported
                              #when joint=TRUE, the change in AME Is reported for each level of the interaction

##interpret results
  ##the PARTIAL effect panel tells us the AME at each level of the moderator in the full model
  ##The TOTAL effect panel tells us the aME at each level of the moderator in the restricted model
  #the PARTIAL second.diffs panel tells us the second difference in the full model--that is, the change in AME when alters' gender is changed from a mismatch to a match in the model that controls for GWESP
  #the TOTAL second.diffs. panel tells us the second difference in the restricted model--that is, the change in AME when alters' gender is changed from a mismatch to a match in the model that does NOT control for GWESP
  #The Third.diffs tests the change in the second difference between models. It is equal to the increase or decrease in second difference after controlling for GWESP
   
 #This is the quantity of interest. When the third diffs is negative, it means that the effect of an interaction declines after controlling for a confounder
    #our Third difference is nonsignificant. We can therefore conclude that there is no evidence that GWESP mediates the effect of sex homophily



##we can do the same with a continuous variable

ergm.mod.mma(restricted.model=model2, #this provides the model without mediator
             full.model=model3, ##model with mediator
             var1="nodecov.Grade", ##the interaction main effect
             var2="nodecov.Grade", #the moderator (identical to the main effect in this case)
             inter="absdiff.Grade", #the interaction variable
             mediator="gwesp.fixed.5", #the mediating variable
             int.eff=TRUE) #this tells whether to report the mediated effect acting through all levels of interaction or individually--it only affects output when the interaction has more than 2 distinct levels


##the interpretation for a continuous variable is the same as before, but now we get multiple third differences for each unit increase in moderator
  # can interpret each third difference independently. Sometimes it's easiest to visualize them
  # we can also use the "summary.output" panel to summarize the change in grade homophily effect.
  #the summary output is telling us that, on average, increasing alters' grade by one is associated with .0005 greater probability of a tie BECAUSE of its effect on triadic closure
    #in this particular case, we can see that the second difference is a nonlinear function: it gets stronger as it approaches ego's value and declines as it gets further away
    #to simplify the result, we could simply report the change when moving from the minimum to ego mean:

ergm.mod.mma(restricted.model=model2, 
             full.model=model3, 
             var1="nodecov.Grade", 
             var2="nodecov.Grade", 
             inter="absdiff.Grade", 
             mediator="gwesp.fixed.5", 
             at.2=c(14,17.46), ##17.46 is the mean for nodecov.Grade
             int.eff=TRUE) 

#Now we get the 2 value comparison. 
  #Interestingly, the third difference is extremely close to the mean third difference reported above.
  #we can conclude that moving alters' grade from 14 to be equal to ego's grade increases the probability of a tie by .0184 (total second diff)
  # Roughly 30.5% of this effect can be attributed to triadic closure (partial second diff/total second diff)
  #In fact, increasing alter grade from its minimum to ego's value increases the probability of a tie by .0129 because of the DIRECT effect of grade similarity
  # and by .0056 because of the INDIRECT effect of grade similarity on triadic closure
