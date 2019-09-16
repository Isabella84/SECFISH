#########################################################################################################
#   SECFISH (Strengthening regional cooperation in the area of fisheries data collection                #
#   -Socio-economic data collection for fisheries, aquaculture and the processing industry at EU level) #
#   Functions to identify correlations between costs and transversal variables by metier using          #
#   individual vessel data and for disaggregating variable costs from fleet segment to metier  level    #
#                                                                                                       #
#   Authors: Isabella Bitetto (COISPA), Loretta Malvarosa (NISEA), Maria Teresa Spedicato (COISPA),     #
#   Ralf Doering (THUENEN), Joerg Berkenhagen (THUENEN)                                                 #
#                                                                                                       #
#                                                                                                       #
#   In case of use, the Authors should be cited. If you have any comments or suggestions please         #
#   contact the following e-mail address: bitetto@coispa.it                                             #
#   SECFISH is believed to be reliable.                                                                 #
#   However, we disclaim any implied warranty.                                                          #
#                                                                                                       #
#   July 2019                                                                                           #
#########################################################################################################



Detect_outliers <- function(COSTS,Fleet_segment,formula) {

  #CO <- NULL




COSTS_temp=COSTS[COSTS$FS==Fleet_segment,]
mod1=glm(formula, data=COSTS_temp,family=gaussian())
# Detecting outliers
rstand<-rstandard(mod1)
plot(rstand, main="Standardized residuals")
abline(h=1.96)
abline(h=-1.96)
vessels<-COSTS_temp$vessel_ID # names(rjack)
names(rstand)=vessels
outliers=identify(1:length(rstand),rstand, vessels)
as.character(vessels[outliers])

rjack<-rstudent(mod1)
rjack
names(rjack)=vessels
plot(rjack, main="Jacknife Residuals")
abline(h=-1.96)
abline(h=1.96)
vessels<-names(rjack)

outliers=identify(1:length(rjack),rjack, vessels)
as.character(vessels[outliers])

}
