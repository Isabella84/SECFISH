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


# Comparison between the costs by fleet segment and the sum of the costs disaggregated by metier

Cons_check <- function(Costs_FS,Costs_MET,path=tempdir()) {

dir.create(file.path(path,"Consistency_checks"))

Costs_sum= aggregate(Costs_MET$value,by=list(Costs_MET$year, Costs_MET$Fleet_segment,Costs_MET$variable_name ),FUN="sum")
colnames(Costs_sum)=c("year","Fleet_segment","variable_name","Sum_costs_by_metier")

Merge=merge(Costs_sum,Costs_FS,by=c("year","Fleet_segment","variable_name") )[,c(1,2,3,4,7)]
colnames(Merge)=c("year","Fleet_segment","variable_name","Sum_costs_by_metier","Costs_by_fleet_segment")

Merge$DIFF= round((Merge$Sum_costs_by_metier -  Merge$Costs_by_fleet_segment)/ Merge$Costs_by_fleet_segment*100,1)

#print(Merge)

write.table(Merge,file.path(path,"Consistency_checks","Consistency_checks.csv"),sep=";",row.names=F)

unlink(file.path(tempdir(),"Consistency_checks"),recursive=T)

}

