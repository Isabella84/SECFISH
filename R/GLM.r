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


# GLM modelling



GLM<- function(COSTS,thr_obs,thr_cum,FORMULA_LAB1=crewcost~factor(Met_LOA)+Rev_minus_Tot_var_costs+0,FORMULA_LAB2=crewcost~factor(Met_LOA)*Rev_minus_Tot_var_costs+0,path=tempdir()) {


  oldpar <- par('mfrow')
  on.exit(par(oldpar))



  dir.create(file.path(path,"GLMs"))
  dir.create(file.path(path,"GLMs","Energy_costs"))
  dir.create(file.path(path,"GLMs","Other_variable_costs"))
  dir.create(file.path(path,"GLMs","Labour_costs"))
  dir.create(file.path(path,"GLMs","Maintenance_costs"))


COSTS$Rev_minus_Tot_var_costs=COSTS$Sum_revenue-rowSums(COSTS[,c(5,6,8,9)])
LAND4=COSTS

fs=unique(COSTS$FS)

for (flee in fs){

sink(file=file.path(path,"GLMs","Energy_costs",paste("Fuel_cost",flee,".txt",sep="")))
message(flee)
COSTS_temp=COSTS[COSTS$FS==flee & !is.na(COSTS$fuelcost) & !is.na(COSTS$fuelcons),]

met1=aggregate(COSTS_temp$vessel_ID,by=list(COSTS_temp$Met_LOA),FUN="length")
met1$per=met1[,2]/sum(met1[,2])
met1=met1[order(met1$per,decreasing=T),]
met1$cum[1]=met1$per[1]

if (nrow(met1)>=2){
for (ii in 2:nrow(met1)){
met1$cum[ii]=met1$cum[ii-1]+met1$per[ii]
}
}

if (met1$cum[1]>thr_cum){
metier=(met1[1,1])} else {
buffer=0.05
metier=(met1[which(met1[,4]<=thr_cum+buffer*thr_cum),1])
}

if((sum(met1[,2])>=thr_obs) & (length(metier)>0)){


COSTS_temp=COSTS_temp[COSTS_temp$Met_LOA %in% metier,]

write.table(COSTS_temp,file.path(path,"GLMs","Energy_costs",paste("Dataset_",flee,".csv",sep="")),sep=";",row.names=F)


# Energy_costs


if (length(unique(COSTS_temp$Met_LOA))>=2){

  message(met1)

  if (class(try(glm(fuelcost~factor(Met_LOA)+Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
mod1 <- glm(fuelcost~factor(Met_LOA)+Effort+0, data=COSTS_temp,family=gaussian())
jpeg(file.path(path,"GLMs","Energy_costs",paste("Energy_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
par(mfrow=c(2,2))
plot(mod1)
dev.off()
message(summary(mod1))
} # close the IF checking if the fitting multiplicative converged

  if (class(try(glm(fuelcost~factor(Met_LOA)*Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
mod2 <- glm(fuelcost~factor(Met_LOA)*Effort+0, data=COSTS_temp,family=gaussian())
jpeg(file.path(path,"GLMs","Energy_costs",paste("Energy_costs_Gaussian_mod2_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
par(mfrow=c(2,2))
plot(mod2)
dev.off()
message(summary(mod2))
} # close the IF checking if the fitting multiplicative converged

  sink()

} else if (class(try(glm(fuelcost~Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
mod1 <- glm(fuelcost~Effort+0, data=COSTS_temp,family=gaussian())

jpeg(file.path(path,"GLMs","Energy_costs",paste("Energy_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
par(mfrow=c(2,2))
plot(mod1)
dev.off()

message(summary(mod1))
sink()
}



#FUEL CONSUMPTION
sink(file=file.path(path,"GLMs","Energy_costs",paste("Fuel_cons_",flee,".txt",sep="")))

if (length(unique(COSTS_temp$Met_LOA))>=2){

  message(met1)

  if (class(try(glm(fuelcons~factor(Met_LOA)+Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
    mod1 <- glm(fuelcons~factor(Met_LOA)+Effort+0, data=COSTS_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Energy_costs",paste("Fuel_cons_Energy_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
    par(mfrow=c(2,2))
    plot(mod1)
    dev.off()
    message(summary(mod1))
  } # close the IF checking if the fitting multiplicative converged

  if (class(try(glm(fuelcons~factor(Met_LOA)*Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
    mod2 <- glm(fuelcons~factor(Met_LOA)*Effort+0, data=COSTS_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Energy_costs",paste("Fuel_cons_Energy_costs_Gaussian_mod2_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
    par(mfrow=c(2,2))
    plot(mod2)
    dev.off()
        message(summary(mod2))
  } # close the IF checking if the fitting multiplicative converged



} else if (class(try(glm(fuelcons~Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
  mod1 <- glm(fuelcons~Effort+0, data=COSTS_temp,family=gaussian())

  jpeg(file.path(path,"GLMs","Energy_costs",paste("Fuel_cons_Energy_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
  par(mfrow=c(2,2))
  plot(mod1)
  dev.off()


  message(summary(mod1))

}
} else {
  message(paste("this fleet segment has less than",thr_obs," observations"))
  }
 sink()

}


# Other_variable_costs
for (flee in fs){
  sink(file=file.path(path,"GLMs","Other_variable_costs",paste(flee,".txt",sep="")))

message(flee)
COSTS_temp=COSTS[COSTS$FS==flee & !is.na(COSTS$othvarcost),]
met1=aggregate(COSTS_temp$vessel_ID,by=list(COSTS_temp$Met_LOA),FUN="length")
met1$per=met1[,2]/sum(met1[,2])
met1=met1[order(met1$per,decreasing=T),]
met1$cum[1]=met1$per[1]

if (nrow(met1)>=2){
for (ii in 2:nrow(met1)){
met1$cum[ii]=met1$cum[ii-1]+met1$per[ii]
}
}

if (met1$cum[1]>thr_cum){
metier=(met1[1,1])} else {
buffer=0.05
metier=(met1[which(met1[,4]<=thr_cum+buffer*thr_cum),1])

}



if ((sum(met1[,2])>=thr_obs) & length(metier)>0){



COSTS_temp=COSTS_temp[COSTS_temp$Met_LOA %in% metier,]
write.table(COSTS_temp,file.path(path,"GLMs","Other_variable_costs",paste("Dataset_",flee,".csv",sep="")),sep=";",row.names=F)

if (length(unique(COSTS_temp$Met_LOA))>=2){


  message(met1)

  if (class(try(glm(othvarcost~factor(Met_LOA)+Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
    mod1 <- glm(othvarcost~factor(Met_LOA)+Effort+0, data=COSTS_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Other_variable_costs",paste("Other_variable_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
    par(mfrow=c(2,2))
    plot(mod1)
    dev.off()
    message(summary(mod1))
  } # close the IF checking if the fitting multiplicative converged

  if (class(try(glm(othvarcost~factor(Met_LOA)*Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
    mod2 <- glm(othvarcost~factor(Met_LOA)*Effort+0, data=COSTS_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Other_variable_costs",paste("Other_variable_costs_Gaussian_mod2_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
    par(mfrow=c(2,2))
    plot(mod2)
    dev.off()

    message(summary(mod2))
  } # close the IF checking if the fitting multiplicative converged


} else if (class(try(glm(othvarcost~Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
  mod1 <- glm(othvarcost~Effort+0, data=COSTS_temp,family=gaussian())

  jpeg(file.path(path,"GLMs","Other_variable_costs",paste("Other variable_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
  par(mfrow=c(2,2))
  plot(mod1)
  dev.off()

  message(summary(mod1))


}

}else {
  message(paste("this fleet segment has less than",thr_obs," observations"))
  }
 sink()
 }



# Labour_costs

for (flee in fs){
  message(flee)
LAND4_temp=LAND4[LAND4$FS==flee & !is.na(LAND4$crewcost),]
met1=aggregate(LAND4_temp$vessel_ID,by=list(LAND4_temp$Met_LOA),FUN="length")
met1$per=met1[,2]/sum(met1[,2])
met1=met1[order(met1$per,decreasing=T),]
met1$cum[1]=met1$per[1]

if (nrow(met1)>=2){
for (ii in 2:nrow(met1)){
met1$cum[ii]=met1$cum[ii-1]+met1$per[ii]
}
}

if (met1$cum[1]>thr_cum){
metier=(met1[1,1])} else {
  buffer=0.05
  metier=(met1[which(met1[,4]<=thr_cum+buffer*thr_cum),1])
}

sink(file=file.path(path,"GLMs","Labour_costs",paste(flee,".txt",sep="")))

if ((sum(met1[,2])>=thr_obs) & length(metier)>0){


LAND4_temp=LAND4_temp[LAND4_temp$Met_LOA %in% metier,]
write.table(LAND4_temp,file.path(path,"GLMs","Labour_costs",paste("Dataset_",flee,".csv")),sep=";",row.names=F)

message(met1)

if (length(unique(LAND4_temp$Met_LOA))>=2){

if (class(try(glm(FORMULA_LAB1, data=LAND4_temp,family=gaussian())))[1]!="try-error"){
  mod1 <- glm(FORMULA_LAB1, data=LAND4_temp,family=gaussian())
  jpeg(file.path(path,"GLMs","Labour_costs",paste("Labour_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
  par(mfrow=c(2,2))
  plot(mod1)
  dev.off()
  message(summary(mod1))
} # close the IF checking if the fitting multiplicative converged

  if (class(try(glm(FORMULA_LAB2, data=LAND4_temp,family=gaussian())))[1]!="try-error"){
    mod2 <- glm(FORMULA_LAB2, data=LAND4_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Labour_costs",paste("Labour_costs_Gaussian_mod2_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
    par(mfrow=c(2,2))
    plot(mod2)
    dev.off()
    message(summary(mod2))
  } # close the IF checking if the fitting multiplicative converged



} else {
  if (class(try(glm(crewcost~Rev_minus_Tot_var_costs+0, data=LAND4_temp,family=gaussian())))[1]!="try-error"){
 mod1 <- glm(crewcost~Rev_minus_Tot_var_costs+0, data=LAND4_temp,family=gaussian())
jpeg(file.path(path,"GLMs","Labour_costs",paste("Labour_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
par(mfrow=c(2,2))
plot(mod1)
dev.off()
message(summary(mod1))
  }

  if (class(try(glm(crewcost~Sum_revenue+0, data=LAND4_temp,family=gaussian())))[1]!="try-error"){
    mod2 <- glm(crewcost~Sum_revenue+0, data=LAND4_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Labour_costs",paste("Labour_costs_Gaussian_mod2_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
    par(mfrow=c(2,2))
    plot(mod2)
    dev.off()
    message(summary(mod2))
  }

  if (class(try(glm(crewcost~Effort+0, data=LAND4_temp,family=gaussian())))[1]!="try-error"){
    mod3 <- glm(crewcost~Effort+0, data=LAND4_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Labour_costs",paste("Labour_costs_Gaussian_mod3_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
    par(mfrow=c(2,2))
    plot(mod3)
    dev.off()
    message(summary(mod3))
  }

  if (class(try(glm(crewcost~Rev_minus_fuel+0, data=LAND4_temp,family=gaussian())))[1]!="try-error"){
    mod4 <- glm(crewcost~Rev_minus_fuel+0, data=LAND4_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Labour_costs",paste("Labour_costs_Gaussian_mod4_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=300)
    par(mfrow=c(2,2))
    plot(mod4)
    dev.off()
    message(summary(mod4))
  }

}

}else {
  message(paste("this fleet segment has less than",thr_obs," observations"))
  }
 sink()
 }




# REPAIR AND Maintenance_costs

for (flee in fs){
message(flee)
COSTS_temp=COSTS[COSTS$FS==flee & !is.na(COSTS$repmaint),]
met1=aggregate(COSTS_temp$vessel_ID,by=list(COSTS_temp$Met_LOA),FUN="length")
met1$per=met1[,2]/sum(met1[,2])
met1=met1[order(met1$per,decreasing=T),]
met1$cum[1]=met1$per[1]

if (nrow(met1)>=2){
for (ii in 2:nrow(met1)){
met1$cum[ii]=met1$cum[ii-1]+met1$per[ii]
}
}

if (met1$cum[1]>thr_cum){
metier=(met1[1,1])} else {buffer=0.05
metier=(met1[which(met1[,4]<=thr_cum+buffer*thr_cum),1])

}

sink(file=file.path(path,"GLMs","Maintenance_costs",paste(flee,".txt",sep="")))

if ((sum(met1[,2])>=thr_obs) & length(metier)>0){


COSTS_temp=COSTS_temp[COSTS_temp$Met_LOA %in% metier,]
write.table(COSTS_temp,file.path(path,"GLMs","Maintenance_costs",paste("Dataset_",flee,".csv",sep="")),sep=";",row.names=F)


if (length(unique(COSTS_temp$Met_LOA))>=2){


  message(met1)

  if (class(try(glm(repmaint~factor(Met_LOA)+Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
    mod1 <- glm(repmaint~factor(Met_LOA)+Effort+0, data=COSTS_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Maintenance_costs",paste("Maintenance_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=100)
    par(mfrow=c(2,2))
    plot(mod1)
    dev.off()
    message(summary(mod1))
  } # close the IF checking if the fitting multiplicative converged

  if (class(try(glm(repmaint~factor(Met_LOA)*Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
    mod2 <- glm(repmaint~factor(Met_LOA)*Effort+0, data=COSTS_temp,family=gaussian())
    jpeg(file.path(path,"GLMs","Maintenance_costs",paste("Maintenance_costs_Gaussian_mod2_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=100)
    par(mfrow=c(2,2))
    plot(mod2)
    dev.off()
    message(summary(mod2))
  } # close the IF checking if the fitting multiplicative converged


} else if (class(try(glm(repmaint~Effort+0, data=COSTS_temp,family=gaussian())))[1]!="try-error"){
  mod1 <- glm(repmaint~Effort+0, data=COSTS_temp,family=gaussian())

  jpeg(file.path(path,"GLMs","Maintenance_costs",paste("Maintenance_costs_Gaussian_mod1_",flee,".jpg",sep="")),height=20,width=20,units="cm",res=100)
  par(mfrow=c(2,2))
  plot(mod1)
  dev.off()
  summary(mod1)

  message(summary(mod1))

}

}else {
  message(paste("this fleet segment has less than",thr_obs," observations"))
  }
sink()
}

unlink(file.path(tempdir(),"GLMs"),recursive=T)
unlink(file.path(tempdir(),"GLMs","Energy_costs"),recursive=T)
unlink(file.path(tempdir(),"GLMs","Other_variable_costs"),recursive=T)
unlink(file.path(tempdir(),"GLMs","Labour_costs"),recursive=T)
unlink(file.path(tempdir(),"GLMs","Maintenance_costs"),recursive=T)


}
