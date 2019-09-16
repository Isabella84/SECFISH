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


EA<-function(Effort, Landings, Trip, OperID, Operations, Costs_vess,Capacity, thr,n_obs,Eff_option,path=tempdir()) {


vessel_ID <- Percentage <- NULL
Costs<-Costs_vess

dir.create(file.path(path,"Preliminary_checks"))
dir.create(file.path(path,"LinearRegressions"))
dir.create(file.path(path,"LinearRegressions","Energy_costs"))
dir.create(file.path(path,"LinearRegressions","Other_variable_costs"))
dir.create(file.path(path,"LinearRegressions","Labour costs"))
dir.create(file.path(path,"LinearRegressions","Maintenance_costs"))


Effort_lab=ifelse(Eff_option==1,"Hours_at_sea","Days_at_seaxKW")

# Merging tables
MAT1=merge(Effort,Trip,by="Trip_ID")               # association trip-vessel
MAT2=merge(MAT1,Capacity,by="vessel_ID")            # association vessel- its characteristics
MAT3=aggregate(MAT2$total_hours,by=list(MAT2$vessel_ID),FUN="sum",na.omit=T)         # sum of fishing hours carried out in the trips by vessel to associate a number of hours to each vessel
colnames(MAT3)= c("vessel_ID","Sum_total_hours")
MAT4=merge(MAT3,Costs,by="vessel_ID",all.y=T) # ONLY THE VESSELS WITH COSTS WILL BE MANTAINED
COSTS=merge(MAT4,Capacity,by="vessel_ID",all.x=T) # ONLY THE VESSELS WITH COSTS WILL BE MANTAINED
COSTS=COSTS[!is.na(COSTS$Sum_total_hours),]

mat1=merge(Operations,OperID,by="oper_ID",all.x=T)        #  ONLY THE OPERATIONS FOR WHICH THE METIER IS SPECIFIED  WILL BE RETAINED
mat2=merge(mat1,Trip,by="Trip_ID",all.x=T)                # association to the vessel id
METIER=merge(mat2,Capacity,by="vessel_ID",all.x=T)            # association of characteristics of the vessel


write.table(METIER,file.path(path,"Effort-Trip-Vessel.csv"),sep=";",row.names=F) # to check if there are NA in the hour fished.

MET=aggregate(METIER$hours_fished,by=list(as.character(METIER$vessel_ID),METIER$METIER),FUN="sum")
colnames(MET)=c("vessel_ID","METIER","Sum_hours_fished")
MET2=aggregate(MET$Sum_hours_fished,by=list(MET$vessel_ID),FUN="sum")
colnames(MET2)=c("vessel_ID","Sum_hours_fished")


LAND1=merge(Landings,Trip,by="Trip_ID")               # association trip-vessel
LAND2=merge(LAND1,Capacity,by="vessel_ID")          # association vessel- its characteristics
write.table(LAND2,file.path(path,"Revenues-vessel.csv"),sep=";",row.names=F) # to check if there are NA in the revenues.

if(nrow(LAND2[is.na(LAND2$revenue),])!=0){
LAND2[is.na(LAND2$revenue),]$revenue<-0
}

LAND3=aggregate(LAND2$revenue,by=list(LAND2$vessel_ID),FUN="sum")

colnames(LAND3)=c("vessel_ID","Sum_revenue")
LAND4=merge(LAND3,Costs,by="vessel_ID")

LAND4$Rev_minus_fuel = LAND4$Sum_revenue - LAND4$fuelcost

#Preliminary checks
#---------------------
neg_eff=Effort[Effort$total_hours<0,]
if(nrow(neg_eff)>0){
write.table(neg_eff,file.path(path,"Preliminary_checks","Negative_effort.csv"),sep=";",row.names=F)
  message(neg_eff)
}


no_trip=COSTS[is.na(COSTS$Sum_total_hours),]
if(nrow(no_trip)>0){
write.table(no_trip,file.path(path,"Preliminary_checks","No_trip_associated.csv"),sep=";",row.names=F)
  message(no_trip)
}


neg_rev_min_vaarcost=LAND4[LAND4$Rev_minus_fuel<=0 & !is.na(LAND4$Rev_minus_fuel),]
if(nrow(neg_rev_min_vaarcost)>0){
 message(neg_rev_min_vaarcost)
write.table(neg_rev_min_vaarcost,file.path(path,"Preliminary_checks","Negative_revenues_minus_fuel.csv"),sep=";",row.names=F)
}


COSTS$Fuel_price =   COSTS$fuelcost/ COSTS$fuelcons

jpeg(file.path(path,"Preliminary_checks","Fuel_price.jpg"),height=10,width=10,units="cm",res=300)
plot(COSTS$Fuel_price,ylab="Fuel price per litre",main="Fuel price")
write("Min.  1st Qu.   Median     Mean  3rd Qu.     Max.", file.path(path,"Preliminary_checks","Fuel_price_statistics.txt"))
write(round(summary(COSTS$Fuel_price),2),file.path(path,"Preliminary_checks","Fuel_price_statistics.txt"),append=T)
dev.off()

# A prevalent metier is associated to each vessel, according to the percentage of fishing hours.
# If the percentage is exactly 50%, the first metier in alphabethic order is associated to the vessel.

# Deriving the prevalent Metier for each vessel
#-----------------------------------------------
MET3=merge(MET,MET2,by="vessel_ID",all=T)
colnames(MET3)=c("vessel_ID","METIER","Sum_hours_fished_MET","Sum_hours_fished_TOT")


MET4=aggregate(METIER$oper_ID,by=list(METIER$vessel_ID,METIER$METIER),FUN="length")
colnames(MET4)=c("vessel_ID","METIER","Number_Operations")

MET6=merge(MET3,MET4,by=c("vessel_ID","METIER"),all.x=T)
MET6$percentage= MET6$Sum_hours_fished_MET/MET6$Sum_hours_fished_TOT*100
MET6=MET6[,c(1,2,5,6)]
colnames(MET6)=c("vessel_ID","METIER","Number_Operations","percentage")
write.table(MET6,file.path(path,"Preliminary_checks","VESSEL_METIER.csv"),sep=";",row.names=F)

vessels=unique(MET6$vessel_ID)

DF=data.frame(vessel_ID=vessels,prevalent_METIER=as.character(vessels),Percentage=rep(10,length(vessels)),nb_operations=rep(10,length(vessels)))
class(DF[,2])="character"

for (i in 1:length(vessels)){
DF_temp=MET6[MET6$vessel_ID==vessels[i],]
DF[i,2]=as.character(DF_temp[which(DF_temp$percentage==max(DF_temp$percentage)),]$METIER[1] )
DF[i,3]=round(as.numeric(as.character(DF_temp[which(DF_temp$percentage==max(DF_temp$percentage)),]$percentage[1])),1)
DF[i,4]=as.character(DF_temp[which(DF_temp$percentage==max(DF_temp$percentage)),]$Number_Operations[1])
}


if (Eff_option==1){
# Effort as Hours_sea
COSTS$Effort=COSTS$Sum_total_hours #/24*COSTS$kw
} else if (Eff_option==2) { # Effort as FD X KW
COSTS$Effort=COSTS$Sum_total_hours/24*COSTS$kw
}

COSTS=COSTS[!is.na(COSTS$Effort),]
COSTS$Prev_metier=rep("OTB",nrow(COSTS))

write("no fishing operation associated to:",file.path(path,"Preliminary_checks","No Fishing operations.txt"))
for (j in 1:nrow(COSTS)){
if (nrow(DF[as.character(DF$vessel_ID)==as.character(COSTS$vessel_ID[j]),])>0){
COSTS[j,]$Prev_metier=as.character(DF[as.character(DF$vessel_ID)==as.character(COSTS$vessel_ID[j]),]$prevalent_METIER )
} else {
write(as.character(COSTS$vessel_ID[j]),file.path(path,"Preliminary_checks","No Fishing operations.txt"),append=T)
warning(paste("Impossible to associate a prevalent metier to ",as.character(COSTS$vessel_ID[j]),": no fishing operation associated",sep=""),quote=F )

}
}
COSTS$FS=paste(COSTS$Tech,COSTS$VesLen,sep="_")
cost2=aggregate(COSTS$Effort,by=list(COSTS$Prev_metier,COSTS$VesLen,COSTS$FS),FUN="length")
colnames(cost2)=c("METIER","VesLen","FS","obs")

Capacity$FS=paste(Capacity$Tech,Capacity$VesLen,sep="_")

DF=merge(DF,Capacity,by=c("vessel_ID"))

ggplot(data=DF, aes(x=vessel_ID,y= Percentage)) + geom_point(stat="identity",colour = "blue", fill = "blue") + facet_grid(FS~prevalent_METIER)
ggsave(file.path(path,"Preliminary_checks","Prevalent_metier_percentage_by_vessel.jpg"),width = 20, height = 10, units = c("in"))

write.table(DF,file.path(path,"Preliminary_checks","Prevalent_METIER_overall.csv"),sep=";",row.names=F)

vessels=as.character(DF$vessel_ID[DF$Percentage>=thr])
write.table(DF,file.path(path,"Preliminary_checks","Prevalent_METIER_selected.csv"),sep=";",row.names=F)

#head(DF)
COSTS=merge(COSTS,DF[,c(1,3)],by="vessel_ID")


# EXPLORATION OF SIMPLE LINEAR CORRELATIONS
# Energy_costs

COSTS=COSTS[as.character(COSTS$vessel_ID) %in% vessels,]

COSTS$Met_LOA=paste(COSTS$Prev_metier,COSTS$VesLen)
COSTS$FS=paste(COSTS$Tech,COSTS$VesLen)
COSTS[,c(2:7)]<-COSTS$Percentage/100*COSTS[,c(2:7)]
#COSTS$Hours_sea=COSTS$Sum_total_hours
if (Eff_option==1){
# Effort as Hours_sea
COSTS$Effort=COSTS$Sum_total_hours #/24*COSTS$kw
} else if (Eff_option==2) { # Effort as FD X KW
COSTS$Effort=COSTS$Sum_total_hours/24*COSTS$kw
}

fs=unique(COSTS$FS)

for (flee in fs){
COSTS_temp=COSTS[COSTS$FS==flee & !is.na(COSTS$fuelcost),]
metier= unique(COSTS_temp$Prev_metier)


for (met in 1:length(metier)){

y= COSTS_temp$fuelcost[COSTS_temp$Prev_metier==metier[met]]
x= COSTS_temp$Effort[COSTS_temp$Prev_metier==metier[met]]
if (length(x)>n_obs) {
jpeg(file.path("LinearRegressions","Energy_costs",flee,"_metier_",met,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab=Effort_lab,ylab="Fuel_costs",main=paste(flee,"-",metier[met],sep=""))
modl=lm(y~x+0,data=data.frame(x=x,y=y))

if (length(x)>1) {
  abline(0,coefficients(modl)[1],col="blue")
}

corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]

P=round(rcorr(x,y)$P,3)[2,1]

if (!is.na(corre)){
if(corre==1){
corre=0.99
}
} else {
    corre=NA
  }


if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}

# Correlations whole dataset
y= COSTS_temp$fuelcost[COSTS_temp$Prev_metier %in% metier]
x= COSTS_temp$Effort[COSTS_temp$Prev_metier %in% metier]
if (length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Energy_costs",flee,"_whole_dataset.jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab=Effort_lab,ylab="Fuel_costs",main=flee)
modl=lm(y~x+0,data=data.frame(x=x,y=y))
if (length(x)>1) {
  abline(0,coefficients(modl)[1],col="blue")
}
corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}

}

# FUEL CONSUMPTION
for (flee in fs){
  COSTS_temp=COSTS[COSTS$FS==flee & !is.na(COSTS$fuelcons),]
  metier= unique(COSTS_temp$Prev_metier)


  for (met in 1:length(metier)){


    y= COSTS_temp$fuelcons[COSTS_temp$Prev_metier==metier[met]]
    x= COSTS_temp$Effort[COSTS_temp$Prev_metier==metier[met]]
    if (length(x)>n_obs) {
      jpeg(file.path(path,"LinearRegressions","Energy_costs","fuel_consumption_",flee,"_metier_",met,".jpg"),height=10,width=10,units="cm",res=300)
      plot(x,y,xlab=Effort_lab,ylab="Fuel_consumption",main=paste(flee,"-",metier[met],sep=""))
      modl=lm(y~x+0,data=data.frame(x=x,y=y))

      if (length(x)>1) {
        abline(0,coefficients(modl)[1],col="blue")
      }

      corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]

      P=round(rcorr(x,y)$P,3)[2,1]


      if (!is.na(corre)){
        if(corre==1){
          corre=0.99
        }
      } else {
        corre=NA
      }
      if(P==0){
        P="< 0.05"}
      text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
      dev.off()
    }
  }

  # Correlations whole dataset
  y= COSTS_temp$fuelcons[COSTS_temp$Prev_metier %in% metier]
  x= COSTS_temp$Effort[COSTS_temp$Prev_metier %in% metier]
  if (length(x)>n_obs) {
    jpeg(file.path(path,"LinearRegressions","Energy_costs",flee,"_fuel_consumption_whole_dataset.jpg"),height=10,width=10,units="cm",res=300)
    plot(x,y,xlab=Effort_lab,ylab="Fuel_consumption",main=flee)
    modl=lm(y~x+0,data=data.frame(x=x,y=y))
    if (length(x)>1) {
      abline(0,coefficients(modl)[1],col="blue")
    }
    corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
    P=round(rcorr(x,y)$P,3)[2,1]
    if (!is.na(corre)){
      if(corre==1){
        corre=0.99
      }
    } else {
      corre=NA
    }
    if(P==0){
      P="< 0.05"}
    text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
    dev.off()
  }

}


# Other_variable_costs

for (flee in fs){
COSTS_temp=COSTS[COSTS$FS==flee & !is.na(COSTS$othvarcost),]

metier=unique(COSTS_temp$Prev_metier)

for (met in 1:length(metier)){
y= COSTS_temp$othvarcost[COSTS_temp$Prev_metier==metier[met]]
x= COSTS_temp$Effort[COSTS_temp$Prev_metier==metier[met]]
if (length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Other_variable_costs",flee,"_metier_",met,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab=Effort_lab,ylab="Other_variable_costs",main=paste(flee,"-",metier[met],sep=""))
modl=lm(y~x+0,data=data.frame(x=x,y=y))

abline(0,coefficients(modl)[1],col="blue")

corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}



# Correlations whole dataset
y= COSTS_temp$othvarcost[COSTS_temp$Prev_metier %in% metier]
x= COSTS_temp$Effort[COSTS_temp$Prev_metier %in% metier]
if (length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Other_variable_costs",flee,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab=Effort_lab,ylab="Other_variable_costs",main=flee)
modl=lm(y~x+0,data=data.frame(x=x,y=y))
if (length(x)>1) {
  abline(0,coefficients(modl)[1],col="blue")
  }
corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}



# Labour_costs correlation with:
# Option 1: revenues minus total variable costs (fuelcost+repmaint + othvarcost)
# Option 2: revenues
# Option 3: FD*kw
# Option 4: revenues minus fuel costs

LAND4=LAND4[as.character(LAND4$vessel_ID) %in% vessels,]

LAND4$Prev_metier=rep("OTB",nrow(LAND4))
LAND4=merge(LAND4,Capacity,by="vessel_ID")
LAND4$Met_LOA=LAND4$Prev_metier
LAND4$Met_LOA=paste(LAND4$Prev_metier,LAND4$VesLen)
LAND4$FS=LAND4$Prev_metier
LAND4$FS=paste(LAND4$Tech,LAND4$VesLen)

for (j in 1:nrow(LAND4)){
LAND4[j,]$Met_LOA=as.character(COSTS[as.character(COSTS$vessel_ID)==as.character(LAND4$vessel_ID[j]),]$Met_LOA[1] )
LAND4[j,]$Prev_metier=as.character(COSTS[as.character(COSTS$vessel_ID)==as.character(LAND4$vessel_ID[j]),]$Prev_metier[1] )
}
LAND4=LAND4[!is.na(LAND4$Met_LOA) & !is.na(LAND4$Sum_revenue),]

fs=unique(LAND4$FS)

for (flee in fs){
LAND4_temp=LAND4[LAND4$FS==flee & !is.na(LAND4$crewcost),]

metier=unique(LAND4_temp$Prev_metier)

# Option 1: revenues minus total variable costs (fuelcost+repmaint + othvarcost)
for (met in 1:length(metier)){
y= LAND4_temp$crewcost[LAND4_temp$Prev_metier==metier[met]]
x= LAND4_temp$Rev_minus_Tot_var_costs[LAND4_temp$Prev_metier==metier[met]]
if (length(x)>n_obs){
jpeg(file.path(path,"LinearRegressions","Labour_costs","corr_rev_minus_tot_var_costs_fleet_",flee,"_metier_",met,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab="Rev minus tot var costs",ylab="Labour_costs",main=paste(flee,"-",metier[met],sep=""))
modl=lm(y~x+0,data=data.frame(x=x,y=y))
if (length(x)>1) {
  abline(0,coefficients(modl)[1],col="blue")
  }
corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}

# Correlations whole dataset
y= LAND4_temp$crewcost[LAND4_temp$Prev_metier %in% metier]
x= LAND4_temp$Rev_minus_Tot_var_costs[LAND4_temp$Prev_metier %in% metier]
if (length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Labour_costs","corr_revenue_minus_total_variable_costs_",flee,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab="Rev minus tot var costs",ylab="Labour_costs",main=flee)
modl=lm(y~x+0,data=data.frame(x=x,y=y))
abline(0,coefficients(modl)[1],col="blue")

corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}

# Option 2: Labour_costs correlation with revenues
for (flee in fs){
LAND4_temp=LAND4[LAND4$FS==flee & !is.na(LAND4$crewcost),]

metier=unique(LAND4_temp$Prev_metier)

for (met in 1:length(metier)){
y= LAND4_temp$crewcost[LAND4_temp$Prev_metier==metier[met]]
x= LAND4_temp$Sum_revenue[LAND4_temp$Prev_metier==metier[met]]

if (length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Labour_costs","corr_with_revenue_",flee,"_metier_",met,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab="Revenue",ylab="Labour_costs",main=paste(flee,"-",metier[met],sep=""))
modl=lm(y~x+0,data=data.frame(x=x,y=y))

abline(0,coefficients(modl)[1],col="blue")

corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}

# Correlations whole dataset
y= LAND4_temp$crewcost[LAND4_temp$Prev_metier %in% metier]
x= LAND4_temp$Sum_revenue[LAND4_temp$Prev_metier %in% metier]
if(length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Labour_costs","corr_with_revenue_",flee,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab="Sum_revenue",ylab="Labour_costs",main=flee)
modl=lm(y~x+0,data=data.frame(x=x,y=y))
if (length(x)>1) {
  abline(0,coefficients(modl)[1],col="blue")
  }
corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}

# Option 3: Labour_costs correlation with  Hours_sea
for (flee in fs){
COSTS_temp=COSTS[COSTS$FS==flee & !is.na(COSTS$crewcost),]

metier=unique(COSTS_temp$Prev_metier)


for (met in 1:length(metier)){
y= COSTS_temp$crewcost[COSTS_temp$Prev_metier==metier[met]]
x= COSTS_temp$Effort[COSTS_temp$Prev_metier==metier[met]]
if (length(x)>n_obs){
jpeg(file.path(path,"LinearRegressions","Labour_costs","corr_with_Hours_sea_",flee,"_metier_",met,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab=Effort_lab,ylab="Labour_costs",main=paste(flee, "-",metier[met]))
modl=lm(y~x+0,data=data.frame(x=x,y=y))
if (length(x)>1) {
  abline(0,coefficients(modl)[1],col="blue")
  }
corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}

# Correlations whole dataset
y= COSTS_temp$crewcost[COSTS_temp$Prev_metier %in% metier]
x= COSTS_temp$Effort[COSTS_temp$Prev_metier %in% metier]

if (length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Labour_costs","corr_with_Hours_sea_",flee,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab=Effort_lab,ylab="Labour_costs",main=flee)
modl=lm(y~x+0,data=data.frame(x=x,y=y))

abline(0,coefficients(modl)[1],col="blue")

corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}
# Option 4: Labour_costs versus Revenues minus fuel costs

for (flee in fs){
LAND4_temp=LAND4[LAND4$FS==flee & !is.na(LAND4$crewcost),]

metier=unique(LAND4_temp$Prev_metier)


for (met in 1:length(metier)){
y= LAND4_temp$crewcost[LAND4_temp$Prev_metier==metier[met]]
x= LAND4_temp$Sum_revenue[LAND4_temp$Prev_metier==metier[met]]-LAND4_temp$fuelcost[LAND4_temp$Prev_metier==metier[met]]

if (length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Labour_costs","corr_with_Revenue_minus_fuel_costs_",flee,"_metier_",met,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab="Revenue minus fuel costs",ylab="Labour_costs",main=paste(flee,"-",metier[met]))
modl=lm(y~x+0,data=data.frame(x=x,y=y))

abline(0,coefficients(modl)[1],col="blue")

corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}

# Correlations whole dataset
y= LAND4_temp$crewcost[LAND4_temp$Prev_metier %in% metier]
x= LAND4_temp$Sum_revenue[LAND4_temp$Prev_metier %in% metier]     -  LAND4_temp$fuelcost[LAND4_temp$Prev_metier %in% metier]
if(length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Labour_costs","corr_with_Revenue_minus_fuel_costs_",flee,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab="Revenue minus fuel costs",ylab="Labour_costs",main=flee)
modl=lm(y~x+0,data=data.frame(x=x,y=y))
abline(0,coefficients(modl)[1],col="blue")
corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}




for (flee in fs){
COSTS_temp=COSTS[COSTS$FS==flee & !is.na(COSTS$repmaint),]

metier=unique(COSTS_temp$Prev_metier)

# Maintenance_costs
for (met in 1:length(metier)){
y= COSTS_temp$repmaint[COSTS_temp$Prev_metier==metier[met]]
x= COSTS_temp$Effort[COSTS_temp$Prev_metier==metier[met]]
if(length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Maintenance_costs",flee,"_metier_",met,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab=Effort_lab,ylab="Maintenance_costs",main=paste(flee,"-",metier[met]))
modl=lm(y~x+0,data=data.frame(x=x,y=y))
abline(0,coefficients(modl)[1],col="blue")
corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}
if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}

# Correlations whole dataset
y= COSTS_temp$repmaint[COSTS_temp$Prev_metier %in% metier]
x= COSTS_temp$Effort[COSTS_temp$Prev_metier %in% metier]

if (length(x)>n_obs) {
jpeg(file.path(path,"LinearRegressions","Maintenance_costs",flee,".jpg"),height=10,width=10,units="cm",res=300)
plot(x,y,xlab=Effort_lab,ylab="Maintenance_costs",main=flee)
modl=lm(y~x+0,data=data.frame(x=x,y=y))

abline(0,coefficients(modl)[1],col="blue")

corre=round(cor(data.frame(fc=y,h_kw=x)),3)[2,1]
P=round(rcorr(x,y)$P,3)[2,1]
if (!is.na(corre)){
  if(corre==1){
    corre=0.99
  }
} else {
  corre=NA
}


if(P==0){
P="< 0.05"}
text(x=mean(x),y=0.9*max(y),paste("corr=",corre ,"\n Pvalue=",P,"\n slope=",round(coefficients(modl)[1],2),sep=""),col="red")
dev.off()
}
}

LAND5=merge(LAND4[,c(1,2,8)],COSTS,by="vessel_ID",all.y=T)

write.table(LAND5,file.path(path,"COSTS.csv"),sep=";",row.names=F)


unlink(file.path(tempdir(),"Preliminary_checks"),recursive=T)
unlink(file.path(tempdir(),"LinearRegressions"),recursive=T)
unlink(file.path(tempdir(),"LinearRegressions","Energy_costs"),recursive=T)
unlink(file.path(tempdir(),"LinearRegressions","Other_variable_costs"),recursive=T)
unlink(file.path(tempdir(),"LinearRegressions","Labour costs"),recursive=T)
unlink(file.path(tempdir(),"LinearRegressions","Maintenance_costs"),recursive=T)


}
