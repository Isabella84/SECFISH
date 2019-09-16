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



Constrained_regression <- function(COSTS, Fleet_segment, metier,type,path=tempdir()) {

#CO <- type <-met1 <- met2 <- NULL
met1<- metier[1]
met2 <- metier[2]


COSTS_TEMP=COSTS[COSTS$FS==Fleet_segment,]
Cost_FS=COSTS_TEMP[COSTS_TEMP$FS==Fleet_segment,which(colnames(COSTS_TEMP)==type)]  # put here the name of the variable you are interested in exploring



DF1=  data.frame(Cost= COSTS[COSTS$Prev_metier==met1,]$fuelcost,Activity=COSTS[COSTS$Prev_metier==met1,]$Effort)
DF2=data.frame(Cost= COSTS[COSTS$Prev_metier==met2,]$fuelcost,Activity=COSTS[COSTS$Prev_metier==met2,]$Effort)
DF=list(DF1,DF2)

a= mean(DF[[1]]$Cost/DF[[1]]$Activity)
b=  mean(DF[[2]]$Cost/DF[[2]]$Activity)

coeff=c(a,b)
DF[[1]]$Cost_est=coeff[1]*DF[[1]]$Activity
DF[[2]]$Cost_est=coeff[2]*DF[[2]]$Activity


minimizer<-function(coeff){
  DF[[1]]$Cost_est=coeff[1]*DF[[1]]$Activity
  DF[[2]]$Cost_est=coeff[2]*DF[[2]]$Activity
  GSum= sum(DF[[1]]$Cost_est)+sum(DF[[2]]$Cost_est)

  diff2=sum((GSum-Cost_FS)^2 )
  return(diff2)
}


opt=optim_sa(minimizer, start = c(a,b),lower=c(-1000,-1000),upper=c(1000,1000))

DF[[1]]$Cost_best=opt$par[1]*DF[[1]]$Activity
DF[[2]]$Cost_best=opt$par[2]*DF[[2]]$Activity


jpeg(file.path(path, "Figure1.jpg"),height=15,width=20,units="cm",res=300)
par(mai=c(0.5,0.5,0.5,0.5))                          #
plot(DF[[1]]$Activity,DF[[1]]$Cost,xlab="Fishing Activity",ylim=c(0,300000), ylab="Costs",main=met1,cex.lab=1.8,cex.main=1.8)
lines(DF[[1]]$Activity,DF[[1]]$Cost_best,col="red")
lines(DF[[1]]$Activity,DF[[1]]$Cost_est,col="blue")
legend("topleft", c(paste("slope(lm)=", round(a,2)),paste("slope(minimized)=",round(opt$par[1],2))), col = c("blue", "red"), lty = c(1,1),lwd=c(2,2), merge = TRUE, bg = "gray95")
dev.off()


jpeg(file.path(path, "Figure2.jpg"),height=15,width=20,units="cm",res=300)
par(mai=c(0.5,0.5,0.5,0.5))                          #
plot(DF[[2]]$Activity,DF[[2]]$Cost,xlab="Fishing Activity",ylim=c(0,300000), ylab="Costs",main=met2,cex.lab=1.8,cex.main=1.8)
lines(DF[[2]]$Activity,DF[[2]]$Cost_best,col="red")
lines(DF[[2]]$Activity,DF[[2]]$Cost_est,col="blue")
legend("topleft", c(paste("slope(lm)=", round(b,2)),paste("slope(minimized)=",round(opt$par[2],2))), col = c("blue", "red"), lty = c(1,1),lwd=c(2,2), merge = TRUE, bg = "gray95")
dev.off()


unlink(file.path(tempdir(),"Figure1.jpg"))
unlink(file.path(tempdir(),"Figure2.jpg"))

}
