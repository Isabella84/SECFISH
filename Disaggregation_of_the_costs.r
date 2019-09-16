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


Disaggr <- function(Costs_or,key_table_or,Eff,path=tempdir()) {



TC=unique(as.character(Costs_or$variable_name))

for (ii in 1:length(TC)){
message(TC[ii], quote=F)
  key_table=key_table_or[key_table_or$Type_of_cost==TC[ii],]
  Costs=Costs_or[Costs_or$variable_name==TC[ii],]
  # check if the number of coefficients is correct

  fs=unique(key_table$Fleet_segment)
Costs_disag_total=Eff[1,]
Costs_disag_total$variable_name=Costs$variable_name[1]
Costs_disag_total$value=999
Costs_disag_total=Costs_disag_total[,c(1:5,8,9)]

Costs_disag_total1=Eff[1,]
Costs_disag_total1$variable_name=Costs$variable_name[1]
Costs_disag_total1$value=999
Costs_disag_total1=Costs_disag_total1[,c(1:5,8,9)]



  for (flee in 1:length(fs)){

    Costs_temp=Costs[Costs$ Fleet_segment==fs[flee],]
    Eff_temp=Eff[Eff$Fleet_segment==fs[flee],]
    key_table=key_table_or[key_table_or$Fleet_segment==fs[flee] & key_table_or$Type_of_cost==TC[ii],]
    # CHECKS
    YEARS_COSTS=unique(Costs_temp$year)
    YEARS_EFF= unique(Eff_temp$year)

    if(all(YEARS_COSTS)==all(YEARS_EFF))
    {
      message(fs[flee])
      message("All the years in Costs.csv file can be disaggregated",quote=F)
    } else {
      message(fs[flee])
      message("The years in Costs.csv and in Effort.csv do not match. Only the data of the years present in both files will be disaggregated.",quote=F)
    }


    option=key_table[key_table$Fleet_segment==fs[flee],]$Option[1]
    met1=unique(Eff[Eff$Fleet_segment==fs[flee],]$Metier)
    met_key=unique(key_table[key_table$Fleet_segment==fs[flee],])

    met_key=met_key[met_key$Explanatory_variable!="Effort",]$Explanatory_variable

    if((option == 1 | option == 2 ) & all(met1 %in% met_key)) {
      message(fs[flee],quote=F)
      message(paste("The disaggregation is possible with the provided data for",fs[flee]),quote=F)
    } else if (option == 3 & (length(met1)>=1)){
      message(fs[flee],quote=F)
      message(paste("The disaggregation is possible with the provided data for",fs[flee]),quote=F)
    } else {
      message(paste("The disaggregation is not possible with the provided data for",fs[flee]),". Please check if in the effort data all  years for each metier are provided.",quote=F)
    }




  Costs_disag= Eff_temp[-c(ncol(Eff_temp)-1,ncol(Eff_temp))]
  Costs_disag$variable_name=Costs$variable_name[1]
  Costs_disag$value=999

  #metier=unique(Effort$Metier)

  if (Costs$variable_name[1]!="labour_costs"){

    for (i in 1:nrow(Costs_disag)){

      if (key_table$Option[1]==1){

        Costs_disag$value[i]= as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable)==as.character(Costs_disag$Metier[i]) & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient)) + as.numeric(as.character(Eff_temp[i,]$Effort) ) *  as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable) =="Effort" & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient) )

      } else if (key_table$Option[1] ==2 & nrow(key_table[as.character(key_table$Explanatory_variable) ==paste("Effort*",Costs_disag$Metier[i],sep="") & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),])==1) {

        Costs_disag$value[i]= as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable)==as.character(Costs_disag$Metier[i])& as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient)) + as.numeric(as.character(Eff_temp[i,]$Effort)) *  as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable) =="Effort" & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient) ) +  as.numeric(as.character(Eff_temp[i,]$Effort)) *  as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable) ==paste("Effort*",Costs_disag$Metier[i],sep="") & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient))

      } else if ( (key_table$Option[1] ==2 & nrow(key_table[as.character(key_table$Explanatory_variable) ==paste("Effort*",Costs_disag$Metier[i],sep="") & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),])==0) ) {
        Costs_disag$value[i]= as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable)==as.character(Costs_disag$Metier[i]) & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient)) + as.numeric(as.character(Eff_temp[i,]$Effort)) *  as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable) =="Effort",]$Coefficient))
      } else if ( key_table$Option[1] ==3){
        Costs_disag$value[i]= Eff_temp[i,]$Effort *  key_table[as.character(key_table$Explanatory_variable) =="Effort" & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient
      }
    }
  } else if (Costs_temp$variable_name[1]=="labour_costs"){

    for (i in 1:nrow(Costs_disag)){

      if (key_table$Option[1]==1){

        Costs_disag$value[i]= as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable)==as.character(Costs_disag$Metier[i]) & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient)) + as.numeric(as.character(Eff_temp[i,]$Lab_expl_var)) *  as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable) =="Lab_expl_var" & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient))

      } else if (key_table$Option[1] ==2 & nrow(key_table[as.character(key_table$Explanatory_variable) ==paste("Lab_expl_var*",Costs_disag$Metier[i],sep="") & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),])==1) {

        Costs_disag$value[i]= as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable)==as.character(Costs_disag$Metier[i]) & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient)) + as.numeric(as.character(Eff_temp[i,]$Lab_expl_var)) *  as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable) =="Lab_expl_var" & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient))  +  as.numeric(as.character(Eff_temp[i,]$Lab_expl_var)) *  as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable) ==paste("Lab_expl_var*",Costs_disag$Metier[i],sep="") & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient) )

      } else if ( (key_table$Option[1] ==2 & nrow(key_table[as.character(key_table$Explanatory_variable) ==paste("Lab_expl_var*",Costs_disag$Metier[i],sep="") & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),])==0) ) {
        Costs_disag$value[i]= as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable)==as.character(Costs_disag$Metier[i]) & as.character(key_table$Fleet_segment)==as.character(Costs_disag$Fleet_segment[i]),]$Coefficient)) + as.numeric(as.character(Eff_temp[i,]$Lab_expl_var)) *  as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable) =="Lab_expl_var",]$Coefficient))
      } else if ( key_table$Option[1] ==3){
        Costs_disag$value[i]= as.numeric(as.character(Eff_temp[i,]$Lab_expl_var)) *  as.numeric(as.character(key_table[as.character(key_table$Explanatory_variable)=="Lab_expl_var" ]$Coefficient))
      }
    }


  }
  Costs_disag_total= rbind(Costs_disag_total,Costs_disag)


  }
            # "for" cycling on the fleet segment

  if(ii>=2){
  Costs_disag_total=rbind(bkp,Costs_disag_total[-1,])

  }  else {
  Costs_disag_total=rbind(Costs_disag_total[-1,])
  }
   bkp=Costs_disag_total
#


}  # close the type of costs "for"

write.table(Costs_disag_total,file.path(path, "Costs_disaggregated.csv"),sep=";",row.names=F)
 message("Disaggregation executed.",quote=F)

unlink(file.path(tempdir(),"Costs_disaggregated.csv"))


}
