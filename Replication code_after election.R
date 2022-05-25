setwd("D:/Makhare Files/Makhare/Blogs/2022_ Attitudes toward CEC")
library(haven)
library(tidyverse)
library(margins)
library(plotly)
library(survey)
library(ggplot2)
library(questionr)
library(ggpubr)
library(png)
library(writexl)
library(xlsx)
library(dplyr)
library(MASS)
library(sjPlot)
library(ggeffects)
library(questionr)
library(stats)
library(wCorr)
library(EdSurvey)


ndi21<-read_dta("D:/Makhare Files/Makhare/Blogs/2022_ Attitudes toward CEC/NDI_December_2021_NDI_December_2021_17.12.2021_Public.dta")
names(ndi21)


#for time series
ndi14_apr<-read_dta("./NDI_2014_April_30.04.14_Public.dta")
ndi14_aug<-read_dta("./NDI_2014_August_22.08.14.dta")
ndi15<-read_dta("./NDI_2015_April_01.05.15_Public.dta")
ndi16<-read_dta("./NDI_2016_March_31.03.16_Public.dta")
ndi20<-read_dta("./NDI_2020_Dec_29.12.20_Public.dta")

##ndi14_april
table(ndi14_apr$PERFCEC)
ndi14_apr$PERFCEC_new_14ap<-ndi14_apr$PERFCEC
ndi14_apr$PERFCEC_new_14ap[ndi14_apr$PERFCEC_new_14ap<=-3]<-NA
ndi14_apr$PERFCEC_new_14ap[ndi14_apr$PERFCEC_new_14ap==-2]<--1
ndi14_apr$PERFCEC_new_14ap[ndi14_apr$PERFCEC_new_14ap==2]<-1
ndi14_apr$PERFCEC_new_14ap[ndi14_apr$PERFCEC_new_14ap==3]<-2
ndi14_apr$PERFCEC_new_14ap[ndi14_apr$PERFCEC_new_14ap>3]<-3
ndi14_apr$PERFCEC_new_14ap<-factor(ndi14_apr$PERFCEC_new_14ap, levels = c(-1,1,2,3), labels = c("DK/RA", "Badly", "Average", "Well"))
table(ndi14_apr$PERFCEC_new_14ap)
ndi14_apr_per<-round(prop.table(xtabs(WTIND~PERFCEC_new_14ap, data=ndi14_apr, na.action = "na.omit"))*100, 0)

##ndi14_august
table(ndi14_aug$q51_32)
ndi14_aug$PERFCEC_new_14ag<-ndi14_aug$q51_32
ndi14_aug$PERFCEC_new_14ag[ndi14_aug$PERFCEC_new_14ag<=-3]<-NA
ndi14_aug$PERFCEC_new_14ag[ndi14_aug$PERFCEC_new_14ag==-2]<--1
ndi14_aug$PERFCEC_new_14ag[ndi14_aug$PERFCEC_new_14ag==2]<-1
ndi14_aug$PERFCEC_new_14ag[ndi14_aug$PERFCEC_new_14ag==3]<-2
ndi14_aug$PERFCEC_new_14ag[ndi14_aug$PERFCEC_new_14ag>3]<-3
ndi14_aug$PERFCEC_new_14ag<-factor(ndi14_aug$PERFCEC_new_14ag, levels = c(-1,1,2,3), labels = c("DK/RA", "Badly", "Average", "Well"))
table(ndi14_aug$PERFCEC_new_14ag)
ndi14_aug_per<-round(prop.table(xtabs(indwt~PERFCEC_new_14ag, data=ndi14_aug, na.action = "na.omit"))*100, 0)

##ndi15
table(ndi15$PERFCEC)
ndi15$PERFCEC_new_15<-ndi15$PERFCEC
ndi15$PERFCEC_new_15[ndi15$PERFCEC_new_15<=-3]<-NA
ndi15$PERFCEC_new_15[ndi15$PERFCEC_new_15==-2]<--1
ndi15$PERFCEC_new_15[ndi15$PERFCEC_new_15==2]<-1
ndi15$PERFCEC_new_15[ndi15$PERFCEC_new_15==3]<-2
ndi15$PERFCEC_new_15[ndi15$PERFCEC_new_15>3]<-3
ndi15$PERFCEC_new_15<-factor(ndi15$PERFCEC_new_15, levels = c(-1,1,2,3), labels = c("DK/RA", "Badly", "Average", "Well"))
table(ndi15$PERFCEC_new_15)
ndi15per<-round(prop.table(xtabs(WTIND~PERFCEC_new_15, data=ndi15, na.action = "na.omit"))*100, 0)


##ndi16
table(ndi16$PERFCEC)
ndi16$PERFCEC_new_16<-ndi16$PERFCEC
ndi16$PERFCEC_new_16[ndi16$PERFCEC_new_16<=-3]<-NA
ndi16$PERFCEC_new_16[ndi16$PERFCEC_new_16==-2]<--1
ndi16$PERFCEC_new_16[ndi16$PERFCEC_new_16==2]<-1
ndi16$PERFCEC_new_16[ndi16$PERFCEC_new_16==3]<-2
ndi16$PERFCEC_new_16[ndi16$PERFCEC_new_16>3]<-3
ndi16$PERFCEC_new_16<-factor(ndi16$PERFCEC_new_16, levels = c(-1,1,2,3), labels = c("DK/RA", "Badly", "Average", "Well"))
table(ndi16$PERFCEC_new_16)
ndi16per<-round(prop.table(xtabs(WTIND~PERFCEC_new_16, data=ndi16, na.action = "na.omit"))*100, 0)


##ndi20
table(ndi20$PERFCEC)
ndi20$PERFCEC_new_20<-ndi20$PERFCEC
ndi20$PERFCEC_new_20[ndi20$PERFCEC_new_20<=-3]<-NA
ndi20$PERFCEC_new_20[ndi20$PERFCEC_new_20==-2]<--1
ndi20$PERFCEC_new_20[ndi20$PERFCEC_new_20==2]<-1
ndi20$PERFCEC_new_20[ndi20$PERFCEC_new_20==3]<-2
ndi20$PERFCEC_new_20[ndi20$PERFCEC_new_20>3]<-3
ndi20$PERFCEC_new_20<-factor(ndi20$PERFCEC_new_20, levels = c(-1,1,2,3), labels = c("DK/RA", "Badly", "Average", "Well"))
table(ndi20$PERFCEC_new_20)
ndi20per<-round(prop.table(xtabs(WTIND~PERFCEC_new_20, data=ndi20, na.action = "na.omit"))*100, 0)

##ndi21
table(ndi21$PERFCEC)
ndi21$PERFCEC_new_21<-ndi21$PERFCEC
ndi21$PERFCEC_new_21[ndi21$PERFCEC_new_21<=-3]<-NA
ndi21$PERFCEC_new_21[ndi21$PERFCEC_new_21==-2]<--1
ndi21$PERFCEC_new_21[ndi21$PERFCEC_new_21==2]<-1
ndi21$PERFCEC_new_21[ndi21$PERFCEC_new_21==3]<-2
ndi21$PERFCEC_new_21[ndi21$PERFCEC_new_21>3]<-3
ndi21$PERFCEC_new_21<-factor(ndi21$PERFCEC_new_21, levels = c(-1,1,2,3), labels = c("DK/RA", "Badly", "Average", "Well"))
table(ndi21$PERFCEC_new_21)
ndi21per<-round(prop.table(xtabs(WTIND~PERFCEC_new_21, data=ndi21, na.action = "na.omit"))*100, 0)


tabs<-data.frame(cbind(ndi14_apr_per, ndi14_aug_per, ndi15per, ndi16per, ndi20per, ndi21per))
openxlsx::write.xlsx(tabs, file = "timeseries.xlsx")





###___________________________________________________MODELING_________________________________________________________

###______________________________________________Transform independent variables________________________________________
#SETTYPE
table(ndi21$SETTYPE)
ndi21$SETTYPE_new<-ndi21$SETTYPE
ndi21$SETTYPE_new<-factor(ndi21$SETTYPE_new, levels = c(1,2,3) , labels = c("Capital", "Urban", "Rural"))
table(ndi21$SETTYPE_new)


#sex
table(ndi21$RESPSEX)
ndi21$RESPSEX_new<-ndi21$RESPSEX
ndi21$RESPSEX_new<-factor(ndi21$RESPSEX_new, levels = c(1,2), labels = c("Male", "Female"))
prop.table(table(ndi21$RESPSEX_new))


##agegroup
table(ndi21$RESPAGE)
ndi21$AGEGROUP_new<-ndi21$AGEGROUP
ndi21$AGEGROUP_new<-factor(ndi21$AGEGROUP_new, levels = c(1,2,3), labels = c("18-34", "35-54", "55+"))
table(ndi21$AGEGROUP_new)


##Ethnic group
table(ndi21$ETHNIC)
ndi21$ETHNIC_new<-ndi21$ETHNIC
ndi21$ETHNIC_new[ndi21$ETHNIC_new>3]<-4
ndi21$ETHNIC_new<-factor(ndi21$ETHNIC_new, levels = c(1,2,3,4), labels = c("Armenian", "Azerbaijani", "Georgian", "Other"))
table(ndi21$ETHNIC_new)

#ethnic 2 recoded as Georgian and Non-Georgian
ndi21$ETHNIC_new_2<-ndi21$ETHNIC
ndi21$ETHNIC_new_2[ndi21$ETHNIC_new_2==2]<-1
ndi21$ETHNIC_new_2[ndi21$ETHNIC_new_2==3]<-2
ndi21$ETHNIC_new_2[ndi21$ETHNIC_new_2>3]<-1
ndi21$ETHNIC_new_2<-factor(ndi21$ETHNIC_new_2, levels = c(1,2), labels = c("Non-Georgian", "Georgian"))
table(ndi21$ETHNIC_new_2)


##Education
table(ndi21$RESPEDU)
ndi21$RESPEDU_new<-ndi21$RESPEDU
ndi21$RESPEDU_new[ndi21$RESPEDU_new<0]<-NA
ndi21$RESPEDU_new[ndi21$RESPEDU_new<4]<-1
ndi21$RESPEDU_new[ndi21$RESPEDU_new==4]<-2
ndi21$RESPEDU_new[ndi21$RESPEDU_new>4]<-3
ndi21$RESPEDU_new<-factor(ndi21$RESPEDU_new, levels = c(1,2,3), labels = c("Secondary or lower", "Technical", "Higher"))
table(ndi21$RESPEDU_new)


##party support
table(ndi21$PARTSUPP)
ndi21$PARTSUPP_new<-ndi21$PARTSUPP
ndi21$PARTSUPP_new[ndi21$PARTSUPP_new == -9]<-NA
ndi21$PARTSUPP_new[ndi21$PARTSUPP_new<1]<-0
ndi21$PARTSUPP_new[ndi21$PARTSUPP_new>1]<-2
ndi21$PARTSUPP_new<-factor(ndi21$PARTSUPP_new, levels = c(0,1,2), labels = c("No party/DK/RA", "Georgian Dream", "Opposition"))
table(ndi21$PARTSUPP_new)

table(ndi21$PARTSUPP)
ndi21$PARTSUPP_new_r<-ndi21$PARTSUPP
ndi21$PARTSUPP_new_r[ndi21$PARTSUPP_new_r==-5]<-0
ndi21$PARTSUPP_new_r[ndi21$PARTSUPP_new_r<0]<-NA
ndi21$PARTSUPP_new_r[ndi21$PARTSUPP_new_r>1]<-2
ndi21$PARTSUPP_new_r<-factor(ndi21$PARTSUPP_new_r, levels = c(0,1,2), labels = c("Supports no party", "Georgian Dream", "Opposition"))
table(ndi21$PARTSUPP_new_r)




##Wealth
table(ndi21$OWNAIRC)
table(ndi21$OWNCARS)
table(ndi21$OWNCHTG)
table(ndi21$OWNCOMP)
table(ndi21$OWNCOTV)
table(ndi21$OWNFRDG)
table(ndi21$OWNHWT)
table(ndi21$OWNSPHN)
table(ndi21$OWNTBLT)
table(ndi21$OWNWASH)


ndi21$OWNAIRC_n<-ndi21$OWNAIRC
ndi21$OWNAIRC_n[ndi21$OWNAIRC_n<=-1]<-NA
ndi21$OWNCARS_n<-ndi21$OWNCARS
ndi21$OWNCARS_n[ndi21$OWNCARS_n<=-1]<-NA
ndi21$OWNCHTG_n<-ndi21$OWNCHTG
ndi21$OWNCHTG_n[ndi21$OWNCHTG_n<=-1]<-NA
ndi21$OWNCOMP_n<-ndi21$OWNCOMP
ndi21$OWNCOMP_n[ndi21$OWNCOMP_n<=-1]<-NA
ndi21$OWNCOTV_n<-ndi21$OWNCOTV
ndi21$OWNCOTV_n[ndi21$OWNCOTV_n<=-1]<-NA
ndi21$OWNFRDG_n<-ndi21$OWNFRDG
ndi21$OWNFRDG_n[ndi21$OWNFRDG_n<=-1]<-NA
ndi21$OWNHWT_n<-ndi21$OWNHWT
ndi21$OWNHWT_n[ndi21$OWNHWT_n<=-1]<-NA
ndi21$OWNSPHN_n<-ndi21$OWNSPHN
ndi21$OWNSPHN_n[ndi21$OWNSPHN_n<=-1]<-NA
ndi21$OWNTBLT_n<-ndi21$OWNTBLT
ndi21$OWNTBLT_n[ndi21$OWNTBLT_n<=-1]<-NA
ndi21$OWNWASH_n<-ndi21$OWNWASH
ndi21$OWNWASH_n[ndi21$OWNWASH_n<=-1]<-NA

table(ndi21$OWNAIRC_n)
table(ndi21$OWNCARS_n)
table(ndi21$OWNCHTG_n)
table(ndi21$OWNCOMP_n)
table(ndi21$OWNCOTV_n)
table(ndi21$OWNFRDG_n)
table(ndi21$OWNHWT_n)
table(ndi21$OWNSPHN_n)
table(ndi21$OWNTBLT_n)
table(ndi21$OWNWASH_n)


ndi21$wealth<-(ndi21$OWNAIRC_n+
               ndi21$OWNCARS_n+
               ndi21$OWNCHTG_n+
               ndi21$OWNCOMP_n+
               ndi21$OWNCOTV_n+
               ndi21$OWNFRDG_n+
               ndi21$OWNHWT_n+
               ndi21$OWNSPHN_n+
               ndi21$OWNTBLT_n+
               ndi21$OWNWASH_n)
table(ndi21$wealth)
hist(ndi21$wealth)


## _______________________________________________________Transform Outcome variable_______________________________________ 
table(ndi21$PERFCEC)
ndi21$PERFCEC_new<-ndi21$PERFCEC
ndi21$PERFCEC_new[ndi21$PERFCEC_new<0]<-NA
ndi21$PERFCEC_new[ndi21$PERFCEC_new<3]<-1
ndi21$PERFCEC_new[ndi21$PERFCEC_new==3]<-2
ndi21$PERFCEC_new[ndi21$PERFCEC_new>3]<-3
ndi21$PERFCEC_new<-factor(ndi21$PERFCEC_new, levels = c(1,2,3), labels = c("Badly", "Average", "Well"))
table(ndi21$PERFCEC_new)
round(prop.table(xtabs(WTIND~PERFCEC_new, data=ndi21, na.action = "na.omit"))*100, 0)



## model
#_svyolr_ a ordinal logistic regression to check significance_______
ndi_svy<-svydesign(~1, weights = ndi21$WTIND, data = ndi21 )


mod_svyolr<-svyolr(PERFCEC_new~RESPEDU_new+
                        SETTYPE_new+
                        ETHNIC_new_2+
                        AGEGROUP_new+
                        RESPSEX_new+
                        PARTSUPP_new+
                        wealth, design = ndi_svy)
summary(mod_svyolr)


## OLR regression - POLR______For coefficients
ndi_svy_1<-ndi_svy$variables
ndi_svy_1$normalized<-(ndi_svy_1$WTIND/((sum(ndi_svy_1$WTIND)/length(ndi_svy_1$WTIND))))
mod_olr_estim<-polr(PERFCEC_new~SETTYPE_new+
                      RESPEDU_new+
                      ETHNIC_new_2+
                      AGEGROUP_new+
                      RESPSEX_new+
                      PARTSUPP_new+
                      wealth, weights= normalized, data = ndi_svy_1)

summary(mod_olr_estim)
##SETTYPE, ETHNICITY, Party support and Wealth matter


set_olr_tabs<-ggemmeans(mod_olr_estim, terms = c("SETTYPE_new"))
eth_olr_tabs<-ggemmeans(mod_olr_estim, terms = c("ETHNIC_new_2"))
part_olr_tabs<-ggemmeans(mod_olr_estim, terms = c("PARTSUPP_new"))
weal_olr_tabs<-ggemmeans(mod_olr_estim, terms = c("wealth"))

openxlsx::write.xlsx(set_olr_tabs, file = "settlement1.xlsx")
openxlsx::write.xlsx(eth_olr_tabs, file = "ethnic1.xlsx")
openxlsx::write.xlsx(part_olr_tabs, file = "party1.xlsx")
openxlsx::write.xlsx(weal_olr_tabs, file = "wealth1.xlsx")

print(ggemmeans(mod_olr_estim,"SETTYPE_new"))
print(ggemmeans(mod_olr_estim, "ETHNIC_new_2"))
print(ggemmeans(mod_olr_estim, "PARTSUPP_new_r"))
print(ggemmeans(mod_olr_estim, "wealth"))

##plots
set_olr_plot<-plot(ggemmeans(mod_olr_estim, terms = c("SETTYPE_new")))
eth_olr_plot<-plot(ggemmeans(mod_olr_estim, terms = c("ETHNIC_new_2")))
part_olr_plot<-plot(ggemmeans(mod_olr_estim, terms = c("PARTSUPP_new_r")))
weal_olr_plot<-plot(ggemmeans(mod_olr_estim, terms = c("wealth")))

ggarrange(set_olr_plot, eth_olr_plot, part_olr_plot, weal_olr_plot,
          ncol = 2, nrow = 2)
















