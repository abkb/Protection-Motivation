setwd("Documents/Arizona/Collaboration & myone/Papers/LOS_EAUIPOS/Protection M theory")

# read file
library(foreign)
dataBG= read.csv("BGDATA_Cleaned_021722_v3.csv", header = T)

# PMT variables
library(dplyr)

# perceived severi. 
ps= (dataBG$PMT_PS_1+ dataBG$PMT_PS_2+ dataBG$PMT_PS_3+ dataBG$PMT_PS_4+ dataBG$PMT_PS_5)/5 

# perceived vuln.
pv= (dataBG$PMT_PV_1+ dataBG$PMT_PV_2+ dataBG$PMT_PV_3+ dataBG$PMT_PV_4+ dataBG$PMT_PV_5)/5

# fear respo.
fr= (dataBG$PMT_FR_1+ dataBG$PMT_FR_2+ dataBG$PMT_FR_3)/3

# response effica.
re= ( dataBG$PMT_RE_1+dataBG$ PMT_RE_2+ dataBG$PMT_RE_3+ dataBG$PMT_RE_4)/4 

# self effica.
se= (dataBG$PMT_SE_1+ dataBG$PMT_SE_2+ dataBG$PMT_SE_3+ dataBG$PMT_SE_4+ dataBG$PMT_SE_5)/5

# response cost
rc= (dataBG$PMT_RC_1+ dataBG$PMT_RC_2+dataBG$ PMT_RC_3+ dataBG$PMT_RC_4+ dataBG$PMT_RC_5+ dataBG$PMT_RC_6)/6


# ecological attitude

ea= (dataBG$EA_1+  dataBG$EA_2 + dataBG$EA_3+ dataBG$EA_4+ dataBG$EA_5+ dataBG$EA_6)/6


# prior knowledge (second)

pk= (dataBG$BGT_SCL_FIRE+ dataBG$BGT_SCL_NATS+ dataBG$BGT_SCL_PROP+ dataBG$BGT_SCL_SAG+ dataBG$BGT_SCL_VIST)/4



# cronbasch's alpha

library(psych)
pmtdt= data.frame(ps, pv, fr, re, se, rc, ea, pk)
alpha(pmtdt)


# data recoding demog. var.
library(plyr)

liv_tu_time= mapvalues(dataBG$LIV_TUC_TIME, from = c("1", "2", "3", "4", "5", "6"), 
                       to = c("0", "0", "0","1", "1" , "0"))                 # 1= >10yr, 0= <10yr
table(liv_tu_time)
educn= mapvalues(dataBG$EDUC, from = c("1", "2", "3", "4", "5", "6"), 
                                      to = c("1", "1", "1", "1", "2", "3")) 
edu_rf3= relevel(as.factor(educn))

edu_lv= mapvalues(dataBG$EDUC, from = c("1", "2", "3", "4", "5", "6"), 
                  to = c("0", "0", "0", "0", "1", "1"))               # 1= =>bachel
edu_lcg= mapvalues(dataBG$EDUC, from = c("1", "2", "3", "4", "5", "6"), # factor and level
                             to = c("1", "1", "0", "1", "0", "0"))
edu_cg= mapvalues(dataBG$EDUC, from = c("1", "2", "3", "4", "5", "6"), 
                             to = c("0", "0", "1", "0", "1", "0") ) 
edu_gr= mapvalues(dataBG$EDUC, from = c("1", "2", "3", "4", "5", "6"), 
                             to = c("0", "0", "0", "0", "0", "1"))
ed= relevel(dataBG$EDUC, ref = 6)
employ= mapvalues(dataBG$EMPLOY, from= c("1", "2", "3", "4", "5", "6", "7", "8"),
                  to = c("1", "1", "0", "0", "0", "0", "0", "0"))          # 1= employed

pol_part= mapvalues(dataBG$POL_PARTY, from= c("1", "2", "3", "4", "5", "6", "7"),
                    to = c("0", "1", "0", "0", "0", "0", "0"))    #1= democratic

pct_liv_tu= mapvalues(dataBG$LIV_TUC_PCT, from = c("1", "2", "3", "4", "5"),
                      to= c("1", "1", "0", "0", "0"))

age_rf4=relevel( as.factor(dataBG$AGE))

age_1829= mapvalues(dataBG$AGE, from = c("1", "2", "3", "4"),
                    to= c("1", "0", "0", "0"))
age_3044= mapvalues(dataBG$AGE, from = c("1", "2", "3", "4"),
                              to= c("0", "1", "0", "0"))
age_4564= mapvalues(dataBG$AGE, from = c("1", "2", "3", "4"),
                              to= c("0", "0", "1", "0"))
age_65am= mapvalues(dataBG$AGE, from = c("1", "2", "3", "4"),
                              to= c("0", "0", "0", "1"))
land_5am= mapvalues(dataBG$LAND_SIZE, from = c("1", "2", "3", "4"),
                    to= c("0", "0", "1", "1"))

zip_cd= mapvalues(dataBG$ZIPCODE, from = c("85641", "85704", "85713", "85718", "85730", 
                                           "85735", "85737", "85743", "85745", "85746", 
                                           "85747", "85748", "85749", "85750", "85756", 
                                           "85757", "85629",  
                                           
                                           "85701", "85705", "85707", "85710", "85711", 
                                           "85712", "85714", "85715", "85716", "85741", 
                                           "85742"),
                  
                                            to= c("1", "1","1","1","1","1","1","1","1",
                                                  "1","1","1","1","1","1","1","1",
                                                  
                                                  "0","0","0","0","0","0","0","0","0","0",
                                                  "0"
                                                  ))




                
zp_edg= as.factor(ifelse(zip_cd=="1",1,0))

zp_cnt=  as.factor(ifelse(zip_cd=="0",1,0))

pol_id_cv= as.factor(mapvalues(dataBG$POL_IDEO, from = c("1", "2", "3", "4", "5", "6"),
                  to= c("1", "1", "0", "0", "0", "0")))


pol_id_lb= as.factor(mapvalues(dataBG$POL_IDEO, from = c("1", "2", "3", "4", "5", "6"),
                     to= c("0", "0", "0", "1", "1", "0")))

fr_aft= as.factor(mapvalues(dataBG$BHF_AFFECT, from = c("0", "1", "2", "3", "4", "2,3", "1,2", "1,3", 
                                               "1,2,3", "2,0", "1,0", "0,4"),
                   to= c("0","1", "1", "1", "0", "1","1", "1", "1", "1", "1", "0")))
fr_aft

# combine and missing data

pmt_var= cbind(dataBG , pk, ps, pv, fr, re, se, rc, ea,  
               edu_lv, liv_tu_time, employ, pol_part, pct_liv_tu,
               age_1829, age_3044, age_4564, age_65am,
               edu_lcg, edu_cg, edu_gr,
               land_5am, zp_edg, zp_cnt)
summary(pmt_var)

############# MODELS FOR ACTIONS #############################

# data prep
library(dplyr)
library(plyr)

#elimination
elim_act= mapvalues(dataBG$BGA_WHICH, from = c("1", "2", "3", "4", 
                                             "1,2,3", "1,2", "2,3", "3,4", 
                                             "1,3,4", "1,3", "1,4","2,4",
                                             "1,2,3,4", "1,2,4"),
                  to= c("1", "0", "0", "0", 
                        "1", "1","0", "0",
                        "1", "1", "1", "0", 
                        "1", "1"))

elm_act_na= as.numeric(elim_act)

# public land actions taken
pub_actk= mapvalues(dataBG$BGA_WHICH, from = c("1", "2", "3", "4", 
                                              "1,2,3", "1,2", "2,3", "3,4", 
                                              "1,3,4", "1,3", "1,4","2,4",
                                              "1,2,3,4", "1,2,4"),
                   
                                  to= c("0", "1", "1", "1", 
                                        "1", "1","1", "1",
                                        "1", "1", "1", "1", 
                                        "1", "1"))

pub_ac_tk_na= as.numeric(pub_actk)
table(pub_ac_tk_na)
# private land willingness
pvt_wll= (dataBG$BGA_ELIM)
pvt_wll

# public land willingness

pub_wll= (dataBG$BGA_PULL+ dataBG$BGA_SPRAY+ dataBG$BGA_MONEY)
pub_wll

# combined data frame
dtf_act=cbind (pmt_var, ps, pv, fr, re, se, rc,pol_id_cv, pol_id_lb,
               pub_wll,  pvt_wll,fr_aft, age_rf4, edu_rf3, elm_act_na, pub_ac_tk_na)
dtf_act

################ Willingness to take action ############### 
# private land
sink("pvt lnd will.txt")
library(ordinal)
own_lnd_wll= clm(factor(pvt_wll)~pk+ ps+ pv+ fr+ ea+ re+ se+ rc+ liv_tu_time+ pct_liv_tu+ 
                 HOUSING + employ+ POL_IDEO_MOD+HOA_Y+ 
                  factor(age_rf4) + factor(edu_rf3)+ fr_aft, data=dtf_act)

summary(own_lnd_wll)

sink()
library(car)
vif(own_lnd_wl)
#odds-ratio


# public land
sink("pub lnd will.txt")
pub_lnd_wll= lm(pub_wll~pk+ ps+ pv+ fr+ ea+ re+ se+ rc+ liv_tu_time+ pct_liv_tu+ 
                  HOUSING + employ+ POL_IDEO_MOD+HOA_Y+ 
                  factor(age_rf4) + factor(edu_rf3)+ fr_aft, data=dtf_act)

summary(pub_lnd_wll)
sink()
library(car)
vif(pub_lnd_wll)
############ actions taken on lands###############
# private land 

sink("pvt act tkn.txt")
pvt_act_tk= glm(factor(elm_act_na) ~ pk+ ps+ pv+ fr+ ea+ re+ se+ rc+ liv_tu_time+ pct_liv_tu+ 
                  HOUSING + employ+ POL_IDEO_MOD+HOA_Y+ 
                  factor(age_rf4) + factor(edu_rf3)+ fr_aft, family = "binomial")

summary(pvt_act_tk)
sink()
vif(act_pvt_lnd)
exp(coef(act_pvt_lnd))
# public land 
sink("pub act tkn.txt")

pub_act_tk= glm(factor(pub_ac_tk_na)~ pk+ ps+ pv+ fr+ ea+ re+ se+ rc+ liv_tu_time+ pct_liv_tu+ 
                  HOUSING + employ+ POL_IDEO_MOD+HOA_Y+ 
                  factor(age_rf4) + factor(edu_rf3)+ fr_aft, family = "binomial")

summary(pub_act_tk)
sink()
vif(pb_ac_tkn)
exp(coef(pb_ac_tkn))










############### previous works ###############
##means and standard deviations
descriptive.table(vars = d(elm_act,pull_act),data= dtf_act,
                  func.names =c("Mean","St. Deviation","Valid N"))
# model for - action taken
sink("ac_tkn_outp.txt")

act_tkn= glm (BGA_TAKEN~ pk+ ps+ pv+ fr+ ea+ re+ se+ rc+ liv_tu_time+ pct_liv_tu+ 
                HOUSING + employ+ POL_IDEO_MOD+HOA_Y+ 
                factor(age_rf4) + factor(edu_rf3)+
                fr_afft, data=dtf_act, family = "binomial")
summary(act_tkn)
sink()

exp(coef(act_Tkn_mdll))
library(oddsratio)

# correlation 
as.numeric(pmt_var$BGA_ELIM, pmt_var$BGA_PULL, pmt_var$BGA_SPRAY, pmt_var$BGA_MONEY, 
           pmt_var$BGA_TAKEN,pmt_var$BGA_WHICH, pmt_var$BGA_WHICH_ELIM)

cordt= data.frame(pmt_var$AGE_18T29, dataBG$AGE_30T44, pmt_var$AGE_45T64, pmt_var$AGE_65AM,
                  pmt_var$EDUC_SHS, pmt_var$EDUC_HS, pmt_var$EDUC_SC, 
                  pmt_var$EDUC_AOT, pmt_var$EDUC_BAC, pmt_var$EDUC_GOP,
                  pmt_var$BGA_ELIM, pmt_var$BGA_PULL, pmt_var$BGA_SPRAY, pmt_var$BGA_MONEY, 
                  pmt_var$BGA_TAKEN)

cordco= cbind(cordt, elm_act, pull_act, spray_act, dona_act)
cordtna= na.omit(cordco)
cordtna
corl= cor(cordtna, method = "pearson")
corl

upper<-corl         # cor matrix table lower part only
upper[upper.tri(corl)]<-""
upper<-as.data.frame(upper)
upper
#######descriptive state#########
#age
numa= table(dataBG$AGE)
numa
(numa/548)*100

#edu
nume= table(dataBG$EDUC)
nume
(nume/548)*100

#housing
numh= table(dataBG$HOUSING)
numh
(numh/548)*100
#gender
numg= table(dataBG$GENDER)
numg
(numg/548)*100

# politics
nump= table(dataBG$POL_IDEO)
nump
(nump/548)*100

#income
numi= table(dataBG$HOUSE_INC)
numi
(numi/548)*100

#land own
numlo= table(dataBG$OWN_LAND)
numlo
(numlo/548)*100

# own land size
numlz= table(dataBG$LAND_SIZE)
numlz
(numlz/548)*100


# length or res
numt= table(dataBG$LIV_TUC_TIME)
numt
(numt/548)*100

# perc
nump= table(dataBG$LIV_TUC_PCT)
nump
(nump/548)*100

# zip code
numz= table(dtf_act$zp_cnt)
numz
(numz/548)*100

######  plot  ###############

library(ggplot2)
library(vioplot)
dtlikl= subset(dataBG, select= c("BGA_ELIM", "BGA_PULL", "BGA_MONEY", "BGA_SPRAY"))
nadtlkl=na.omit(dtlikl)

dtact= subset(dtf_act, select = c("elm_act", "pull_act", "spray_act", "dona_act"))
nadtact= na.omit(dtact)
vioplot(nadt, col = 4:8, border = 4:8)
p <- ggplot(nadt, aes(x=c, y=m)) + 
  geom_violin()
p

# boxplot
boxplot(nadtlkl, 
        col = c("orange", "blue", "yellow", "green"))

stripchart(nadtlkl,
           method = "jitter",
           pch = 10,
           col = 11,
           add = TRUE,
           dotsi)

nadt= na.omit (dataBG)
bxdt= select(dataBG, StartDate, EndDate, BGA_ELIM, BGA_PULL, BGA_SPRAY, BGA_MONEY)
library(reshape2)
cbcol= melt(bxdt, id.var = c('StartDate', 'EndDate'), variable.name = 'type')
cbcol

library(ggplot2)
ggplot(cbcol, aes(x=type, y=value, fill= type)) + 
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=3, 
               color="yellow", fill="red") +
  theme(legend.position="none")+ labs(x="Action type",y="Score")+
  theme(axis.text.x  = element_text(size = 8))+
  theme(axis.text.y  = element_text(size = 10))
  
  

  
  
  

  
###### summary for willingess vs action taken ######

library(summarytools)
freq(data= dtf_act, elm_act, pull_act)
descr(nadtact,
      headings = FALSE, # remove headings
      stats = "common") # most common descriptive statistics
dfSummary(nadt)


# data frame for the test
dtst= cbind(dtact, dtlikl)
# t=test of elimination W vs A
t.test(BGA_ELIM~elm_act, data = dtst)

# t=test of spray W vs A
t.test(BGA_SPRAY ~spray_act, data = dtst)
# t=test of pull W vs A
t.test(BGA_PULL ~ pull_act, data = dtst)
# t=test of dona W vs A
t.test(BGA_MONEY ~ dona_act, data = dtst)

##


