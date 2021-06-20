#========== PACOTES                   ==========
library(haven) #Ler SAS
library(biglm) #Regressão
library(fastDummies) #Criar dummies
library(data.table)
library(ggplot2)
library(stargazer)

#outros
library(car)
library(lmtest)
library(GoFKernel)
library(sandwich)
library(robustbase)
library(GGally)
library(car)
library(corpcor)
library(dplyr)
library(mctest)
library(plm)

#########################################

#########################################X
#========== DADOS                     ==========
#---------- 2016                      ----------
SIPP2016 = read_sas("m:/Users/Marcus/Downloads/pu2014w4.sas7bdat",cols_only=c(
  "ENJ_FLKWK1","ENJ_TLKWK1",
  "ENJ_FLKWK2","ENJ_TLKWK2",
  "ENJ_FLKWK3","ENJ_TLKWK3",
  "ENJ_FLKWK4","ENJ_TLKWK4",
  "ENJ_LKCTFLG1","ENJ_LKCTFLG2","ENJ_LKCTFLG3",
  "EANYKID","TCBYR_1","TCBYR_2","TCBYR_3","TCBYR_4","TCBYR_5","TCBYR_6","TCBYR_7",
  "TJB1_OCC", #tipo de trabalho
  "ERACE","EORIGIN",
  "EEDUC",
  "ESEX",
  "TAGE",
  "EHOWWELL", #língua em casa e quão bem fala inglês
  "EMS", #estado civil
  "TPTOTINC", "TPEARN", #soma das rendas mensais do ano, soma rendas com trabalho
  "TBORNPLACE","TST_INTV","TMETRO_INTV","ECITIZEN", #local de nascimento, residência e área metropolitana
  "EWELACTV2_1","EWELACTV2_2","EWELACTV2_3","EWELACTV3", #uso de boosts de emprego
  "SSUID","SHHADID","SWAVE","PNUM","MONTHCODE","GHLFSAM")) #infos
#SIPP2016¹ = SIPP2016[!duplicated(SIPP2016$SSUID),]

#---------- 2015                      ----------
SIPP2015 = read_sas("m:/Users/Marcus/Downloads/pu2014w3.sas7bdat",cols_only=c(
  "ENJ_FLKWK1","ENJ_TLKWK1",
  "ENJ_FLKWK2","ENJ_TLKWK2",
  "ENJ_FLKWK3","ENJ_TLKWK3",
  "ENJ_FLKWK4","ENJ_TLKWK4",
  "ENJ_LKCTFLG1","ENJ_LKCTFLG2","ENJ_LKCTFLG3",
  "EANYKID","TCBYR_1","TCBYR_2","TCBYR_3","TCBYR_4","TCBYR_5","TCBYR_6","TCBYR_7",
  "TJB1_OCC", #tipo de trabalho
  "ERACE","EORIGIN",
  "EEDUC",
  "ESEX",
  "TAGE",
  "EHOWWELL", #língua em casa e quão bem fala inglês
  "EMS", #estado civil
  "TPTOTINC", "TPEARN", #soma das rendas mensais do ano
  "TBORNPLACE","TST_INTV","TMETRO_INTV", "ECITIZEN",#local de nascimento, residência e área metropolitana
  "EWELACTV2_1","EWELACTV2_2","EWELACTV2_3","EWELACTV3", #uso de boosts de emprego
  "SSUID","SHHADID","SWAVE","PNUM","MONTHCODE","GHLFSAM")) #infos
#SIPP2015¹ = SIPP2015[!duplicated(SIPP2015$SSUID),]

#---------- 2014                      ----------
SIPP2014 = read_sas("m:/Users/Marcus/Downloads/pu2014w2.sas7bdat",cols_only=c(
  "ENJ_FLKWK1","ENJ_TLKWK1",
  "ENJ_FLKWK2","ENJ_TLKWK2",
  "ENJ_FLKWK3","ENJ_TLKWK3",
  "ENJ_FLKWK4","ENJ_TLKWK4",
  "ENJ_LKCTFLG1","ENJ_LKCTFLG2","ENJ_LKCTFLG3",
  "EANYKID","TCBYR_1","TCBYR_2","TCBYR_3","TCBYR_4","TCBYR_5","TCBYR_6","TCBYR_7",
  "TJB1_OCC", #tipo de trabalho
  "ERACE","EORIGIN",
  "EEDUC",
  "ESEX",
  "TAGE",
  "EHOWWELL", #língua em casa e quão bem fala inglês
  "EMS", #estado civil
  "TPTOTINC", "TPEARN", #soma das rendas mensais do ano
  "TBORNPLACE","TST_INTV","TMETRO_INTV","ECITIZEN", #local de nascimento, residência e área metropolitana
  "EWELACTV2_1","EWELACTV2_2","EWELACTV2_3","EWELACTV3", #uso de boosts de emprego
  "SSUID","SHHADID","SWAVE","PNUM","MONTHCODE","GHLFSAM")) #infos
#SIPP2014¹ = SIPP2014[!duplicated(SIPP2014$SSUID),]

#---------- 2013                      ----------
SIPP2013 = read_sas("m:/Users/Marcus/Downloads/pu2014w1.sas7bdat",cols_only=c(
  "ENJ_FLKWK1","ENJ_TLKWK1",
  "ENJ_FLKWK2","ENJ_TLKWK2",
  "ENJ_FLKWK3","ENJ_TLKWK3",
  "ENJ_FLKWK4","ENJ_TLKWK4",
  "ENJ_LKCTFLG1","ENJ_LKCTFLG2","ENJ_LKCTFLG3",
  "EANYKID","TCBYR_1","TCBYR_2","TCBYR_3","TCBYR_4","TCBYR_5","TCBYR_6","TCBYR_7",
  "tjb1_occ", #tipo de trabalho
  "ERACE","EORIGIN",
  "EEDUC",
  "ESEX",
  "TAGE",
  "EHOWWELL", #língua em casa e quão bem fala inglês
  "EMS", #estado civil
  "TPTOTINC", "TPEARN", #soma das rendas mensais do ano
  "tbornplace","tst_intv","tmetro_intv","ECITIZEN", #local de nascimento, residência e área metropolitana
  "EWELACTV2_1","EWELACTV2_2","EWELACTV2_3","EWELACTV3", #uso de boosts de emprego
  "ssuid","shhadid","swave","PNUM","monthcode","ghlfsam")) #infos
colnames(SIPP2013) = colnames(SIPP2016)
#SIPP2013¹ = SIPP2013[!duplicated(SIPP2013$SSUID),]

#---------- sipp                      ----------
#SIPP5 = rbind(
#  SIPP2016[!duplicated(SIPP2016$SSUID),],
#  SIPP2015[!duplicated(SIPP2015$SSUID),],
#  SIPP2014[!duplicated(SIPP2014$SSUID),],
#  SIPP2013[!duplicated(SIPP2013$SSUID),])
#remove(SIPP2016¹,SIPP2015¹,SIPP2014¹,SIPP2013¹)

SIPP5 = rbind(SIPP2016,SIPP2015,SIPP2014,SIPP2013)
SIPP5 = SIPP2016
#SIPP5 = SIPP2015
#SIPP5 = SIPP2014
#SIPP5 = SIPP2013

SIPP5 = subset(SIPP5,!is.na(SIPP5$ENJ_FLKWK1))
#SIPP5 = subset(SIPP5, SIPP5$GHLFSAM=="01"|SIPP5$GHLFSAM==1)
#SIPP5 = subset(SIPP5, SIPP5$TPTOTINC>0)

#########################################

#########################################X
#========== VARIÁVEIS                 ==========
#---------- desemprego                ----------
TIME5 = as.data.frame(cbind(
  SIPP5$ENJ_TLKWK2,SIPP5$ENJ_TLKWK3,SIPP5$ENJ_TLKWK4,
  SIPP5$ENJ_FLKWK2,SIPP5$ENJ_FLKWK3,SIPP5$ENJ_FLKWK4))

TIME5[is.na(TIME5)]=0
SIPP5$ENJ_TLKWK1[is.na(SIPP5$ENJ_TLKWK1)]=0

#DES5 = as.matrix(apply(cbind(ifelse(TIME5[,1]==0,SIPP5$ENJ_TLKWK1-SIPP5$ENJ_FLKWK1,0),
#                             ifelse(TIME5[,2]==0,TIME5$ENJ_TLKWK2-TIME5$ENJ_FLKWK2,0),
#                             ifelse(TIME5[,3]==0,TIME5$ENJ_TLKWK3-TIME5$ENJ_FLKWK3,0),
#                             TIME5$ENJ_TLKWK4-TIME5$ENJ_FLKWK4),1,max)) #maior período que tenha começado no ano

#DES2 = cbind(ifelse(TIME5[,1]==0,SIPP5$ENJ_TLKWK1-SIPP5$ENJ_FLKWK1,0)+
#               ifelse(TIME5[,2]==0,TIME5$ENJ_TLKWK2-TIME5$ENJ_FLKWK2,0)+
#               ifelse(TIME5[,3]==0,TIME5$ENJ_TLKWK3-TIME5$ENJ_FLKWK3,0)+
#               TIME5$ENJ_TLKWK4-TIME5$ENJ_FLKWK4) #soma dos períodos que começaram no ano

DES6 = as.matrix(apply(cbind(SIPP5$ENJ_TLKWK1-SIPP5$ENJ_FLKWK1,
                             TIME5$ENJ_TLKWK2-TIME5$ENJ_FLKWK2,
                             TIME5$ENJ_TLKWK3-TIME5$ENJ_FLKWK3,
                             TIME5$ENJ_TLKWK4-TIME5$ENJ_FLKWK4),1,max)) #maior período

#DES7 = DES5
#DES7[DES7==0]=NA

DES8 = as.numeric(DES6)
DES8[DES8==0]=1

remove(TIME5)
remove(DES6)

#---------- raça                      ----------
RAÇA = as.data.frame(dummy_cols(SIPP5$ERACE)[,-1])
colnames(RAÇA) = c("RAÇb","RAÇn","RAÇa","RAÇo")

#RAÇb = RAÇA[,2]
#RAÇn = RAÇA[,3]
#RAÇa = RAÇA[,4]
#RAÇo = RAÇA[,5]

#---------- raça2                     ----------
RAÇA2 = ifelse(SIPP5$EORIGIN==1,5,SIPP5$ERACE)  
RAÇA2 = as.data.frame(dummy_cols(RAÇA2)[,-1])

colnames(RAÇA2) = c("RAÇb","RAÇn","RAÇa","RAÇo","RAÇl")

#---------- nível de ingles           ----------
SIPP5$EHOWWELL[is.na(SIPP5$EHOWWELL)]=1
INGL = as.data.frame(dummy_cols(SIPP5$EHOWWELL)[,-1])
colnames(INGL) = c("ING1","ING2","ING3","ING4")

#ING1 = INGL[,2]
#ING2 = INGL[,3]
#ING3 = INGL[,4]
#ING4 = INGL[,5]

#---------- educação                  ----------
EDUd = dummy_cols(SIPP5$EEDUC)

EDUC = as.data.frame(cbind(
  "EDU1" = EDUd[,2]+EDUd[,3],
  #+EDUd[,18],
  "EDU2" = EDUd[,4]+EDUd[,5]+EDUd[,6],
  "EDU3" = EDUd[,7]+EDUd[,8]+EDUd[,9]+EDUd[,10],
  "EDU4" = EDUd[,11]+EDUd[,12]+EDUd[,13]+EDUd[,14],
  "EDU5" = EDUd[,15]+EDUd[,16]+EDUd[,17]))

remove(EDUd)

#---------- sexo                      ----------
SEXO = as.data.frame(cbind(
  "SEXm" = as.numeric(SIPP5$ESEX==1),
  "SEXf" = as.numeric(SIPP5$ESEX==2)))

#---------- idade                     ----------
IDAD = as.data.frame(as.numeric(SIPP5$TAGE))
colnames(IDAD) = c("IDAD")

IDAD² = IDAD^2
colnames(IDAD²) = c("IDAD²")

#---------- estado civil              ----------
CASD = as.data.frame(cbind(
  "CASs" = ifelse(SIPP5$EMS==1,1,0),
  "CASn" = ifelse(SIPP5$EMS==1,0,1)))
CASD[is.na(CASD)]=0 #NA-->menor de 15, consideramos então não casados

#---------- renda                     ----------
RENDl = as.data.frame(as.numeric(ifelse(!is.na(SIPP5$TPTOTINC),SIPP5$TPTOTINC,0))) - as.data.frame(as.numeric(ifelse(!is.na(SIPP5$TPEARN),SIPP5$TPEARN,0)))
colnames(RENDl) = c("RENDl")

RENDt = as.data.frame(as.numeric(ifelse(!is.na(SIPP5$TPTOTINC),SIPP5$TPTOTINC,0))) #renda total
colnames(RENDt) = c("RENDt")

RENDs = as.data.frame(as.numeric(ifelse(!is.na(SIPP5$TPEARN),SIPP5$TPEARN,0))) #renda de empregos
colnames(RENDs) = c("RENDs")

#---------- nacionalidade             ----------
NACI = as.data.frame(cbind(
  "NACl"  = ifelse(SIPP5$TBORNPLACE%in%1:56,1,0), #nacionalidade local
  "NACam" = ifelse(SIPP5$TBORNPLACE==63,1,0),     #nacionalidade americana
  "NACaf" = ifelse(SIPP5$TBORNPLACE==64,1,0),     #nacionalidade africana
  "NACeu" = ifelse(SIPP5$TBORNPLACE==61,1,0),     #nacionalidade européia
  "NACas" = ifelse(SIPP5$TBORNPLACE==62,1,0),     #nacionalidade asia
  "NACoo" = ifelse(SIPP5$TBORNPLACE%in%65:66,1,0),#nacionalidade oceania e outros
  "NACe"  = ifelse(SIPP5$TBORNPLACE%in%1:56,0,1))) #nacionalidade estrangeira
NACIe = NACI$NACe

#---------- residência                ----------
RESI = as.data.frame(dummy_cols(SIPP5$TST_INTV))[,-1]
resi = c(".data_",".data_01",".data_02",".data_04",".data_05",".data_06",".data_08",".data_09",".data_10",".data_11",".data_12",".data_13",".data_15",".data_16",".data_17",".data_18",".data_19",".data_20",".data_21",".data_22",".data_23",".data_24",".data_25",".data_26",".data_27",".data_28",".data_29",".data_30",".data_31",".data_32",".data_33",".data_34",".data_35",".data_36",".data_37",".data_38",".data_39",".data_40",".data_41",".data_42",".data_44",".data_45",".data_46",".data_47",".data_48",".data_49",".data_50",".data_51",".data_52",".data_53",".data_54",".data_55",".data_56",".data_NA")

#---------- cidadania                 ----------
CITI = as.data.frame(SIPP5$ECITIZEN-1)
colnames(CITI) = c("CITI")

#---------- urbano                    ----------
SIPP5$TMETRO_INTV[is.na(SIPP5$TMETRO_INTV)]=0 #não respondeu --> joga pro "outro"
#METR = as.data.frame(cbind(
#  "METr" = as.numeric(SIPP5$TMETRO_INTV==2),  #rural
#  "METu" = as.numeric(SIPP5$TMETRO_INTV==1),  #metropolitano
#  "METo" = as.numeric(SIPP5$TMETRO_INTV==0))) #outro

METR = as.data.frame(cbind(
  "METr" = as.numeric(SIPP5$TMETRO_INTV==2|0),  #rural ou outro
  "METu" = as.numeric(SIPP5$TMETRO_INTV==1)))   #metro

#---------- programas                 ----------
PROG = as.data.frame(SIPP5$EWELACTV2_1==1|SIPP5$EWELACTV2_2==1|SIPP5$EWELACTV2_3==1)
PROG[is.na(PROG)]=0
colnames(PROG)=c("PROG")

#PRO1 = SIPP5$EWELACTV2_1==2
#PRO2 = SIPP5$EWELACTV2_2==2
#PRO3 = SIPP5$EWELACTV2_3==2

#PROv = as.data.frame(dummy_cols(SIPP5$EWELACTV3)[,c(-1,-5)])
#colnames(PROv) = c("REQ","CHO","BTH")
#PROv[is.na(PROv)] = 0

#---------- filhos                    ----------
#EANY  = (!is.na(SIPP5$"TCBYR_1")&(SIPP5$EANYKID-1)) | (!is.na(SIPP5$"TCBYR_1")&!is.na(SIPP5$"TCBYR_2"))
#FILH1 = (SIPP5$"TCBYR_1">2014|SIPP5$"TCBYR_2">2014|SIPP5$"TCBYR_3">2014|SIPP5$"TCBYR_4">2014|SIPP5$"TCBYR_5">2014|SIPP5$"TCBYR_6">2014|SIPP5$"TCBYR_7">2014)
#FILH2 = (SIPP5$"TCBYR_1">1999&SIPP5$"TCBYR_1"<2014|SIPP5$"TCBYR_2">1999&SIPP5$"TCBYR_2"<2014|SIPP5$"TCBYR_3">1999&SIPP5$"TCBYR_3"<2014|SIPP5$"TCBYR_4">1999&SIPP5$"TCBYR_4"<2014|SIPP5$"TCBYR_5">1999&SIPP5$"TCBYR_5"<2014|SIPP5$"TCBYR_6">1999&SIPP5$"TCBYR_6"<2014|SIPP5$"TCBYR_7">1999&SIPP5$"TCBYR_7"<2014)

FILH1 = (SIPP5$"TCBYR_1">2014|SIPP5$"TCBYR_2">2014|SIPP5$"TCBYR_3">2014|SIPP5$"TCBYR_4">2014|SIPP5$"TCBYR_5">2014|SIPP5$"TCBYR_6">2014|SIPP5$"TCBYR_7">2014)
FILH2 = (SIPP5$"TCBYR_1">2002&SIPP5$"TCBYR_1"<2014|SIPP5$"TCBYR_2">2002&SIPP5$"TCBYR_2"<2014|SIPP5$"TCBYR_3">2002&SIPP5$"TCBYR_3"<2014|SIPP5$"TCBYR_4">2002&SIPP5$"TCBYR_4"<2014|SIPP5$"TCBYR_5">2002&SIPP5$"TCBYR_5"<2014|SIPP5$"TCBYR_6">2002&SIPP5$"TCBYR_6"<2014|SIPP5$"TCBYR_7">2002&SIPP5$"TCBYR_7"<2014)


#FILHO = as.data.frame(cbind(as.numeric(FILH1),as.numeric(FILH2)))
#colnames(FILHO) = c("FILH1","FILH2")
#FILHO[is.na(FILHO)]=0
#remove(FILH1,FILH2)

FILHO = as.data.frame(as.numeric(FILH1|FILH2))
colnames(FILHO) = c("FILHO")
FILHO[is.na(FILHO)]=0
remove(FILH1,FILH2,EANY)

#########################################X
#########################################X
#########################################X
#########################################X


#########################################

################### CERTA ######################
#---------- S1                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],                     #tirando branco
  SEXO[,-2,drop=FALSE],          #tirando mulher
  NACI[,2:5])

s1 = lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))
summary(s1)$coefficients[1:9,]

#---------- S2                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:5],
  RESI)

s2 = lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))
summary(s2)$coefficients[1:9,]

#---------- S3                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:5],
  INGL[,-1],
  EDUC[,-1],
  RESI)

s3 = lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))
#summary(s3)$coefficients[1:12,]

#---------- S4                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:5],
  INGL[,-1],
  EDUC[,-1],
  IDAD,
  IDAD²,
  RESI)

s4 = lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))
#summary(s4)$coefficients[1:14,]

#---------- S5                        ----------
#DATA = cbind(
#  DES8,
#  RAÇA[,-1],            
#  SEXO[,-2,drop=FALSE], 
#  NACI[,2:5],
#  INGL[,-1],
#  IDAD,
#  IDAD²,
#  EDUC[,-1],
#  RESI)
#
#s5 = lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))
#summary(s5)$coefficients[1:18,]

#---------- star s1-s5                ----------
stargazer(s1,s2,s3,s4,s5, single.row=TRUE, omit=resi)

#---------- S6                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:5],
  INGL[,-1],
  EDUC[,-1],
  IDAD,
  IDAD²,
  RENDl,
  CASD[,-2,drop=FALSE],
  FILHO,
  CITI,
  METR,
  PROG,
  RESI[,-52,drop=FALSE])

s6 = lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))
round(summary(s6)$coefficients[1:25,],3)

H0 = colnames(cbind(RENDl,CASD[,-2,drop=FALSE],FILHO,CITI,METR,PROG))
linearHypothesis(s6,H0)
remove(H0)

#---------- star s1-s6                ----------
stargazer(s1,s2,s3,s4,s5,s6, single.row=TRUE, omit=cbind(resi,H0))

#---------- S6-1                      ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
"S_n" =  SEXO[,-2]*RAÇA[,2],
"S_a" =  SEXO[,-2]*RAÇA[,3],
"S_o" =  SEXO[,-2]*RAÇA[,4],
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

summary(lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4&NACI$NACl==1))) #tira citi tbm
#---------- S6-R                      ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  SEXO[,-2]*RAÇA[,2],
  SEXO[,-2]*RAÇA[,3],
  SEXO[,-2]*RAÇA[,4],
  (SEXO[,-2]*RAÇA[,2]*RENDl),
  (SEXO[,-2]*RAÇA[,3]*RENDl),
  (SEXO[,-2]*RAÇA[,4]*RENDl),
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)
colnames(DATA)=c(colnames(DATA)[1:5],"S_n","S_a" ,"S_o" ,"S_ni" ,"S_ai" ,"S_oi" ,colnames(DATA)[12:78])

s5_R = summary(lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4&NACI$NACl==1))) #tira citi tbm

R_n = function(x){s5_R$coefficients[1,1] + s5_R$coefficients[6,1] + s5_R$coefficients[9,1]*x}
R_a = function(x){s5_R$coefficients[1,1] + s5_R$coefficients[7,1] + s5_R$coefficients[10,1]*x}
R_o = function(x){s5_R$coefficients[1,1] + s5_R$coefficients[8,1] + s5_R$coefficients[11,1]*x}

df = cbind(DES8,RENDl, paste(subset(SIPP5$ERACE,SIPP5$ERACE!=1),SEXO$SEXm))
df = df[RENDl>0,]

qplot(RENDl,DES8,data=df,color=as.factor(df[,3])) +
  geom_smooth(method="lm",se=FALSE) +
  stat_function(fun = R_n, size=1, color="darkred") + 
  stat_function(fun = R_a, size=1, color="darkblue") +
  stat_function(fun = R_o, size=1, color="darkmagenta") +
  xlim(0,11000) + ylim(-30,55)


#---------- S6-I                      ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  SEXO[,-2]*RAÇA[,2],
  SEXO[,-2]*RAÇA[,3],
  SEXO[,-2]*RAÇA[,4],
  (SEXO[,-2]*RAÇA[,2]*IDAD),
  (SEXO[,-2]*RAÇA[,3]*IDAD),
  (SEXO[,-2]*RAÇA[,4]*IDAD),
  (SEXO[,-2]*RAÇA[,2]*IDAD²),
  (SEXO[,-2]*RAÇA[,3]*IDAD²),
  (SEXO[,-2]*RAÇA[,4]*IDAD²),
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)
colnames(DATA)=c(colnames(DATA)[1:5],"S_n","S_a" ,"S_o" ,"S_ni" ,"S_ai" ,"S_oi" ,"S_ni²","S_ai²","S_oi²",colnames(DATA)[15:81])

s5_I = summary(lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4&NACI$NACl==1))) #tira citi tbm

I_n = function(x){s5_I$coefficients[1,1] + s5_I$coefficients[6,1] + s5_R$coefficients[9,1]*x  + s5_R$coefficients[12,1]*(x^2)}
I_a = function(x){s5_I$coefficients[1,1] + s5_I$coefficients[7,1] + s5_R$coefficients[10,1]*x + s5_R$coefficients[13,1]*(x^2)}
I_o = function(x){s5_I$coefficients[1,1] + s5_I$coefficients[8,1] + s5_R$coefficients[11,1]*x + s5_R$coefficients[14,1]*(x^2)}

df = cbind(DES8,IDAD, paste(subset(SIPP5$ERACE,SIPP5$ERACE!=1),SEXO$SEXm))
df = df[RENDl>0,]

qplot(IDAD,DES8,data=df,color=as.factor(df[,3])) +
  geom_smooth(method="lm",se=FALSE) +
  stat_function(fun = I_n, size=1, color="darkred") + 
  stat_function(fun = I_a, size=1, color="darkblue") +
  stat_function(fun = I_o, size=1, color="darkmagenta") +
  xlim(0,90) + ylim(-30,55)


#################################################
################### RAÇA 2 ######################X # ta faltando acertar os coefs depois de mudar filho
#---------- S1                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],                     #tirando branco
  SEXO[,-2,drop=FALSE],          #tirando mulher
  NACI[,2:5])

s1 = lm(DES8 ~ ., data = DATA)
#summary(s1)$coefficients[1:10,]

#---------- S2                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:5],
  RESI)

s2 = lm(DES8 ~ ., data = DATA)
#summary(s2)$coefficients[1:10,]

#---------- S3                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:5],
  INGL[,-1],
  EDUC[,-1],
  RESI)

s3 = lm(DES8 ~ ., data = DATA)
#summary(s3)$coefficients[1:13,]

#---------- S4                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:5],
  INGL[,-1],
  EDUC[,-1],
  IDAD,
  IDAD²,
  RESI)

s4 = lm(DES8 ~ ., data = DATA)
#summary(s4)$coefficients[1:15,]

#---------- S5                        ----------
#DATA = cbind(
#  DES8,
#  RAÇA[,-1],            
#  SEXO[,-2,drop=FALSE], 
#  NACI[,2:5],
#  INGL[,-1],
#  IDAD,
#  IDAD²,
#  EDUC[,-1],
#  RESI)

s5 = lm(DES8 ~ ., data = DATA)
#summary(s5)$coefficients[1:19,]

#---------- star s1-s5                ----------
#stargazer(s1,s2,s3,s4,s5, single.row=TRUE, omit=resi)

#---------- S6                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:5],
  INGL[,-1],
  IDAD,
  IDAD²,
  EDUC[,-1],
  RENDl,
  CASD[,-2,drop=FALSE],
  FILHO,
  CITI,
  METR,
  PROG,
  RESI[,-52,drop=FALSE])

s6 = lm(DES8 ~ ., data = DATA)

#s6r = lmrob(DES8 ~ ., data=DATA[,-1],fast.s.large.n = Inf)
#round(summary(s6)$coefficients[1:26,c(1,4)],3)

#---------- star s1-s6                ----------
H0 = colnames(cbind(RENDl,CASD[,-2,drop=FALSE],FILHO,CITI,METR,PROG))
stargazer(s1,s2,s3,s4,s6, single.row=TRUE, omit=cbind(resi,H0))
remove(H0)

#---------- COMPARAÇÃO                ----------
#comp = cbind(
#round(rbind(summary(s1)$coefficients[1:10,c(1,4)],matrix(0,nrow=26-10,ncol=2)),2),
#round(summary(s2)$coefficients[1:26,c(1,4)],2),
#round(summary(s3)$coefficients[1:26,c(1,4)],2),
#round(summary(s4)$coefficients[1:26,c(1,4)],2),
#round(summary(s5)$coefficients[1:26,c(1,4)],2),
#round(summary(s6)$coefficients[1:26,c(1,4)],2))
#rownames(comp) = colnames(DATA[,1:26])

#comp2016=comp
#comp2015=comp
#comp2014=comp

#---------- S6-S_R                    ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],
  NACI[,2:6],
  SEXO[,-2,drop=FALSE], 
  "S_n" =  SEXO[,-2]*RAÇA[,2],
  "S_a" =  SEXO[,-2]*RAÇA[,3],
  "S_o" =  SEXO[,-2]*RAÇA[,4],
  "S_l" =  SEXO[,-2]*RAÇA[,5],
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

summary(lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))) #tira citi tbm

#---------- S6-N_R                    ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],
  NACI[,2:6],
  SEXO[,-2,drop=FALSE], 
  NACI[,2]*RAÇA[,2],
  NACI[,2]*RAÇA[,3],
  NACI[,2]*RAÇA[,4],
  NACI[,2]*RAÇA[,5],
  NACI[,3]*RAÇA[,2],
  NACI[,3]*RAÇA[,3],
  NACI[,3]*RAÇA[,4],
  NACI[,3]*RAÇA[,5],
  NACI[,4]*RAÇA[,2],
  NACI[,4]*RAÇA[,3],
  NACI[,4]*RAÇA[,4],
  NACI[,4]*RAÇA[,5],
  NACI[,5]*RAÇA[,2],
  NACI[,5]*RAÇA[,3],
  NACI[,5]*RAÇA[,4],
  NACI[,5]*RAÇA[,5],
  NACI[,6]*RAÇA[,2],
  NACI[,6]*RAÇA[,3],
  NACI[,6]*RAÇA[,4],
  NACI[,6]*RAÇA[,5],
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

summary(lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))) #tira citi tbm

#---------- S6-N_R_S                  ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE],
  NACI[,2:6],
  SEXO[,-2]*NACI[,2]*RAÇA[,2],
  SEXO[,-2]*NACI[,2]*RAÇA[,3],
  SEXO[,-2]*NACI[,2]*RAÇA[,4],
  SEXO[,-2]*NACI[,2]*RAÇA[,5],
  SEXO[,-2]*NACI[,3]*RAÇA[,2],
  SEXO[,-2]*NACI[,3]*RAÇA[,3],
  SEXO[,-2]*NACI[,3]*RAÇA[,4],
  SEXO[,-2]*NACI[,3]*RAÇA[,5],
  SEXO[,-2]*NACI[,4]*RAÇA[,2],
  SEXO[,-2]*NACI[,4]*RAÇA[,3],
  SEXO[,-2]*NACI[,4]*RAÇA[,4],
  SEXO[,-2]*NACI[,4]*RAÇA[,5],
  SEXO[,-2]*NACI[,5]*RAÇA[,2],
  SEXO[,-2]*NACI[,5]*RAÇA[,3],
  SEXO[,-2]*NACI[,5]*RAÇA[,4],
  SEXO[,-2]*NACI[,5]*RAÇA[,5],
  SEXO[,-2]*NACI[,6]*RAÇA[,2],
  SEXO[,-2]*NACI[,6]*RAÇA[,3],
  SEXO[,-2]*NACI[,6]*RAÇA[,4],
  SEXO[,-2]*NACI[,6]*RAÇA[,5],
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

summary(lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))) #tira citi tbm


#---------- S6-N_R_S c/I              ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE],
  NACI[,2:6],
  SEXO[,-2]*IDAD,
  SEXO[,-2]*IDAD²,
  NACI[,2]*IDAD,
  NACI[,2]*IDAD²,
  NACI[,3]*IDAD,
  NACI[,3]*IDAD²,
  NACI[,4]*IDAD,
  NACI[,4]*IDAD²,
  NACI[,5]*IDAD,
  NACI[,5]*IDAD²,
  NACI[,6]*IDAD,
  NACI[,6]*IDAD²,
  RAÇA[,2]*IDAD,
  RAÇA[,2]*IDAD²,
  RAÇA[,3]*IDAD,
  RAÇA[,3]*IDAD²,
  RAÇA[,4]*IDAD,
  RAÇA[,4]*IDAD²,
  RAÇA[,5]*IDAD,
  RAÇA[,5]*IDAD²,
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

summary(lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))) #tira citi tbm


#---------- S6-N_R_S c/R              ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE],
  NACI[,2:6],
  SEXO[,-2]*RENDl,
  NACI[,2]*RENDl,
  NACI[,3]*RENDl,
  NACI[,4]*RENDl,
  NACI[,5]*RENDl,
  NACI[,6]*RENDl,
  RAÇA[,2]*RENDl,
  RAÇA[,3]*RENDl,
  RAÇA[,4]*RENDl,
  RAÇA[,5]*RENDl,
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

summary(lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))) #tira citi tbm


#---------- S6-S_F e S_C              ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],
  NACI[,2:6],
  SEXO[,-2,drop=FALSE], 
  "S_n" =  SEXO[,-2]*FILHO,
  "S_a" =  SEXO[,-2]*CASD[,-2],
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

summary(lm(DES8 ~ ., data = subset(DATA,SIPP5$SWAVE==4))) #tira citi tbm


#################################################
################### RAÇA 2 FGLS #################X # ta faltando acertar os coefs depois de mudar filho
#---------- S1                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],                     #tirando branco
  SEXO[,-2,drop=FALSE],          #tirando mulher
  NACI[,2:6])

s1 = lm(DES8 ~ ., data = DATA)
s1 = lm(DES8 ~ ., data = DATA, weights=1/fitted(s1)^2)

#h = sqrt(exp(fitted(lm(log(resid(s1)^2) ~ ., data=DATA[,-1]))))
#s1 = lm(DES8/h ~ ., data = DATA[,-1]/h)
#summary(s1)$coefficients[1:10,]

#---------- S2                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:6],
  RESI)

s2 = lm(DES8 ~ ., data = DATA)
s2 = lm(DES8 ~ ., data = DATA, weights=1/fitted(s2)^2)

#h = sqrt(exp(fitted(lm(log(resid(s2)^2) ~ ., data=DATA[,-1]))))
#s2 = lm(DES8/h ~ ., data = DATA[,-1]/h)
#summary(s2)$coefficients[1:10,]

#---------- S3                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE],
  NACI[,2:6],
  EDUC[,-1],
  INGL[,-1],
  RESI)

s3 = lm(DES8 ~ ., data = DATA)
s3 = lm(DES8 ~ ., data = DATA, weights=1/fitted(s3)^2)

#h = sqrt(exp(fitted(lm(log(resid(s3)^2) ~ ., data=DATA[,-1]))))
#s3 = lm(DES8/h ~ ., data = DATA[,-1]/h)
#summary(s3)$coefficients[1:13,]

#---------- S4                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:6],
  INGL[,-1],
  EDUC[,-1],
  IDAD,
  IDAD²,
  RESI)

s4 = lm(DES8 ~ ., data = DATA)
s4 = lm(DES8 ~ ., data = DATA, weights=1/fitted(s4)^2)

#h = sqrt(exp(fitted(lm(log(resid(s4)^2) ~ ., data=DATA[,-1]))))
#s4 = lm(DES8/h ~ ., data = DATA[,-1]/h)
#summary(s4)$coefficients[1:15,]

#---------- S5                        ----------
#DATA = cbind(
#  DES8,
#  RAÇA[,-1],            
#  SEXO[,-2,drop=FALSE], 
#  NACI[,2:5],
#  INGL[,-1],
#  EDUC[,-1],
#  IDAD,
#  IDAD²,
#  RESI)
#
#s5 = lm(DES8 ~ ., data = DATA)
#s5 = lm(DES8 ~ ., data = DATA, weights=1/fitted(s5)^2)

#h = sqrt(exp(fitted(lm(log(resid(s5)^2) ~ ., data=DATA[,-1]))))
#s5 = lm(DES8/h ~ ., data = DATA[,-1]/h)

#summary(s5)$coefficients[1:19,]

#---------- star s1-s5                ----------
#stargazer(s1,s2,s3,s4,s5, single.row=TRUE, omit=resi)

#---------- S6                        ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:6],
  INGL[,-1],
  EDUC[,-1],
  IDAD,
  IDAD²,
  RENDl,
  CASD[,-2,drop=FALSE],
  FILHO,
  CITI,
  METR,
  PROG,
  RESI)

s6 = lm(DES8 ~ ., data = DATA)
s6 = lm(DES8 ~ ., data = DATA, weights=1/fitted(s6)^2)

#h = sqrt(exp(fitted(lm(log(resid(s6)^2) ~ ., data=DATA[,-1]))))
#s6 = lm(DES8/h ~ ., data = DATA[,-1]/h)

#---------- S6r                       ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE], 
  NACI[,2:5],
  INGL[,-1],
  EDUC[,-1],
  IDAD,
  IDAD²,
  RENDl,
  CASD[,-2,drop=FALSE],
  FILHO,
  CITI,
  METR,
  PROG,
  RESI[,-52,drop=FALSE])

s6 = lm(DES8 ~ ., data = DATA)
s6 = lm(DES8 ~ ., data = DATA, weights=1/fitted(s6)^2)

#h = sqrt(exp(fitted(lm(log(resid(s6)^2) ~ ., data=DATA[,-1]))))
#s6 = lm(DES8/h ~ ., data = DATA[,-1]/h)

#---------- star s1-s6                ----------
H0 = colnames(cbind(RENDl,CASD[,-2,drop=FALSE],FILHO,CITI,METR,PROG))
stargazer(s1,s2,s3,s4,s6, single.row=TRUE, omit=cbind(resi,H0))
remove(H0)

#---------- COMPARAÇÃO                ----------
#comp = cbind(
#round(rbind(summary(s1)$coefficients[1:10,c(1,4)],matrix(0,nrow=26-10,ncol=2)),2),
#round(summary(s2)$coefficients[1:26,c(1,4)],2),
#round(summary(s3)$coefficients[1:26,c(1,4)],2),
#round(summary(s4)$coefficients[1:26,c(1,4)],2),
#round(summary(s5)$coefficients[1:26,c(1,4)],2),
#round(summary(s6)$coefficients[1:26,c(1,4)],2))
#rownames(comp) = colnames(DATA[,1:26])

#comp2016=comp
#comp2015=comp
#comp2014=comp

#---------- S6-S_R                    ----------
DATA = cbind(
  DES8,
  RAÇA[,-4],
  NACI[,2:6],
  SEXO[,-2,drop=FALSE], 
  "S_n" =  SEXO[,-2]*RAÇA[,2],
  "S_a" =  SEXO[,-2]*RAÇA[,3],
  "S_b" =  SEXO[,-2]*RAÇA[,1],
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

s_SR = lm(DES8 ~ ., data = DATA)
s_SR = lm(DES8 ~ ., data = DATA, weights=1/fitted(s_SR)^2)

#h = exp(resid(lm(log(resid(s_SR)^2) ~ ., data=DATA[,-1])))
#s_SR = lm(DES8/h ~ ., data = DATA/h)

#---------- S6-N_R                    ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],
  NACI[,2:6],
  SEXO[,-2,drop=FALSE], 
  NACI[,2]*RAÇA[,2], #
#  NACI[,2]*RAÇA[,3], # asiatico no brasil
   NACI[,2]*RAÇA[,4], ##se outro for latino
   NACI[,3]*RAÇA[,2], ##pra comparar com brancos africanos
#  NACI[,3]*RAÇA[,3],      #
   NACI[,3]*RAÇA[,4], ##
###  NACI[,4]*RAÇA[,2], não tem
   NACI[,4]*RAÇA[,3],       #
##  NACI[,4]*RAÇA[,4], ##
#  NACI[,5]*RAÇA[,2], #asiático c/ negro #
   NACI[,5]*RAÇA[,3],  ##P/ COMPARAR COM NÃO ASIÁTICOS
#  NACI[,5]*RAÇA[,4], 
#  NACI[,6]*RAÇA[,2],
   NACI[,6]*RAÇA[,3],
#  NACI[,6]*RAÇA[,4],
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

s_NR = lm(DES8 ~ ., data = DATA)
s_NR = lm(DES8 ~ ., data = DATA, weights=1/fitted(s_NR)^2)

#h = exp(resid(lm(log(resid(s_NR)^2) ~ ., data=DATA[,-1])))
#s_NR = lm(DES8/h ~ ., data = DATA/h)

#---------- S6-N_R_S                  ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE],
  NACI[,2:6],
  SEXO[,-2]*NACI[,2]*RAÇA[,2],
  SEXO[,-2]*NACI[,2]*RAÇA[,3],
 SEXO[,-2]*NACI[,2]*RAÇA[,4],
  SEXO[,-2]*NACI[,3]*RAÇA[,2],
 SEXO[,-2]*NACI[,3]*RAÇA[,3],
  SEXO[,-2]*NACI[,3]*RAÇA[,4],
  SEXO[,-2]*NACI[,4]*RAÇA[,2],
SEXO[,-2]*NACI[,4]*RAÇA[,3],
  SEXO[,-2]*NACI[,4]*RAÇA[,4],
 SEXO[,-2]*NACI[,5]*RAÇA[,2],
  SEXO[,-2]*NACI[,5]*RAÇA[,3],
  SEXO[,-2]*NACI[,5]*RAÇA[,4],
 SEXO[,-2]*NACI[,6]*RAÇA[,2],
 SEXO[,-2]*NACI[,6]*RAÇA[,3],
 SEXO[,-2]*NACI[,6]*RAÇA[,4],
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

s_NRS = lm(DES8 ~ ., data = DATA)
s_NRS = lm(DES8 ~ ., data = DATA, weights=1/fitted(s_NRS)^2)

#h = exp(resid(lm(log(resid(s_NRS)^2) ~ ., data=DATA[,-1])))
#s_NRS = lm(DES8/h ~ ., data = DATA/h)

#---------- S6-N_R_S c/I              ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE],
  NACI[,2:6],
  SEXO[,-2]*IDAD,
  SEXO[,-2]*IDAD²,
  NACI[,2]*IDAD,
  NACI[,2]*IDAD²,
  NACI[,3]*IDAD,
  NACI[,3]*IDAD²,
  NACI[,4]*IDAD,
  NACI[,4]*IDAD²,
  NACI[,5]*IDAD,
  NACI[,5]*IDAD²,
  NACI[,6]*IDAD,
  NACI[,6]*IDAD²,
  RAÇA[,2]*IDAD,
  RAÇA[,2]*IDAD²,
  RAÇA[,3]*IDAD,
  RAÇA[,3]*IDAD²,
  RAÇA[,4]*IDAD,
  RAÇA[,4]*IDAD²,
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

colnames(DATA) = c(colnames(DATA[1:10]),"S","S²","am","am²","af","af²","eu","eu²","as","as²","ou","ou²","n","n²","a","a²","o","o²",colnames(DATA)[29:96])

s_cI = lm(DES8 ~ ., data = DATA)
s_cI = lm(DES8 ~ ., data = DATA, weights=1/fitted(s_cI)^2)

#h = exp(resid(lm(log(resid(s_cI)^2) ~ ., data=DATA[,-1])))
#s_cI = lm(DES8/h ~ ., data = DATA/h)

#---------- S6-N_R_S c/R              ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],            
  SEXO[,-2,drop=FALSE],
  NACI[,2:6],
  SEXO[,-2]*RENDl,
  NACI[,2]*RENDl,
  NACI[,3]*RENDl,
  NACI[,4]*RENDl,
  NACI[,5]*RENDl,
  NACI[,6]*RENDl,
  RAÇA[,2]*RENDl,
  RAÇA[,3]*RENDl,
  RAÇA[,4]*RENDl,
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

colnames(DATA) = c(colnames(DATA[1:10]),c("S","S²","am","am²","af","af²","eu","eu²","as","as²","ou","ou²","n","n²","a","a²","o","o²")[seq(1,18,by=2)],colnames(DATA)[20:87])

s_cR = lm(DES8 ~ ., data = DATA)
s_cR = lm(DES8 ~ ., data = DATA, weights=1/fitted(s_cR)^2)

#h = exp(resid(lm(log(resid(s_cR)^2) ~ ., data=DATA[,-1])))
#s_cR = lm(DES8/h ~ ., data = DATA/h)

#---------- S6-S_F e S_C              ----------
DATA = cbind(
  DES8,
  RAÇA[,-1],
  NACI[,2:6],
  SEXO[,-2,drop=FALSE], 
  "S_n" =  SEXO[,-2]*FILHO,
  "S_a" =  SEXO[,-2]*CASD[,-2],
  EDUC[,-1],INGL[,-1],IDAD,IDAD²,RENDl,CASD[,-2,drop=FALSE],FILHO,METR,PROG,RESI)

s_SFC = lm(DES8 ~ ., data = DATA)
s_SFC = lm(DES8 ~ ., data = DATA, weights=1/fitted(s_SFC)^2)

#h = exp(resid(lm(log(resid(s_SFC)^2) ~ ., data=DATA[,-1])))
#s_SFC = lm(DES8/h ~ ., data = DATA/h)

#---------- COMPARAÇÃO                ----------
round(summary(s6)$coefficients[1:19,c(1,4)],2)

round(summary(s_SR)$coefficients [,c(1,4)],2)
round(summary(s_NR)$coefficients [,c(1,4)],2)
round(summary(s_NRS)$coefficients[,c(1,4)],2)
round(summary(s_cI)$coefficients [,c(1,4)],2)
round(summary(s_cR)$coefficients [,c(1,4)],2)
round(summary(s_SFC)$coefficients[,c(1,4)],2)


#########################################

#########################################X
#========== GRÁFICOS                  ==========
#---------- plots pelo tempo          ----------
graph = matrix(1,nrow=21*4,ncol=5)

fd = t(vector(length=1))
for(i in 1:4){fd = rbind(fd,nrow(na.omit(subset(DATA,SIPP5$SWAVE==(i)))-80))}

for(j in 1:21){
  graph[(4*j-3):(4*j),1] = c(rownames(s2[j,]),rownames(s2[j,]),rownames(s2[j,]),rownames(s2[j,]))
  graph[(4*j-3):(4*j),2] = c(2013:2016)
  graph[(4*j-3):(4*j),3] = as.numeric(s2[j,c(1,3,5,7)])
  graph[(4*j-3):(4*j),4] = as.numeric(s2[j,c(2,4,6,8)])
  graph[(4*j-3):(4*j),5] = fd[-1,]}
    
graph = as.data.frame(graph)
colnames(graph) = c("Estimador","Ano","Valor","DP","GL")
for(i in 3:5){graph[,i] = as.numeric(as.character(graph[,i]))}

qplot(Ano,Valor,data=graph[c(-(1:4),-(17:44),-(49:68)),],color=Estimador) +
  geom_hline(yintercept=0) +
  geom_line(aes(group = Estimador)) +
  geom_errorbar(aes(ymin = Valor-abs(qt(0.95, GL))*DP, ymax = Valor+abs(qt(0.95, GL))*DP),
                width = 0.25*graph[c(-(1:4),-(17:44),-(49:68)),4]+0.1,
                size  = 0.2*graph[c(-(1:4),-(17:44),-(49:68)),4])
                #alpha = minmax_01(graph[c(-(1:4),-(17:44),-(49:68)),4]))

#---------- raça pelo tempo           ----------
qplot(Ano,Valor,data=graph[5:16,],color=Estimador) +
  geom_hline(yintercept=0) +
  geom_line(aes(group = Estimador)) +
  geom_errorbar(aes(ymin = Valor-abs(qt(0.95, GL))*DP, ymax = Valor+abs(qt(0.95, GL))*DP),
                width = 0.25*graph[5:16,4]+0.1,
                size  = 0.2* graph[5:16,4])

#---------- sexo pelo tempo           ----------
qplot(Ano,Valor,data=graph[45:48,],color=Estimador) +
  geom_hline(yintercept=0) +
  geom_line(aes(group = Estimador)) +
  geom_errorbar(aes(ymin = Valor-abs(qt(0.95, GL))*DP, ymax = Valor+abs(qt(0.95, GL))*DP),
                width = 0.25*graph[45:48,4]+0.1,
                size  = 0.2* graph[45:48,4])

#---------- nacionalidade pelo tempo  ----------
qplot(Ano,Valor,data=graph[69:84,],color=Estimador) +
  geom_hline(yintercept=0) +
  geom_line(aes(group = Estimador)) +
  geom_errorbar(aes(ymin = Valor-abs(qt(0.95, GL))*DP, ymax = Valor+abs(qt(0.95, GL))*DP),
                width = 0.25*graph[69:84,4]+0.1,
                size  = 0.2* graph[69:84,4])


minmax_01 = function(x) {
  for(i in 1:length(x)){x[i]=ifelse(x[i]<0,0,ifelse(x[i]>1,1,x[i]))}
  print(x)}

#---------- cidadania pelo tempo      ----------
qplot(Ano,Valor,data=graph[69:84,],color=Estimador) +
  geom_hline(yintercept=0) +
  geom_line(aes(group = Estimador)) +
  geom_errorbar(aes(ymin = Valor-abs(qt(0.95, GL))*DP, ymax = Valor+abs(qt(0.95, GL))*DP),
                width = 0.25*graph[69:84,4]+0.1,
                size  = 0.2* graph[69:84,4])


minmax_01 = function(x) {
  for(i in 1:length(x)){x[i]=ifelse(x[i]<0,0,ifelse(x[i]>1,1,x[i]))}
  print(x)}

#########################################

#########################################X
#========== TESTES                    ==========
#---------- F das EXTRAS              ----------
H0 = colnames(cbind(RENDl,CASD[,-2,drop=FALSE],FILHO,CITI,METR,PROG))
linearHypothesis(s6,H0)
remove(H0)

#---------- hétero                    ----------
#bptest(s6, ~fitted(s6) + I(fitted(s6)^2))
bptest(s6)

#gráfico que ajuda ver hetero
qplot(fitted(s6),resid(s6))

#correção de white
vcovHC(s6, "HC1")
coeftest(s6, vcov.=vcovHC(s6, "HC1"))

#fgls
h = exp(resid(lm(log(resid(s6)^2) ~ ., data=DATA[,-1])))
s6 = lm(DES8/h ~ ., data = subset(DATA[,-1]/h,SIPP5$SWAVE==4))
round(summary(s6)$coefficients[1:26,c(1,4)],3)

View(round(vif(s6)))

#---------- dist resid                ----------
dgeometric.test(resid(s6),dnorm)
#fan.test(resid(s6),dnorm)

qplot(resid(s6),geom = 'blank')+
  xlab("Resíduos")+
  ylab("Densidade")+
  geom_histogram(aes(y = ..density..),binwidth=0.1)+
  stat_function(fun=dnorm,aes(color="Normal"),size=1.5,
                #args=list(mean=mean(density(resid(s6))$y),sd=sd(density(resid(s6))$y)))+
                 args=list(mean=mean(resid(s6)),sd=sd(resid(s6))))+
  geom_density(aes(color="Kernel"),size=1.5)+
  scale_colour_manual("Density", values = c("darkred", "darkblue"))+
  theme(legend.position = c(0.9, 0.87))+
  xlim(-40,40)

area.between(dnorm,density(resid(s6)))

#---------- multicolia                ----------
#ggpairs(DATA[,-1])
#ggpairs(DATA[,2:26])
#ggpairs(DATA[,2:10])

cor = as.data.frame(pcor2cor(cor(DATA[,-1])))
rownames(cor) = colnames(cor) = colnames(DATA[,-1])

omcdiag(s6)

round(vif(s6))

#---------- NA nos x's                ----------
TESTE_NA = matrix(NA,nrow=82,ncol=1) #NA impostos em DES5
for(i in 1:82){
  TESTE_NA[i,1] = sum(!is.na(DES7)&!is.na(cbind(CASD,EDUC,IDAD,INGL,METR,NACI,PROG,RAÇA,REND,RESI,SEXO)[,i]))}
row.names(TESTE_NA) = colnames(cbind(CASD,EDUC,IDAD,INGL,METR,NACI,PROG,RAÇA,REND,RESI,SEXO))
View(TESTE_NA)
remove(TESTE_NA)

#---------- Desempregos               ----------
#TESTE_D1 = na.omit(cbind(DES5,DES2,SIPP5$ENJ_TLKWK1-SIPP5$ENJ_FLKWK1))
#View(subset(TESTE_D1, TESTE_D1[,1]!=TESTE_D1[,2] | TESTE_D1[,2]!=TESTE_D1[,3] | TESTE_D1[,1]!=TESTE_D1[,3]))
#remove(TESTE_D1)

#TESTE_D2 = na.omit(cbind(DES5,DES6))
#View(subset( TESTE_D2 , TESTE_D2[,1]!=TESTE_D2[,2]) & TESTE_D2[,1]!=0 )
#remove(TESTE_D2)

#########################################

#########################################X
#========== DADOS INTRO               ==========
#---------- tabela                    ----------
intro = cbind(c("Homem", "Mulher","Branco","Negro","Asiático","Outra","EUA","América","Africa","Ásia","Europa","Oceania","Casado","Solteiro","Com filhos","Sem filhos"),round(rbind(
mean(subset(DES8, SEXO$SEXm==1)),
mean(subset(DES8, SEXO$SEXf==1)),

mean(subset(DES8, RAÇA$RAÇb==1)),
mean(subset(DES8, RAÇA$RAÇn==1)),
mean(subset(DES8, RAÇA$RAÇa==1)),
mean(subset(DES8, RAÇA$RAÇo==1)),

mean(subset(DES8, NACI$NACl==1)),
mean(subset(DES8, NACI$NACam==1)),
mean(subset(DES8, NACI$NACaf==1)),
mean(subset(DES8, NACI$NACas==1)),
mean(subset(DES8, NACI$NACeu==1)),
mean(subset(DES8, NACI$NACoo==1)),

mean(subset(DES8, CASD$CASs==1)),
mean(subset(DES8, CASD$CASs==0)),

mean(subset(DES8, FILHO$FILHO==1)),
mean(subset(DES8, FILHO$FILHO==0)))))

c("Homem", "Mulher","Branco","Negro","Asiático","Outra","EUA","América","Africa","Ásia","Europa","Oceania","Casado","Solteiro","Pai","Sem filhos")

#---------- hist educ                 ----------
df = as.data.frame(cbind(c("Pré escola","Fund.","Ens. médio", "Graduação","Outros"),
c(mean(subset(DES8,EDUC$EDU1==1)),
  mean(subset(DES8,EDUC$EDU2==1)),
  mean(subset(DES8,EDUC$EDU3==1)),
  mean(subset(DES8,EDUC$EDU4==1)),
  mean(subset(DES8,EDUC$EDU5==1)))))

qplot(as.factor(df[,1]),df[,2],data=df)+
  geom_col()

remove(df)

#---------- hist idade                ----------
df = cbind.data.frame(as.numeric(rownames(tapply(DES8,IDAD,mean))),tapply(DES8,IDAD,mean))
fun = function(x) {0.5*x -0.004*x^2}


qplot(df[,1],df[,2], data=df, geom=c("point","smooth")) + 
  stat_function(fun = fun) + xlim(15,90) +
  theme(axis.title.y = element_text(vjust=3)) +
  xlab("Idade") +
  ylab("Semanas desempregadas em média")


remove(df,fun)

#---------- hist renda                ----------
fd = as.data.frame(matrix(0,nrow=80,ncol=4))
for(i in 1:80){
  fd[i,2] = mean(subset(DES8,(i*50-49)<=RENDl&RENDl>=(i*50)))
  fd[i,1] = paste(i*50-49,"-",i*50)}

#fd[,1] = factor(fd[,1], levels=unique(fd[,1]))
fd[,3] = c(rep(0,45),rep(1,80-45))
fd[,4] = seq(1,80,by=1)


qplot(fd[,4],fd[,2], data=fd) +
#, color=fd[,3]), group=fd[,3], show.legend=FALSE) + 
  theme(axis.text.x = element_text(angle = 45, vjust=0.6),
        axis.title.y = element_text(vjust=3)) +
  xlab("Faixa de renda (dólares)")+
  ylab("Semanas desempregadas em média")+
  scale_x_continuous(breaks=seq(1,80,8), label=fd[seq(1,80,8),1])
#  geom_smooth(method="lm") + 
#  geom_smooth(method="lm",group=FALSE) +
  

remove(fd)


#EDUC, IDAD, INGL, RENDt --> Histograma
#CITI, METR, PROG, RESI --> não

##########################################
#---------- nacionalidade             ----------
NACI = as.factor(ifelse(SIPP5$TBORNPLACE%in%c(1:60),"Local",
                            ifelse(SIPP5$TBORNPLACE==63,"Americano",
                                   ifelse(SIPP5$TBORNPLACE==62,"Asiático",
                                   ifelse(SIPP5$TBORNPLACE==64,"Africano",
                                          ifelse(SIPP5$TBORNPLACE==61,"Europeu",
                                                 ifelse(SIPP5$TBORNPLACE%in%c(65:66),"Outra N","Outra N")))))))

SEXO = as.factor(SIPP5$ESEX)
SEXO = ifelse(SEXO=="1","Masc","Fem")

RAÇA = as.factor(SIPP5$ERACE)
CITI = as.factor(SIPP5$ECITIZEN)
#EDUC = as.factor(SIPP5$EEDUC)
INGL = as.factor(SIPP5$EHOWWELL)
INGL[is.na(INGL)]=1
MTRO = as.factor(SIPP5$TMETRO_INTV)
RESI = as.factor(SIPP5$TST_INTV)

#EDUC = as.factor(ifelse(EDUC[,1]==1,1,
                        ifelse(EDUC[,2]==1,2,
                               ifelse(EDUC[,3]==1,3,
                                      ifelse(EDUC[,4]==1,4,
                                             ifelse(EDUC[,5]==1,5,0))))))

IDAD = as.matrix(IDAD)
IDAD² = as.matrix(IDAD²)
RENDl = as.matrix(RENDl)
PROG = as.matrix(PROG)
FILH = as.matrix(FILHO)
ESCV = as.matrix(CASD$CASs)

NACI = relevel(NACI,ref="Local")
SEXO = relevel(SEXO,ref="2")

#---------- hist renda                ----------                                             
mv = lm(DES8 ~ SEXO+RAÇA+NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI)
mv = lm(DES8 ~ SEXO+RAÇA+NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI, weights=1/fitted(mv)^2)
summary(mv)

A = as.data.frame(t(summary(mv)$coefficients))

F_RAÇn = function{}
F_RAÇa = function{}
F_RAÇo = function{}
F_RAÇb = function{}

#---------- hist renda                ----------
mv = lm(DES8 ~ SEXO*RAÇA+NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI)
mv = lm(DES8 ~ SEXO*RAÇA+NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI, weights=1/fitted(mv)^2)
A = as.data.frame(t(summary(mv)$coefficients))[1,]

F_RAÇn_H = function(x){A$`(Intercept)`[1] + A$RAÇA2[1] + x*A$SEXO1[1] + x*A$`SEXO1:RAÇA2`[1]}
F_RAÇa_H = function(x){A$`(Intercept)`[1] + A$RAÇA2[1] + x*A$SEXO1[1] + x*A$`SEXO1:RAÇA3`[1]}
F_RAÇo_H = function(x){A$`(Intercept)`[1] + A$RAÇA2[1] + x*A$SEXO1[1] + x*A$`SEXO1:RAÇA4`[1]}
F_RAÇb_H = function(x){A$`(Intercept)`[1] + x*A$SEXO1[1]}
#F_RAÇn_M = function(x){A$`(Intercept)`[1] + A$RAÇA2[1] + x*A$`SEXO1:RAÇA2`[1]}
#F_RAÇa_M = function(x){A$`(Intercept)`[1] + A$RAÇA2[1] + x*A$`SEXO1:RAÇA2`[1]}
#F_RAÇo_M = function(x){A$`(Intercept)`[1] + A$RAÇA2[1] + x*A$`SEXO1:RAÇA2`[1]}
#F_RAÇb_M = function(x){A$`(Intercept)`[1]}

SEXO = ifelse(SEXO==1,"Masc","Fem")

qplot(y=DES8, x=SEXO, geom="blank")+
  stat_function(fun=F_RAÇn_H, size=2, aes(color="Negros"))+
  stat_function(fun=F_RAÇa_H, size=2, aes(color="Asiáticos"))+
# stat_function(fun=F_RAÇo_H, size=2, color="green")+
  stat_function(fun=F_RAÇb_H, size=2, aes(color="Brancos"))+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=13),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))+
  scale_colour_manual("Raças", values = c("darkred", "darkblue", "darkmagenta"))+
  ylim(2.5,7.5)

#---------- raça e naci               ----------
mv = lm(DES8 ~ SEXO+RAÇA*NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI)
mv = lm(DES8 ~ SEXO+RAÇA*NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI, weights=1/fitted(mv)^2)
summary(mv)

#---------- hist renda                ----------
mv = lm(DES8 ~ SEXO*RAÇA*NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI)
mv = lm(DES8 ~ SEXO*RAÇA*NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI, weights=1/fitted(mv)^2))
summary(mv)

#---------- idad                      ----------
mv = lm(DES8 ~ SEXO*IDAD+RAÇA*IDAD+NACI*IDAD+SEXO*IDAD²+RAÇA*IDAD²+NACI*IDAD²+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI)
mv = lm(DES8 ~ SEXO*IDAD+RAÇA*IDAD+NACI*IDAD+SEXO*IDAD²+RAÇA*IDAD²+NACI*IDAD²+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI, weights=1/fitted(mv)^2)
summary(mv)

A = as.data.frame(t(summary(mv)$coefficients))[1,]

F_RAÇn  = function(x){A$`(Intercept)` + A$RAÇA2 + x*(A$`IDAD:RAÇA2`+A$IDAD) + (x^2)*(A$`RAÇA2:IDAD²`+A$IDAD²)}
F_RAÇa  = function(x){A$`(Intercept)` + A$RAÇA3 + x*(A$`IDAD:RAÇA3`+A$IDAD) + (x^2)*(A$`RAÇA3:IDAD²`+A$IDAD²)}
F_RAÇo  = function(x){A$`(Intercept)` + A$RAÇA4 + x*(A$`IDAD:RAÇA4`+A$IDAD) + (x^2)*(A$`RAÇA4:IDAD²`+A$IDAD²)}
F_RAÇb  = function(x){A$`(Intercept)` + x*A$RAÇA2 + (x^2)*A$IDAD²}
F_NACam = function(x){}
F_NACaf = function(x){}
F_NACeu = function(x){}
F_NACas = function(x){}
F_NACou = function(x){}
F_NAClc = function(x){}
F_SEXf  = function(x){}
F_SEXm  = function(x){}

qplot(y=DES8, x=IDAD, geom="blank")+
  stat_function(fun=F_RAÇn, size=1.5, aes(color="Negros"))+
  stat_function(fun=F_RAÇa, size=1.5, aes(color="Asiáticos"))+
  stat_function(fun=F_RAÇo, size=1.5, color="green")+
  stat_function(fun=F_RAÇb, size=1.5, aes(color="Brancos"))+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=13),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))+
  scale_colour_manual("Density", values = c("darkred", "darkblue", "darkmagenta"))+
  ylim(-10,30) + xlim(15,90)

#---------- hist renda                ----------
mv = lm(DES8 ~ SEXO*RENDl+RAÇA*RENDl+NACI*RENDl+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI)
mv = lm(DES8 ~ SEXO*RENDl+RAÇA*RENDl+NACI*RENDl+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI, weights=1/fitted(mv)^2))
summary(mv)
A = as.data.frame(t(summary(mv)$coefficients))

F_RAÇn  = function(x){A$`(Intercept)`[1] + A$RAÇA2[1] + x*A$`RENDl:RAÇA2`[1] + x*A$RENDl[1]}
F_RAÇa  = function(x){A$`(Intercept)`[1] + A$RAÇA3[1] + x*A$`RENDl:RAÇA3`[1] + x*A$RENDl[1]}
F_RAÇo  = function(x){A$`(Intercept)`[1] + A$RAÇA4[1] + x*A$`RENDl:RAÇA4`[1] + x*A$RENDl[1]}
F_RAÇb  = function(x){A$`(Intercept)`[1] + x*A$RENDl[1]}
F_NACam = function(x){A$`(Intercept)`[1] + A$NACIAmericano[1] + x*A$`RENDl:NACIAmericano`[1] + x*A$RENDl[1]}
F_NACaf = function(x){A$`(Intercept)`[1] + A$NACIAfricano[1] + x*A$`RENDl:NACIAfricano`[1] + x*A$RENDl[1]}
F_NACeu = function(x){A$`(Intercept)`[1] + A$NACIEuropeu[1] + x*A$`RENDl:NACIEuropeu`[1] + x*A$RENDl[1]}
F_NACas = function(x){A$`(Intercept)`[1] + A$NACIAsiático[1] + x*A$`RENDl:NACIAsiático`[1] + x*A$RENDl[1]}
F_NACou = function(x){A$`(Intercept)`[1] + A$`NACIOutra N`[1] + x*A$`RENDl:NACIOutra N`[1] + x*A$RENDl[1]}
F_NAClc = function(x){A$`(Intercept)`[1] + x*A$RENDl[1]}
F_SEXf  = function(x){A$`(Intercept)`[1] + A$SEXO2[1] + x*A$`SEXO2:RENDl`[1] + x*A$RENDl[1]}
F_SEXm  = function(x){A$`(Intercept)`[1] + x*A$RENDl[1]}

qplot(y=DES8, x=RENDl, geom="blank")+
  stat_function(fun=F_RAÇn, size=1, color="black")+
  stat_function(fun=F_RAÇa, size=1, color="yellow")+
  stat_function(fun=F_RAÇo, size=1, color="green")+
  stat_function(fun=F_RAÇb, size=1, color="grey")+
  ylim(0,55)+xlim(0,11000)



#---------- sexo e filho e casdo      ----------
mv = lm(DES8 ~ SEXO*FILH+SEXO*ESCV+RAÇA+NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI)
mv = lm(DES8 ~ SEXO*FILH+SEXO*ESCV+RAÇA+NACI+EDUC+INGL+IDAD+IDAD²+ESCV+CITI+MTRO+RENDl+PROG+FILH+RESI, weights=1/fitted(mv)^2)
summary(mv)


















