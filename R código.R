#########################################
#############    PRELIMINAR           ##########
options(max.print=1000000)
#=========    PACOTES                 ============
library(readxl)
library(tidyverse)
library(plm)
library(car)
library(lmtest)
library(stargazer)


#=========    FUNÇÕES                 ============
agegrp = function(x){
  for(i in 1:17){
    if(i*5-5 <= x[] & x <= i*5-1){a=i}}
  if(85 <= x){a=18}
  return(a)}

cummult = function(x, b){
  for(i in 2:length(x)){
    x[i] = x[i-1] + b}
  return(x)}

#========= GGPLOT                     ================
theme_update(
  panel.grid = element_line(color="darkgrey"),
  plot.background =  element_rect(fill="azure3"),
  panel.background = element_rect(fill="azure3"),
  legend.background =element_rect(fill="azure3", color="darkgrey"),
  legend.title = element_text(face="bold"),
  plot.title = element_text(face="bold"),
  axis.ticks = element_blank())

pal = wesanderson::wes_palettes$BottleRocket1

#########################################
#############    DADOS                ##########
states = c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New hampshire', 'New jersey', 'New mexico', 'New york', 'North carolina', 'North dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode island', 'South carolina', 'South dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West virginia', 'Wisconsin', 'Wyoming')
#=========   RAÇA, SEXO e IDADE       ======
Popula = list(as.data.frame(read_excel("m:/Users/Marcus/Downloads/Econo 3 - trabalho/Raça, Sexo e Idade 2000-2009.xlsx")[-(1:1197),-c(1:3,9,20)]),
              as.data.frame(read_excel("m:/Users/Marcus/Downloads/Econo 3 - trabalho/Raça, Sexo e Idade 2010-2019.xlsx")[,-c(1:4,10:12)]))

#remover 0 = total
for(i in 1:2){
  Popula[[i]][Popula[[i]]==0] = NA
  Popula[[i]] = na.omit(Popula[[i]])}

#igualar as variáveis de AGE
colnames(Popula[[2]])[5] = "AGEGRP"
breaks = cbind(c(0,4,12,Inf), c(0, 20, 60, Inf))
for(i in 1:2){
  Popula[[i]] = Popula[[i]] %>% group_by(SEX, NAME, ORIGIN, RACE,
                                         AGEGRP = cut(AGEGRP,
                                                      breaks=breaks[,i],
                                                      labels = 1:3)) %>%
    summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop') %>%
    relocate(NAME, .before = SEX)}


#preencher com linhas faltantes
a = numeric()
for(i in unique(Popula[[1]]$NAME)){
  for(j in unique(Popula[[1]]$SEX)){
    for(k in unique(Popula[[1]]$ORIGIN)){
      for(l in unique(Popula[[1]]$RACE)){
        for(m in unique(Popula[[1]]$AGEGRP)){
          a = rbind(a, c(i,j,k,l,m))}}}}}

a = as.data.frame(a)
headers.gab = apply(a, 1, function(x){paste(x, collapse=" ")})

for(i in 1:2){
  b = numeric()
  headers.pop = apply(Popula[[i]][,1:5], 1, function(x){gsub(" {2,}", " ",paste(x, collapse=" "))})
  n = ncol(Popula[[i]])
  for(j in 1:length(headers.gab)){
    if(headers.gab[j] %in% headers.pop){b = rbind(b, Popula[[i]][which(headers.pop==headers.gab[j]),6:n])}
    else{b = rbind(b, rep(0, n-5))}}
  Popula[[i]] = cbind(a, b)}

Popula = cbind(Popula[[1]], Popula[[2]][,6:ncol(Popula[[2]])])
colnames(Popula) = c("NAME", "SEX", "ORIGIN", "RACE", "AGEGRP", 2000:2019)

#Crescimento populacional
b = read.table("M:/Users/Marcus/Desktop/1999 population.txt", quote="\"", comment.char="", colClasses="character")
b$V2 = substr(b$V2, 1, 2)
d = read.delim("M:/Users/Marcus/Desktop/Novo Documento de Texto.txt", header=FALSE, colClasses="character")
for(i in 1:nrow(d)){
  b$V2[b$V2 %in% d[i,3]] = d[i,1]}

a = numeric()
for(i in unique(Popula$NAME)){
  a = rbind(a, c(i, apply(Popula[Popula$NAME==i, 6:ncol(Popula)], 2, sum)))}

c = numeric()
for(i in unique(Popula$NAME)){
  c = rbind(c, c(i, sum(as.numeric(b[b$V2==i, 6]))))}

a = cbind(c, a[,-1])
colnames(a)[1:2] = c("STATE", "1999")

b = a[,1]
for(i in 2000:2017){
  b = cbind(b, round((as.numeric(a[,paste(i)]) / as.numeric(a[,paste(i-1)]) - 1)*100, 3))}

a = b
colnames(a) = c("NAME", 2000:2017)
a = pivot_longer(as.data.frame(a), -1, values_to="Crescimento", names_to="YEAR")

a$NAME = str_to_sentence(a$NAME)
a = a[a$NAME %in% states,]
Crescimento = a

npop = numeric()
for(i in unique(Popula$NAME)){
  npop = rbind(npop, c(i, apply(Popula[Popula$NAME==i, 6:ncol(Popula)], 2, sum)))}

npop = as.data.frame(npop)
npop[,-1] = apply(npop[,-1], 2, function(x){as.numeric(as.character(x))})
npop$V1 = str_to_sentence(npop$V1)
npop = npop[npop$V1%in%states, -which(as.numeric(colnames(npop))>=2018)]

#Calculando as porcentagens
a = numeric()
for(i in unique(Popula$NAME)){
#  total = apply(Popula[Popula$NAME==i, 6:ncol(Popula)], 2, sum)
  b = numeric()
  for(j in c("SEX", "ORIGIN")){
    b = cbind(b, apply(Popula[Popula$NAME==i & Popula[[j]]==1, 6:ncol(Popula)], 2, sum))}
  for(j in unique(Popula$RACE)[-1]){
    b = cbind(b, apply(Popula[Popula$NAME==i & Popula$RACE==j, 6:ncol(Popula)], 2, sum))}
  for(j in 2:3){
    b = cbind(b, apply(Popula[Popula$NAME==i & Popula$AGEGRP==j, 6:ncol(Popula)], 2, sum))}
#  b = apply(b, 2, function(x){x/total})
  b = data.frame(i, 2000:2019, b)
  a = rbind(a, b)}

colnames(a) = c("NAME", "TIME", "SEX", "ORIGIN", paste0("RACE",1:5), paste0("AGE", 2:3))
rownames(a) = NULL

a$NAME = str_to_sentence(a$NAME)
a = a[a$TIME %in% 2000:2017,]
a = a[a$NAME %in% states,]

Popula = a
colnames(Popula)[3:11] = str_to_sentence(colnames(Popula)[3:11])
rm(j,k,l,total,m,n,a,b,breaks, headers.gap, headers.pop)


#=========   POPULAÇÃO                ==========
TotalPop = pivot_longer(npop, cols=-1)
colnames(TotalPop) = c("State", "Year", "População")
#rm(TotalPop)

#=========   POLÍTICA                 ==========
Politica = list()
for(i in c("Governador", "Senado", "House")){
  Politica[[i]] = read_excel("m:/Users/Marcus/Downloads/politicosxlsx.xlsx", sheet=i)
  Politica[[i]] = pivot_longer(Politica[[i]], cols=-1)
  colnames(Politica[[i]]) = c("State", "Year", i)}

Politica = data.frame(Politica[[1]],
                      Senado=Politica[[2]]$Senado,
                      House=Politica[[3]]$House)
Politica$State = as.factor(Politica$State)
Politica$Year = as.numeric(Politica$Year)
Politica$Governador = as.factor(Politica$Governador)

Politica$State = str_to_sentence(Politica$State)

corr = cbind(c("Lousiana", "Mississipi", "West virgnia"),
             c("Louisiana", "Mississippi", "West virginia"))
for(i in 1:nrow(corr)){
  Politica$State[Politica$State==corr[i,1]] = corr[i,2]}

Politica = Politica[Politica$State %in% states, ]
Politica = Politica[Politica$Year %in% 2000:2017, ]

cond = Politica$House %in% c("NA","NP") | Politica$Senado %in% c("NA","NP")

Politica$Same = ifelse(as.character(Politica$House)==as.character(Politica$Senado) &
                as.character(Politica$House)==as.character(Politica$Governador), "Equal", "Diff")
Politica$Same[cond] = "NP"
Politica$Same = as.factor(Politica$Same)


a = read_excel("m:/Users/Marcus/Downloads/presidaas.xlsx")
a = a[names(a) %in% 2000:2017]
a = unlist(rep(a, 50))

Politica$Presid.Gov = ifelse(Politica$Governador == a, "Equal", "Diff")

#=========   EMPREGO                  ==========
Employ = read_excel("M:/Users/Marcus/Downloads/ststdsadata.xlsx")

Employ = Employ[Employ$Year%in%2000:2017 & Employ$State%in%states,]

a = numeric()
k=1
for(i in states){
  for(j in 2000:2017){
    a[k] = mean(Employ$Unemployment[Employ$State==i & Employ$Year==j])
    k = k+1}}

Employ = data.frame(State=states, Year=2000:2017, Unemployment=a)

#=========   IMPOSTO                  ==========
Imposto = read_excel("M:/Users/Marcus/Downloads/STC_Historical_DB (2019).xls")
d = read.delim("M:/Users/Marcus/Desktop/Novo Documento de Texto.txt", header=FALSE, colClasses="character")

b = Imposto
b$Name = substr(b$Name, 1, 2)

for(i in 1:nrow(d)){
  b$Name[b$Name %in% d[i,2]] = d[i,1]}

b$Name = str_to_sentence(b$Name)
b = b[b$Name %in% states,]
b = b[b$Year %in% 2000:2017,]

b = b %>% relocate(Name, .before = Year)

Imposto = b

#=========   ESCOLARIDADE             ==========
Educ = as.data.frame(read_excel("m:/Users/Marcus/Downloads/Escolaridade.xlsx", sheet=2, col_names=FALSE)[-1,1:29])[,-2]

j=1
for(i in unique(c(Educ[1,]))){
  while(ifelse(j<=28,is.na(Educ[1,j]),FALSE)){
    Educ[1,j] = i
    j = j+1}
  if(ifelse(j<=28,is.na(Educ[2,j]),FALSE)){Educ[2,j]="Total"}
  j = j+1}

Educ[1,] = substr(Educ[1,], 12, nchar(Educ[1,]))
Educ[1,] = paste(Educ[1,], Educ[2,], sep="::")
colnames(Educ) = Educ[1,]
Educ = Educ[-c(1,2),]

colnames(Educ)[1] = "State"
Educ$State = str_to_sentence(Educ$State)
for(i in 1:53){
  Educ[i*19-18+2,1] = Educ[i*19-18,1]}

Educ = Educ[(1:53)*19-18+2,]
Educ = Educ[Educ$State %in% states,]

Educ = data.frame(State = Educ$State,
               Year = rep(2000:2017, each=50),
               Educ = Educ[,-1])

#Educ = Educ[,c(1,2, grep("Total", colnames(Educ)))]
Educ = Educ[,-grep("Total", colnames(Educ))]

#=========   GASTO ESTADUAL           ========
GasEst = list()
GasEst2 = character()
path = "m:/Users/Marcus/Downloads/Econo 3 - trabalho/Gasto Estadual/"
L = (37:50)[-which(37:50%in% c(42,48))]

for(i in 2017:1997){
  #i=1998
  filetype = ifelse(i>=2012, ".xlsx", ".xls")
  lines = if(i>=2012){c(1, L)} else if(i>=2004){c(3, 6+L)} else if(i>=2002){c(3, 8+L)}
  else if(i>=2000){c(3, 9+L)} else if(i>=1998){c(4, 97, 107, 111,	113, 118, 126, 128,	133, 135, 144:147, 148, 152)}
  else{c(4, 97, 104, 108, 109, 114, 121, 123, 127, 128, 134:137, 138, 142)}
  columns = if(i>=2004){1:52} else if(i>=2002){c(1, 1 + which(!1:102%%2==0))}
            else if(i>=2000){c(1, 1 + which(3:153%%3==0))} else if(i>=1998){c(1, 1 + which(5:134%%5==0))}
            else{c(4, 5 + which(3:158%%3==0))}
  
  if(i %in% c(2017:2000,1997)){
    name = paste0("GasEst",i)
    GasEst[[name]] = as.data.frame(read_excel(paste0(path, "Gasto Estadual ", i, filetype),
                                            col_names=FALSE)[lines,columns])}
  else{
    for(j in 1:2){
      name = paste0("GasEst",i,".",j)
      GasEst[[name]] = as.data.frame(read_excel(paste0(path, "Gasto Estadual ", i, " ", j, filetype),
                                                col_names=FALSE)[lines,columns])}
    GasEst[[paste0("GasEst",i)]] = cbind(GasEst[[paste0("GasEst",i,".",1)]],
                                         GasEst[[name]][,-1])
    name = paste0("GasEst",i)}
 
  if(i %in% 1999:1997){
    GasEst[[name]][1,1] = "Item"
    GasEst[[name]][11,-1] = apply(GasEst[[name]][11:15,-1], 2, function(x){sum(as.numeric(paste(x)))})
    GasEst[[name]] = GasEst[[name]][-(12:14),]
    GasEst[[name]][11,1] = "Government administration"
    GasEst[[name]][12,1] = "Interest on general debt"}
  
  GasEst[[name]] = t(GasEst[[name]])
  GasEst[[name]] = cbind(GasEst[[name]], i)
  
  colnames(GasEst[[name]]) = c("STATE", GasEst$GasEst2017[1,c(-1,-14)], "YEAR")
  GasEst[[name]] = GasEst[[name]][-1,]
  
  GasEst2 = rbind(GasEst2, GasEst[[name]])}

GasEst2 = GasEst2[,c(1,14,2:13)]
GasEst2 = as.data.frame(GasEst2)
GasEst2[,3:14] = apply(GasEst2[,3:14], 2, as.numeric)/1000
GasEst2[is.na(GasEst2)] = 0

GasEst2$Health = GasEst2$Health + GasEst2$Hospitals
GasEst2$Hospitals = NULL

GasEst2$Police = GasEst2$`Police protection` + GasEst2$Correction
GasEst2$Correction = GasEst2$`Police protection` = NULL

GasEst2$PublicWelfare = GasEst2$`Public welfare` + GasEst2$`Parks and recreation`
GasEst2$`Public welfare` = GasEst2$`Parks and recreation` = NULL

GasEst2$Other = GasEst2$`Other and unallocable` + GasEst2$`Governmental administration` + GasEst2$`Interest on general debt`
GasEst2$`Other and unallocable` = GasEst2$`Governmental administration` = GasEst2$`Interest on general debt` = NULL

rownames(GasEst2) = NULL
GasEst2$STATE = str_to_sentence(GasEst2$STATE)
GasEst2 = GasEst2[GasEst2$STATE%in%states,]

GasEst = GasEst2 %>% relocate(STATE, .before = YEAR)

colnames(GasEst)[3:9] = c("EducationEST", "HealthEST", "HighwaysEST","NaturalResourcesEST", "PoliceEST", "PublicWelfareEST", "OtherEST")

a = GasEst[GasEst$YEAR%in%2000:20017,1:2]
for(i in 0:3){
  a = cbind(a, GasEst[GasEst$YEAR %in% (2000-i):(2017-i), 3:9])}

colnames(a) = c("STATE", "YEAR", colnames(GasEst)[-(1:2)],
                paste0(colnames(GasEst)[-(1:2)], ".L", rep(1:3, each=7)))
GasEst = a

rm(GasEst2, columns, filetype, L, lines, name, path, a)




#=========   GASTO FEDERAL            =========
GasFed = as.data.frame(read_excel("m:/Users/Marcus/Downloads/Econo 3 - trabalho/Gasto Federal 1991-2020.xlsm"))
GasFed[is.na(GasFed)] = 0

#GasFed$EducationFED = apply(GasFed[,colnames(GasFed) %in% c("ELSED_FF", "HGRED_FF", "ELCAP_FF", "HEDCP_FF")],1,sum)
GasFed$ElementEducFED = GasFed$ELSED_FF + GasFed$`ELCAP FF`
GasFed$HighEducFED = GasFed$HGRED_FF + GasFed$HEDCP_FF
GasFed$TransportationFED = GasFed$TRANS_FF + as.numeric(GasFed$TRCAP_FF)
GasFed$HealthFED = GasFed$MCAID_FF
GasFed$PoliceFED = GasFed$CORCP_FF + GasFed$CORR_FF
#GasFed$EnviromentFED = GasFed$ENVCP_FF
GasFed$FundsFED = GasFed$OTHCP_FF + GasFed$HSCAP_FF + GasFed$OTHER_FF + GasFed$OTCA_FF + GasFed$TANF_FF


GasFed = GasFed[,c(1, 2, which(grepl("FED", colnames(GasFed))))]
GasFed[,3:ncol(GasFed)] = GasFed[,3:ncol(GasFed)]*1000

a = GasFed[GasFed$YEAR %in% (2000):(2017),1:2]
for(i in 0:3){
  a = cbind(a, GasFed[GasFed$YEAR %in% (2000-i):(2017-i), 3:ncol(GasFed)])}

colnames(a) = c("YEAR", "STATE", colnames(GasFed)[-(1:2)],
                paste0(colnames(GasFed)[-(1:2)], ".L", rep(1:3, each=ncol(GasFed)-2)))

GasFed = a
GasFed = GasFed %>% relocate(STATE, .before = YEAR)

GasFed$STATE = str_to_sentence(GasFed$STATE)
GasFed = GasFed[GasFed$STATE %in% states,]


#=========   GASTO ESTADUAL 2         =========
GasEst = as.data.frame(read_excel("m:/Users/Marcus/Downloads/Econo 3 - trabalho/Gasto Federal 1991-2020.xlsm"))
GasEst[is.na(GasEst)] = 0

#GasEst$EducationEst = apply(GasEst[,colnames(GasEst) %in% c("ELSED_GF", "HGRED_GF", "ELCAP_GF", "HEDCP_GF")],1,sum)
GasEst$ElementEducEST = GasEst$ELSED_GF + GasEst$`ELCAP GF`
GasEst$HighEducEST = GasEst$HGRED_GF + GasEst$HEDCP_GF
GasEst$TransportationEST = GasEst$TRANS_GF + as.numeric(GasEst$TRCAP_GF)
GasEst$HealthEST = GasEst$MCAID_GF
GasEst$PoliceEST = GasEst$CORCP_GF + GasEst$CORR_GF
#GasEst$EnviromentEst = GasEst$ENVCP_GF
GasEst$FundsEST = GasEst$OTHCP_GF + GasEst$HSCAP_GF + GasEst$OTHER_GF + GasEst$OTCA_GF + GasEst$TANF_GF


GasEst = GasEst[,c(1, 2, which(grepl("EST", colnames(GasEst))))]
GasEst[,3:ncol(GasEst)] = GasEst[,3:ncol(GasEst)]*1000

a = GasEst[GasEst$YEAR %in% (2000):(2017),1:2]
for(i in 0:3){
  a = cbind(a, GasEst[GasEst$YEAR %in% (2000-i):(2017-i), 3:ncol(GasEst)])}

colnames(a) = c("YEAR", "STATE", colnames(GasEst)[-(1:2)],
                paste0(colnames(GasEst)[-(1:2)], ".L", rep(1:3, each=ncol(GasEst)-2)))

GasEst = a
GasEst = GasEst %>% relocate(STATE, .before = YEAR)

GasEst$STATE = str_to_sentence(GasEst$STATE)
GasEst = GasEst[GasEst$STATE %in% states,]


#=========   FIRMAS CRIADAS           =========
NFirms = list()

for(i in 1997:2006){
  name = paste0("NFirms",i)
  NFirms[[name]] = as.data.frame(read_excel("m:/Users/Marcus/Downloads/Econo 3 - trabalho/Firmas Criadas 1988-2006.xlsx",
                              col_names=FALSE, sheet=paste(i)))
  
  NFirms[[name]] = NFirms[[name]][8:nrow(NFirms[[name]]),]
  colnames(NFirms[[name]]) = NFirms[[name]][1,]
  
  NFirms[[name]] = NFirms[[name]][NFirms[[name]][,2]=="Firms",]
  NFirms[[name]][,3:ncol(NFirms[[name]])] = apply(NFirms[[name]][,3:ncol(NFirms[[name]])], 2, as.numeric)

  if(i<=2004){
    NFirms[[name]][,4] = NFirms[[name]][,4] + NFirms[[name]][,5]
    NFirms[[name]][,c(5)] = NULL}
  
  NFirms[[name]] = cbind(NFirms[[name]], "YEAR"=i)
  colnames(NFirms[[name]])[4] = "0-4"
  NFirms[[name]] = NFirms[[name]][,-2]}

for(i in 2007:2017){
  name = paste0("NFirms",i)
  NFirms[[name]] = as.data.frame(read_excel("m:/Users/Marcus/Downloads/Econo 3 - trabalho/Firmas Criadas 2007-2017.xlsx",
                                            col_names=FALSE, sheet=paste(i)))[c(5,8:475), 2:4]
  
  NFirms[[name]][,1] = c("GEOGRAPHIC AREA DESCRIPTION", rep(unique(NFirms[[name]][,1])[-c(1,3)], each=9))
  NFirms[[name]] = NFirms[[name]][-which(grepl("<20|<500",NFirms[[name]]$...3)),]
  NFirms[[name]] = as.data.frame(pivot_wider(NFirms[[name]], names_from=...3, values_from=...4)[-1,-2])
  NFirms[[name]][,2:8] = apply(NFirms[[name]][,2:8], 2, as.numeric)
  
  NFirms[[name]] = cbind(NFirms[[name]], "YEAR"=i)
  colnames(NFirms[[name]]) = colnames(NFirms$NFirms2000)}

NFirms2 = numeric()
a = colnames(NFirms$NFirms2000)
for(i in 1997:2017){
  name = paste0("NFirms",i)
  NFirms2 = rbind(NFirms2, NFirms[[name]])}


NFirms2$AREA = str_to_sentence(NFirms2$AREA)
NFirms2 = NFirms2[NFirms2$AREA %in% states,]
NFirms2 = NFirms2 %>% relocate(YEAR, .after = AREA)

NFirms2$`5-19` =  NFirms2$`5-9` + NFirms2$`10-19`
NFirms2$`100+` =  NFirms2$`100-499` + NFirms2$`500+`

NFirms2 = NFirms2[,c("AREA", "YEAR", "TOTAL", "0-4","5-19","20-99","100+")]

a = NFirms2[NFirms2$YEAR %in% (2000):(2017),1:2]
for(i in 0:3){
  a = cbind(a, NFirms2[NFirms2$YEAR %in% (2000-i):(2017-i), 3:ncol(NFirms2)])}

colnames(a) = c("STATE", "YEAR", colnames(NFirms2)[-(1:2)],
                paste0(colnames(NFirms2)[-(1:2)], ".L", rep(1:3, each=ncol(NFirms2)-2)))

NFirms = a
colnames(NFirms)[-c(1,2)] = paste0("Firms", colnames(NFirms)[-c(1,2)])
colnames(NFirms)[grep("100+",colnames(NFirms))] = c("Firms100aInf", paste0("Firms100aInf.L",1:3))
colnames(NFirms) = gsub("-", "a", colnames(NFirms))

rm(a, name, NFirms2)

#=========   SALÁRIO                  ============
Salario = as.data.frame(read_excel("M:/Users/Marcus/Downloads/Econo 3 - trabalho/Salário 1984-2019.xlsx")[c(59, 61:112),1:45])

Salario = Salario[c(1,which(1:45 %% 2 == 0))][,-c(4,10)]
colnames(Salario) = Salario[1,]
Salario = Salario[-(1:2),]

Salario = pivot_longer(Salario, cols=-1)

Salario$State = str_to_sentence(Salario$State)
Salario$name = as.numeric(substr(Salario$name, 1, 4))

Salario = Salario[Salario$name %in% 2000:2017,]
Salario = Salario[Salario$State %in% states,]

colnames(Salario)[3] = "Salário"


#=========   PIB                      ==============
PIB = read.csv("M:/Users/Marcus/Downloads/Econo 3 - trabalho/PIB Por Estado.csv")[,-1]

PIB = pivot_longer(PIB, cols=-1)

PIB$GeoName = str_to_sentence(PIB$GeoName)
PIB$name = as.numeric(substr(PIB$name, 2, 5))

PIB = PIB[PIB$GeoName %in% states,]

a = character()
for(i in 0:3){
  a = cbind(a, PIB$value[PIB$name %in% (2000-i):(2017-i)])}

PIB = cbind(PIB[PIB$name%in%2000:2017,1:2], a)

colnames(PIB)[3:6] = c("PIB", paste0("PIB.L",1:3))

#=========   SALÁRIO MÍNIMO           =========
SalMin = read_excel("M:/Users/Marcus/Downloads/Econo 3 - trabalho/Salário Mínimo.xlsx")[,c(2,3,6)]

SalMin = SalMin[-which(SalMin$year<2000),]
SalMin = SalMin[-which(duplicated(SalMin[,1:2])),]

a = numeric()
for(i in unique(SalMin$statename)){
  for(j in 2000:2019){
    a = rbind(a, c(i,j))}}

a = as.data.frame(a)
headers.gab = apply(a, 1, function(x){paste(x, collapse=" ")})

b = numeric()
headers.pop = apply(SalMin[,1:2], 1, function(x){gsub(" {2,}", " ",paste(x, collapse=" "))})

for(j in 1:length(headers.gab)){
  if(headers.gab[j] %in% headers.pop){b = rbind(b, c(SalMin[which(headers.pop==headers.gab[j]),3]))}
  else{b = rbind(b, NA)}}

SalMin = cbind(a, b)
colnames(SalMin) = c("NAME", "YEAR", "value")
SalMin$YEAR = as.numeric(as.character(SalMin$YEAR))
SalMin$value = as.numeric(SalMin$value)

betas = numeric()
for(i in unique(SalMin$NAME)){
  df = SalMin[SalMin$NAME==i,]
  df$YEAR = df$YEAR - 1999
  
  mod = lm(value ~ YEAR, df)
  SalMin[SalMin$NAME==i,3] = predict(mod, newdata=data.frame(YEAR=1:20))
  
  betas[i] = mod$coefficients[2]}

for(i in names(which(betas < 0.0001))){
  SalMin[SalMin$NAME==i,3] = cummult(SalMin[SalMin$NAME==i,3], mean(betas[-which(betas < 0.0001)]))}

SalMin$NAME = str_to_sentence(SalMin$NAME)
SalMin = SalMin[SalMin$YEAR %in% 2000:2017,]
SalMin = SalMin[SalMin$NAME %in% states,]

colnames(SalMin)[3] = "Sal.Mínimo"

rm(a,b,df,mod)

#=========   POBRES                   =============
Pobres = as.data.frame(read_excel("M:/Users/Marcus/Downloads/Econo 3 - trabalho/Pobres.xlsx")[-(1:2),c(1,5)])
colnames(Pobres) = c("NAME", "POBRES")

a = Pobres[1:53,1]
index = seq(0,2226,53)
for(i in 2:length(index)){
  ind = (index[i-1]+1):index[i]
  a = cbind(a, c(Pobres[ind[1],1], Pobres[ind[-1],2]))}

colnames(a) = a[1,]
a = a[-(1:2),(1:23)[-c(4,10)]]
colnames(a) = c("NAME", 2019:2000)

Pobres = pivot_longer(as.data.frame(a), cols=-1)

Pobres$NAME = str_to_sentence(Pobres$NAME)
Pobres = Pobres[Pobres$name %in% 2000:2017,]
Pobres = Pobres[Pobres$NAME %in% states,]

colnames(Pobres)[3] = "Pobres"

rm(ind, index,j,a)

#=========   CIDADES                  =============
Cidades = as.data.frame(read_excel("M:/Users/Marcus/Downloads/Econo 3 - trabalho/Cidades.xlsx")[,c(5,6,11)])
colnames(Cidades) = c("NAME", "YEAR", "CIDADES")

a = numeric()
for(i in unique(Cidades$NAME)){
  for(j in 2000:2019){
    a = rbind(a, c(i,j))}}

a = as.data.frame(a)
headers.gab = apply(a, 1, function(x){paste(x, collapse=" ")})

b = numeric()
headers.pop = apply(Cidades[,1:2], 1, function(x){gsub(" {2,}", " ",paste(x, collapse=" "))})

for(j in 1:length(headers.gab)){
  if(headers.gab[j] %in% headers.pop){b = rbind(b, c(Cidades[which(headers.pop==headers.gab[j]),3]))}
  else{b = rbind(b, NA)}}

Cidades = cbind(a, b)
colnames(Cidades) = c("NAME", "YEAR", "CIDADES")
Cidades$YEAR = as.numeric(as.character(Cidades$YEAR))
Cidades$CIDADES = as.numeric(as.character(Cidades$CIDADES))

betas = numeric()
for(i in unique(Cidades$NAME)){
  df = Cidades[Cidades$NAME==i,]
  df$YEAR = df$YEAR - 1999
  
  mod = lm(CIDADES ~ YEAR, df)
  Cidades[Cidades$NAME==i,3] = predict(mod, newdata=data.frame(YEAR=1:20))
  
  betas[i] = mod$coefficients[2]}

for(i in names(which(betas < 0.0001))){
  Cidades[Cidades$NAME==i,3] = cummult(Cidades[Cidades$NAME==i,3], mean(betas[-which(betas < 0.0001)]))}

Cidades$NAME = str_to_sentence(Cidades$NAME)
Cidades = Cidades[Cidades$YEAR %in% 2000:2017,]
Cidades = Cidades[Cidades$NAME %in% states,]

colnames(Cidades)[3] = "Cidades"

rm(a,b,df,mod,betas,headers.gab,headers.pop,i,j)

#=========   TERRITÓRIO               =============
Terra = as.data.frame(read_excel("M:/Users/Marcus/Downloads/Econo 3 - trabalho/Terra.xlsx")[2:53,c(1,3)])

Terra = Terra[order(Terra$State),]
Terra$...3 = as.numeric(gsub(",", "", Terra$...3))

Terra$State = substr(Terra$State, 2, nchar(Terra$State))
Terra$State = str_to_sentence(Terra$State)
Terra = Terra[Terra$State %in% states,]

a=numeric()
for(i in Terra$State){
  for(j in 2000:2017)
  a = rbind(a, cbind(Terra[Terra$State==i,], j))}

Terra = a
Terra = Terra %>% relocate(j, .after = State)

colnames(Terra)[3] = "Terra"

rm(a,i,j,agegrp,cummult,Section)


#=========   CRIMES                   =============
Crimes = read_excel("m:/Users/Marcus/Downloads/Econo 3 - trabalho/Crimes.xlsx")[,-c(2,4,8,15)]

for(i in unique(Crimes$state_name)){
  index = Crimes$state_name==i
  df = data.frame(rape=Crimes$rape_legacy[index], year=1:length(Crimes$year[index]))
  mod = lm(rape ~ year, df)
  Crimes$rape_legacy[index] = predict(mod, newdata=data.frame(year=1:length(Crimes$year[index])))}

Crimes$Crime = apply(Crimes[,-c(1,2)], 1, sum)
#Crimes$Crime = apply(Crimes[,-c(1,2,7,9,10)], 1, sum)
#Crimes$Crime = apply(Crimes[,-c(1,2)][,c(7,9,10)], 1, sum)

a = Crimes[Crimes$year %in% (2000):(2017), 1:2]
for(i in 0:3){
  a = cbind(a, Crimes[Crimes$year %in% (2000-i):(2017-i), 12])}

colnames(a) = c("YEAR", "STATE", "Crime", paste0("Crime", ".L", 1:3))

Crimes = a
Crimes = Crimes %>% relocate(STATE, .before = YEAR)

Crimes$STATE = str_to_sentence(Crimes$STATE)
Crimes = Crimes[Crimes$STATE %in% states,]


#=========   BASE                     =========
rm(a,b,breaks, c, d, mod, i, a, LIST, DF, b, df, index, corr, headers.gab, headers.pop,
   j, level, remv, vars, Y, cond, dat)

LIST = mget(ls()[sapply(ls(), function(x) any(class(get(x)) == 'data.frame'))])

for(i in names(LIST)){
  LIST[[i]] = as.data.frame(LIST[[i]])
  colnames(LIST[[i]])[1:2] = c("State", "Year")
  LIST[[i]] = LIST[[i]] %>% arrange(Year) %>% arrange(State)
  if(i != "Politica"){
  LIST[[i]][,-1] = lapply(LIST[[i]][,-1], function(x){as.numeric(as.character(x))})}}
LIST$npop=NULL

level = c("Salario","SalMin","Crescimento","Pobres","Politica","TotalPop","Employ")
for(i in names(LIST)[-which(names(LIST)%in%level)]){
  for(j in 2000:2017){
    LIST[[i]][LIST[[i]]$Year==j, -c(1,2)] = LIST[[i]][LIST[[i]]$Year==j, -c(1,2)]/npop[,paste(j)]}}

#Checagem:
#a = numeric()
#for(i in names(LIST)){
#  a = rbind(a, apply(LIST[[1]][,1:2], 1, function(x){paste(x, collapse=" ")}))}
#apply(a, 2, function(x){length(unique(x)) == 1})

DF = LIST[[1]][,1:2]
for(i in LIST){
  a = colnames(DF)
  DF = cbind(DF, i[,3:ncol(i)])
  colnames(DF) = c(a, colnames(i)[3:ncol(i)])}

DF$State = as.factor(DF$State)
DF$Year = DF$Year - 1999

cond = grepl("(FED)|(EST)|(Educ)", colnames(DF))
a = colnames(DF)[which(cond)]

DF = DF[,c(colnames(DF)[which(!cond)], a[order(a)])]

#rm(list=setdiff(ls(), "DF"))
#write.csv(DF, file="m:/Users/Marcus/Downloads/Econo 3 - trabalho/BASE.csv",
#          quote=FALSE, row.names=FALSE)


#########################################
#############    MODELOS       ##########
#========== MODELO                    ==========
#colnames(NFirms)[-c(1,2)]
table = list()
eq0 = eqa = list()
for(i in colnames(NFirms)[-grep("(Firms).+(\\.L)", colnames(NFirms))][-c(1,2)]){
  #i = "FirmsTOTAL"
  Y = i
  remv = c(colnames(NFirms)[-grep(Y, colnames(NFirms))], Y, "Salário", "População")
  vars = colnames(DF)[-which(colnames(DF) %in% remv)][-c(1,2)]
  
  for(i in c("EST", "FED")){
    a = grep(paste0("Police",i), colnames(DF), value=TRUE)
    b = grep("Crime", colnames(DF), value=TRUE)
    vars = c(vars, paste(a, b, sep=":"))
    
    a = colnames(DF)[grepl("Educ", colnames(DF)) & grepl(i, colnames(DF))]
    vars = c(vars, paste(a, "as.numeric(Year)", sep=":"))}
  
  vars = vars[-grep("over", vars)]
  
  per = numeric()
  for(i in unique(DF$State)){
    ssp = spectrum(DF[DF$State==i,Y], plot=FALSE)  
    per = c(per, rep(1/ssp$freq[ssp$spec==max(ssp$spec)],18))}
  
  #vars = c(vars, "State*as.numeric(Year)", "sin(2*pi/per*as.numeric(Year))*State")
  vars = c("Year", vars)
  vars = vars[-grep("(Firms).+(\\.L)", vars)]
  #vars = vars[-which(grepl("Educ.[0-9]{2}", vars))]
  #vars = vars[-grep("FED|EST", vars)]
  #vars = c("ElementEducFED", vars)
  
  formula = paste(Y, "~")
  for(i in vars){
    formula = paste(formula, i, "+")}
  formula = substr(formula, 1, nchar(formula)-2)
  
  formula = paste(formula, "- Crime.L1 - Crime.L2 - Crime.L3")
  
  mod = plm(formula, model="within", data=DF, index=c("State", "Year"))
  summary(mod)
  
  vcov = vcovHC(mod, method="arellano")
  table[[Y]] = coeftest(mod, vcov)



#========== TESTES ==========
#---------- F conjuntos ----------


  tests = cbind(c("Age", "Race", "(Educ)(.+)(:)", "EST|FED", "EST", "FED", "Transportation",
                  "Police", "Educ[A-Z]{3}", "Funds", "Health", "GOvernador|Senador|House", ":Police"),
                c("Age", "Race", "Educ*Year", "Investment", "InvestEST", "InvestFED", "Transport",
                  "Police", "Educ", "Funds", "Saúde", "Política", "Police*Crime"))
  
  #hyp = grep("Year\\):|:sin", colnames(mod$vcov), value=TRUE)
  #hyp = grep("Year[0-9]", colnames(mod$vcov), value=TRUE)
  #i = 2
  
  for(i in 1:nrow(tests)){
    hyp = grep(tests[i,1], colnames(mod$vcov), value=TRUE)
    a = linearHypothesis(mod, hypothesis.matrix=hyp, 0, white.adjust=TRUE)
    
    eq0[[Y]] = rbind(eq0[[Y]], c(tests[i,2], paste0(a[2,1],"/",a[1,1]), round(a[2,3],3), format(a[2,4], scientific=TRUE, digits=2),
                   ifelse(a[2,4]<=0.05, "Sim", "Não")))}
  
  
  tests = c("Transportation", "Police", "Educ", "Funds", "Health")
  
  for(i in 1:length(tests)){
    a = grep(paste0(tests[i],"EST"), colnames(mod$vcov), value=TRUE)
    b = grep(paste0(tests[i],"FED"), colnames(mod$vcov), value=TRUE)
    a = linearHypothesis(mod, paste(a, "=", b))
    
    eqa[[Y]] = rbind(eqa[[Y]], c(tests[i], paste0(a[2,1],"/",a[1,1]), round(a[2,3],3), format(a[2,4], scientific=TRUE, digits=2),
                   ifelse(a[2,4]<=0.05, "Sim", "Não")))}}

stargazer(table, single.row=TRUE)

a = lapply(eq0, cbind)
b = a$FirmsTOTAL[,1]
for(i in a){b =cbind(b, i[,-1])}

a = lapply(eqa, cbind)
b = a$FirmsTOTAL[,1]
for(i in a){b =cbind(b, i[,-1])}

stargazer()

#---------- outros                    ----------
#Correlação serial
pbgtest(mod)

#Heterocedasticidade
bptest(as.formula(formula), data=DF)

#Matriz robusta
vcov = vcovHC(mod, method="arellano")

#Efeitos fixos
plmtest(as.formula(formula), DF, effect="twoways", type="bp")

#Normalidade
shapiro.test(resid(mod))

#VIF

#========== GRÁFICOS                  ==========
#---------- Tendências                ----------
a = sample(1:50, 50)
df = NFirms[,1:3] %>% 
  mutate(Grupo1 = ifelse(STATE %in% states[a[1:13]] , FirmsTOTAL, NA),
         Grupo2 = ifelse(STATE %in% states[a[14:25]], FirmsTOTAL, NA),
         Grupo3 = ifelse(STATE %in% states[a[26:38]], FirmsTOTAL, NA),
         Grupo4 = ifelse(STATE %in% states[a[39:50]], FirmsTOTAL, NA)) %>%
  gather(Grupo, FirmsTOTAL, Grupo1:Grupo4)

ggplot(df, aes(x=YEAR, y=FirmsTOTAL, color=STATE)) +
  facet_grid(~Grupo) +
  geom_line() + xlab("Ano") + ylab("Firmas criadas") +
  theme(legend.position="none") + theme(axis.text.x=element_text(angle=45))


#---------- Médias por ano            ----------
df = Rmisc::summarySE(DF[,c("Year",Y)], groupvars="Year", measurevar=Y)
ggplot(df, aes(x=Year, y=FirmsTOTAL)) + 
  geom_errorbar(aes(ymin=FirmsTOTAL-se, ymax=FirmsTOTAL+se), width=0.5) +
  geom_line(size=1) +
  geom_point() + ylab("Y") + xlab("Ano")


#---------- Resíduo vs fit            ----------
df = data.frame(Valores.Fitados=fitted(mod), "Resíduos"=resid(mod))
ggplot(df, aes(y=Resíduos, x=Valores.Fitados)) +
  geom_point() + geom_hline(yintercept=0, color=pal[1], size=1, alpha=0.8)

#spreadLevelPlot(mod)

#---------- Normalidade               ----------
df = data.frame("Resíduos"=resid(mod))
g1 = ggplot(df, aes(x=Resíduos)) +
  geom_histogram(aes(y = ..density..), alpha=0.7, color="black") +
  stat_density(fill=NA, aes(color="Estimada"), size=1) +
  stat_function(fun=dnorm, aes(color="Normal"), size=1,
                  args=list(mean=mean(resid(mod)),
                                     sd=sd(resid(mod)))) +
  scale_color_manual(values=pal[c(1,5)], name="Densidade") +
  theme(legend.position="none") + ylab("Densidade")

g2 = ggplot(mapping=aes(y=quantile(resid(mod)/sd(resid(mod)), seq(0,1,0.01)),
                        x=qnorm(seq(0,1,0.01)))) +
  geom_point(color=pal[4], alpha=0.3, size=2.5) +
  geom_line(aes(x=quantile(resid(mod)/sd(resid(mod)), seq(0,1,0.01)), color="Normal"),
            slope=1, intercept=0, size=1) +
  geom_smooth(aes(color="Estimada"), method="lm", size=1) +
  scale_color_manual(values=pal[c(1,5)], name="Densidades") + xlim(-1,1) + ylim(-1,1) +
  xlab("Quantis teóricos") + ylab("Quantis dos resíduos padronizados")

gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(1.7, 2))

#
#========== MANUAL                    ==========
#View(DF[,c("State", "Year", vars)])
detect.lindep(mod)

a = numeric()
for(i in vars){
  for(j in vars){
    a = rbind(a, c(i, j, round(cor(DF[,i],DF[,j]),2)))}}
a = a[a[,1]!=a[,2],]
View(a)

a = colnames(DF)[grepl("FED",colnames(DF)) & grepl(".L",colnames(DF)) & !grepl("Police",colnames(DF))]
remv = c(colnames(NFirms), "Salário", "População")
vars = colnames(DF)[-which(colnames(DF) %in% remv)][-c(1,2)]

formula = paste(Y, "~")
for(i in vars){
  formula = paste(formula, i, "+")
  formula = substr(formula, 1, nchar(formula)-2)
  
  print(i)
  mod = plm(formula, model="within", data=DF, index=c("State", "Year"), effect="twoways")
  summary(mod)
  
  formula = paste(formula, "+")}

mod = plm(formula, model="within", data=DF, index=c("State", "Year"), effect="twoways")
summary(mod)
