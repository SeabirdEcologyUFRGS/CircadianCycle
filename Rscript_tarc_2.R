setwd("C:/Users/00156874/Google Drive/UFRGS/Alunos/Tarcísio Linhares/Análises")
bird <- read.table(file="birdstime.csv", sep=",", h=T)
head(bird)
str(bird)
summary(bird)
dim(bird)          # to get the dimensions of the data set
names(bird)        # to get the columns names (species in our case)
row.names(bird)    # to get the row names (plots in our case)
is.na(bird[,20:65])

## REMOVENDO LINHAS E COLUNAS COM NA
bird = subset(bird, select = -c(Alt_onda,Cob_nuvens,Velocidade_vento,Direcao_vento,Temperatura))
bird = subset(bird, select = -c(X))
bird <- bird[-c(42, 51), ]
bird <- na.omit(bird)
dim(bird)  

## CALCULANDO OCORRÊNCIA E INSERINDO UMA NOVA COLUNA NA OUTRA MATRIZ
library(plyr)
spp.occ <- ldply(bird[,15:60], function(c) sum(c >0))
spp.row <- read.table(file="sp.row.csv", sep=",", h=T)
spp.row$occ <- spp.occ$V1

## CALCULANDO FREQUÊNCIA DE OCORRÊNCIA
dim(bird)  #para identificar o número de observações = 145 (i.e. 100%)
spp.row$freq_occ <- (spp.row$occ*100)/145
sort(spp.row$freq_occ) 
sort_freq_occ <- spp.row[order(spp.row[,"freq_occ"]), , drop=FALSE] #rankear
write.csv(spp.row,"sp.row.csv", row.names= TRUE)

## NÚMERO DE ESPÉCIES EM CADA CENSO
head(bird)
names(bird)
a <- rowSums(bird[,15:60]!=0)
bird$num.spp <- apply(bird[,15:60],1,function(x)sum(x != 0))

## PLOTAR NA ORDEM CRONOLÓGICA
#Riqueza
levels(bird$Time)
circa.time <- factor(bird$Time, levels=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"))
plot(bird$num.spp ~ circa.time)
circa.time.cabras <- factor(bird.cabras$Time, levels=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"))
plot(bird.cabras$num.spp ~ circa.time.cabras)
circa.time.ntdai <- factor(bird.ntdai$Time, levels=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"))
plot(bird.ntdai$num.spp ~ circa.time)

library(ggplot2)
plot.num.spp <- ggplot(bird, aes(x=circa.time, y=bird$num.spp, fill=bird$Beach), ylab="vvvv") + 
  geom_boxplot()
plot.num.spp + guides(fill=guide_legend(title=NULL))
plot.num.spp <- plot.num.spp + scale_fill_discrete(name="Beach",
                         labels=c("Cabras", "Nova Tramandaí"))
plot.num.spp <- plot.num.spp + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
plot.num.spp <- plot.num.spp + theme(legend.position=c(0.9,0.9)) + labs(x = "", y = "Number of species")
plot.num.spp

#Abundância
levels(bird$Time)
circa.time <- factor(bird$Time, levels=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"))
plot(bird$totabund ~ circa.time)
circa.time.cabras <- factor(bird.cabras$Time, levels=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"))
plot(bird.cabras$totabund ~ circa.time.cabras)
circa.time.ntdai <- factor(bird.ntdai$Time, levels=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"))
plot(bird.ntdai$totabund ~ circa.time.ntdai)

par(mfrow=c(1,2))
plot(bird.cabras$totabund ~ circa.time.cabras, xlab = "Census", ylab = "Species Richness", main = "Cabras")
plot(bird.ntdai$totabund ~ circa.time.ntdai, xlab = "Census", ylab = "Species Richness", main = "Nova Tramandaí")

par(mfrow=c(1,2))
plot(log(bird.cabras$totabund) ~ circa.time.cabras, xlab = "Census", ylab = "Total Abundance", main = "Cabras")
plot(log(bird.ntdai$totabund) ~ circa.time.ntdai, xlab = "Census", ylab = "Total Abundance", main = "Nova Tramandaí")

library(ggplot2)
plot.totabund <- ggplot(bird, aes(x=circa.time, y=log(bird$totabund), fill=bird$Beach)) + 
  geom_boxplot()
plot.totabund + guides(fill=guide_legend(title=NULL))
plot.totabund <- plot.totabund + scale_fill_discrete(name="Beach",
                               labels=c("Cabras", "Nova Tramandaí"))
plot.totabund <- plot.totabund + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
plot.totabund <- plot.totabund + theme(legend.position=c(0.9,0.9)) + labs(x = "", y = "Total Abundance (log)")
plot.totabund

## SEPARANDO AS PRAIAS
bird.cabras <- bird[which(bird$Beach=="Cabras"),]
bird.cabras
bird.ntdai <- bird[which(bird$Beach=="Nova_Tdai"),]
ntdai

## CALCULANDO FREQUÊNCIA DE OCORRÊNCIA PARA CADA PRAIA
#Cabras
names(bird.cabras)
spp.occ.cabras <- ldply(bird.cabras[,15:60], function(c) sum(c >0))
spp.row <- read.table(file="sp.row.csv", sep=",", h=T)
spp.row$occ.cabras <- spp.occ.cabras$V1
dim(bird.cabras)  #para identificar o número de observações = 68 (i.e. 100%)
spp.row$freq_occ.cabras <- (spp.row$occ.cabras*100)/68
sort(spp.row$freq_occ.cabras) 
sort_freq_occ <- spp.row[order(spp.row[,"freq_occ.cabras"]), , drop=FALSE] #rankear
spp.row[order(spp.row[,"X"]), , drop=FALSE] #rankear
write.csv(spp.row,"sp.row.csv", row.names= TRUE) # salvar

#N_Tdai
names(bird.ntdai)
spp.occ.ntdai <- ldply(bird.ntdai[,15:60], function(c) sum(c >0))
spp.row <- read.table(file="sp.row.csv", sep=",", h=T)
spp.row$occ.ntdai <- spp.occ.ntdai$V1
dim(bird.ntdai)  #para identificar o número de observações = 68 (i.e. 100%)
spp.row$freq_occ.ntdai <- (spp.row$occ.ntdai*100)/77
sort(spp.row$freq_occ.ntdai) 
sort_freq_occ <- spp.row[order(spp.row[,"freq_occ.ntdai"]), , drop=FALSE] #rankear
write.csv(spp.row,"sp.row.csv", row.names= TRUE) # salvar

## REMOVER COLUNAS INDESEJADAS - se necessário!! Cuidado!
spp.row <- spp.row[,-(1),drop=FALSE] 
write.csv(spp.row,"sp.row.csv", row.names= TRUE) # salvar

## ABUNDÂNCIA TOTAL EM CADA CENSO (fixar como uma coluna na planilha)
bird$totabund <- apply(bird[,20:65], 1, sum)

# PLOT SIMPLES
hist(bird$totabund, main = "Número de observações", xlab = "Abundância por censo", ylab = "Frequência")  # notar a distribuição não normal (típica de dados de contagem)

# PLOT FIRULA
library(tidyverse)
library(hrbrthemes)
hist.counts <- ggplot(bird, aes(x=bird$totabund)) + 
  geom_histogram(binwidth=40, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Countings distribution")
hist.counts

## ABUNDÂNCIA RELATIVA GLOBAL
head(spp.row)
sum(spp.row$tot.abund)
spp.row$tot.rel.abu <- ((spp.row$tot.abund*100)/10837) #calcular e inserir como uma nova coluna
sort_rel.abu <- spp.row[order(spp.row[,"tot.rel.abu"]), , drop=FALSE] #rankear
write.csv(spp.row,"sp.row.csv", row.names= TRUE) # salvar

## ABUNDÂNCIA RELATIVA CABRAS
head(spp.row)
sum(spp.row$ab_Cabras)
spp.row$rel.abu.cabras <- ((spp.row$ab_Cabras*100)/6594) #calcular e inserir como uma nova coluna
sort_rel.abu <- spp.row[order(spp.row[,"rel.abu.cabras"]), , drop=FALSE] #rankear
write.csv(spp.row,"sp.row.csv", row.names= TRUE) # salvar

## ABUNDÂNCIA RELATIVA NOVA TRAMANDAÍ
head(spp.row)
sum(spp.row$ab_NTdai)
spp.row$rel.abu.ntdai <- ((spp.row$ab_NTdai*100)/4243) #calcular e inserir como uma nova coluna
sort_rel.abu <- spp.row[order(spp.row[,"rel.abu.ntdai"]), , drop=FALSE] #rankear
write.csv(spp.row,"sp.row.csv", row.names= TRUE) # salvar

## SEPARANDO OS HORÁRIOS
dim(bird)
names(bird)
tail(bird)

## DIFERENÇA NAS ABUNDÂNCIAS ENTRE AS PRAIAS
par(mfrow=c(1,2))
hist(bird$totabund[which(bird$Beach=="Cabras")], xlab = "Total abundance", main = "Total abundance - Praia das Cabras")
hist(bird$totabund[which(bird$Beach=="Nova_Tdai")], xlab = "Total abundance", main = "Total abundance - Nova Tramandaí")
# notar a diferença nas distribuições

#  SEM NORMALIDADE NOS DADOS, VAMOS PARA UM TESTE NÃO PARAMÉTRICO
test.totabund.beach <- wilcox.test(bird$totabund~bird$Beach)
test.totabund.beach # não temos diferença significativa na abundância entre praias

library(ggplot2)
ggplot(bird, aes(x=as.factor(bird$Beach), y=bird$totabund)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Praias") + ylab("Abundância")

## ABUNDÂNCIA GLOBAL ESPÉCIE ESPECÍFICA
sp.abund <- apply(bird[,20:66], 2, sum, na.rm=TRUE)
bird[148,20:66] <- sp.abund
tail(bird)
# na última linha linha está a abundância global espécie específica
# na última linha da última coluna está o total de observações do estudo

## ABUNDÂNCIA GLOBAL ESPÉCIE ESPECÍFICA EM CADA PRAIA
# Cabras
sp.abund.cabras <- apply(cabras[,20:65], 2, sum, na.rm=TRUE)
sp.abund.cabras # abundância espécie-específica registrada nas Cabras
rev(sort(sp.abund.cabras)) # por ordem 

sp.abund.cabras <- as.numeric(sp.abund.cabras)
spp.reg.cabras <- sp.abund.cabras>0
summary(spp.reg.cabras) #TRUE é o total de spp. registradas nas cabras

# Nova Tramandaí
sp.abund.ntdai <- apply(ntdai[,20:65], 2, sum, na.rm=TRUE)
sp.abund.ntdai # abundância espécie-específica registrada em Nova Tramandaí
rev(sort(sp.abund.ntdai)) # por ordem 

sp.abund.ntdai <- as.numeric(sp.abund.ntdai)
spp.reg.ntdai <- sp.abund.ntdai>0
summary(spp.reg.ntdai) #TRUE é o total de spp. registradas nas cabras

## CRIAR PLANILHA COM SPP NAS LINHAS E ABUNDÂNCIAS NAS COLUNAS
sp.abund.cabras <- apply(cabras[,20:65], 2, sum, na.rm=TRUE)
sp.abund.ntdai <- apply(ntdai[,20:65], 2, sum, na.rm=TRUE)
write.csv(sp.abund.cabras,"sp.abund.cabras.csv", row.names= TRUE)
write.csv(sp.abund.ntdai,"sp.abund.ntdai.csv", row.names= TRUE)
spp_row_cabras <- read.table(file="sp.abund.cabras.csv", sep=",", h=T)
head(spp_row_cabras)
spp_row_ntdai <- read.table(file="sp.abund.ntdai.csv", sep=",", h=T)
head(spp_row_ntdai)
spp_row_ntdai$ab_Cabras = spp_row_cabras$x
names(spp_row_ntdai)[names(spp_row_ntdai) == "x"] <- "ab_NTdai"
names(spp_row_ntdai)[names(spp_row_ntdai) == "X"] <- "Species"
head(spp_row_ntdai)
spp_row_ntdai$tot.abund <- apply(spp_row_ntdai[,2:3], 1, sum)
write.csv(spp_row_ntdai,"sp.row.csv", row.names= TRUE)

log(bird$totabund)

write.csv(bird,"birdstime.csv", row.names= TRUE)

### RIDGELINE PLOT
library(ggridges)
library(ggplot2)

## TOTAL
# Abundância
ridge.ab <- ggplot(bird, aes(x = bird$totabund, y = circa.time, fill = circa.time)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ridge.ab <- ridge.ab + labs(x = "", y = "") + ggtitle("24-hour total abundance distribution") +
  theme(plot.title = element_text(hjust = 0.8))
ridge.ab

# Riqueza
ridge.rich <- ggplot(bird, aes(x = bird$num.spp, y = circa.time, fill = circa.time)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ridge.rich <- ridge.rich + labs(x = "", y = "") + ggtitle("24-hour number of species distribution") +
  theme(plot.title = element_text(hjust = 0.8))
ridge.rich

# Diversidade (Shannon)
ridge.div <- ggplot(bird, aes(x = bird$shannon.div, y = circa.time, fill = circa.time)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ridge.div <- ridge.div + labs(x = "", y = "") + ggtitle("24-hour species diversity distribution") +
  theme(plot.title = element_text(hjust = 0.8))
ridge.div

## Cabras
# Abundância
ridge.ab.cabras <- ggplot(bird.cabras, aes(x = bird.cabras$totabund, y = circa.time.cabras, fill = circa.time.cabras)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ridge.ab.cabras <- ridge.ab.cabras + labs(x = "", y = "") + ggtitle("24-hour total abundance distribution - Praia das Cabras") +
  theme(plot.title = element_text(hjust = 0.8))

# Riqueza
ridge.rich.cabras <- ggplot(bird.cabras, aes(x = bird.cabras$num.spp, y = circa.time.cabras, fill = circa.time.cabras)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ridge.rich.cabras <- ridge.rich.cabras + labs(x = "", y = "") + ggtitle("24-hour number of species distribution - Praia das Cabras") +
  theme(plot.title = element_text(hjust = 0.8))

# Diversidade (Shannon)
ridge.div.cabras <- ggplot(bird.cabras, aes(x = bird.cabras$shannon.div, y = circa.time.cabras, fill = circa.time.cabras)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ridge.div.cabras <- ridge.div.cabras + labs(x = "", y = "") + ggtitle("24-hour species diversity distribution - Praia das Cabras") +
  theme(plot.title = element_text(hjust = 0.8))
ridge.div.cabras

## Nova Tramandaí
# Abundância
ridge.ab.ntdai <- ggplot(bird.ntdai, aes(x = bird.ntdai$totabund, y = circa.time.ntdai, fill = circa.time.ntdai)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ridge.ab.ntdai <- ridge.ab.ntdai + labs(x = "", y = "") + ggtitle("24-hour total abundance distribution - Nova Tramandaí") +
  theme(plot.title = element_text(hjust = 0.8))

# Riqueza
ridge.rich.ntdai <- ggplot(bird.ntdai, aes(x = bird.ntdai$num.spp, y = circa.time.ntdai, fill = circa.time.ntdai)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ridge.rich.ntdai <- ridge.rich.ntdai + labs(x = "", y = "") + ggtitle("24-hour number of species distribution - Nova Tramandaí") +
  theme(plot.title = element_text(hjust = 0.8))

# Diversidade (Shannon)
ridge.div.ntdai <- ggplot(bird.ntdai, aes(x = bird.ntdai$shannon.div, y = circa.time.ntdai, fill = circa.time.ntdai)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ridge.div.ntdai <- ridge.div.ntdai + labs(x = "", y = "") + ggtitle("24-hour species diversity distribution - Nova Tramandaí") +
  theme(plot.title = element_text(hjust = 0.8))
ridge.div.ntdai

## Basic stats

library(vegan)
names(bird)
bird <- subset(bird, select = -X)

## DIVERSIDADE DE SHANNON
bird$shannon.div <- diversity(bird[,15:60], index = "shannon", MARGIN = 1, base = exp(1))
boxplot(bird$shannon.div~circa.time)
bird$shannon.div <- diversity(bird[,15:60], index = "shannon", MARGIN = 1, base = exp(1))
shannon.div.cabras <- diversity(bird.cabras[,15:60], index = "shannon", MARGIN = 1, base = exp(1))
shannon.div.ntdai <- diversity(bird.ntdai[,15:60], index = "shannon", MARGIN = 1, base = exp(1))
par(mfrow=c(1,2))
boxplot(shannon.div.cabras~circa.time.cabras, main = "Diversidade de Shannon - Praia das Cabras")
boxplot(shannon.div.ntdai~circa.time.ntdai, main = "Diversidade de Shannon - Nova Tramandaí")

# PLOT
bp <- ggplot(bird, aes(x=circa.time, y=log(bird$shannon.div), fill=bird$Beach)) + 
  geom_boxplot()
bp + guides(fill=guide_legend(title=NULL))
bp <- bp + scale_fill_discrete(name="Beach",
                               labels=c("Cabras", "Nova Tramandaí"))
bp <- bp + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
bp <- bp + theme(legend.position=c(0.1,0.1)) + labs(x = "", y = "Shannon's species diversity")


## DIVERSIDADE DE SIMPSON
bird$simpson.div <- diversity(bird[,15:60], index = "simpson", MARGIN = 1, base = exp(1))
boxplot(bird$simpson.div~circa.time)

## NÚMERO DE ESPÉCIES EM CADA HORÁRIO E PRAIA
library(vegan)
specnumber(bird[,15:60], bird$Time, MARGIN = 1)
specnumber(bird[,15:60], bird$Beach, MARGIN = 1)

## CURVA DE ACUMULAÇÃO DE ESPÉCIES - Function 'specaccum' finds species accumulation curves or the number of species for a certain number of sampled sites or individuals.
sac <- specaccum(bird[,15:60], method = "random")
plot(sac, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sac, col="yellow", add=TRUE, pch="+")

# Cabras
sac.cabras <- specaccum(bird.cabras[,15:60], method = "random")
plot(sac.cabras, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sac.cabras, col="yellow", add=TRUE, pch="+")

# Nova Tramandaí
sac.ntdai <- specaccum(bird.ntdai[,15:60], method = "random")
plot(sac.ntdai, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sac.ntdai, col="yellow", add=TRUE, pch="+")

########################################################################################

## REMOVING NON-WATERBIRD SPECIES

bird <- read.table(file="birdstime.csv", sep=",", h=T)
waterbird = subset(bird, select = -c(num.spp,totabund,shannon.div,simpson.div,
                                     Streptoprocne_zonaris, Tringa_sp, Columba_livia, Coragyps_atratus, Sternidae,Guira_guira,Caracara_plancus,Milvago_chimango,Furnarius_rufus, Anthus_sp, Passer_domesticus,Pitangus_sulfuratus,Xolmis_irupero, Alopochelidon_fucata,Hirundinidae,Progne_tapera,Pygochelidon_cyanoleuca,Tachycineta_leucorrhoa, Colaptes_campestris, Myiopsitta_monachus,Athene_cunicularia))
waterbird = subset(waterbird, select = -c(X, X.1))
names(waterbird)
write.csv(waterbird,"waterbirdstime.csv", row.names= TRUE)
setwd("C:/Users/00156874/Google Drive/UFRGS/Alunos/Tarcísio Linhares/Análises")
waterbird <- read.table(file="waterbirdstime.csv", sep=",", h=T)
head(waterbird)

library(plyr)
waterbird$Beach <- revalue(waterbird$Beach, c("Cabras"="Non_urban", "Nova_Tdai"="Urban"))
levels(waterbird$Beach)

# Separar praias
waterbird.cabras <- waterbird[which(waterbird$Beach=="Cabras"),]
waterbird.cabras
waterbird.ntdai <- waterbird[which(waterbird$Beach=="Nova_Tdai"),]
waterbird.ntdai

## Chamar no laptop
setwd("E:/Google Drive/UFRGS/Alunos/Tarcísio Linhares/Análises")

## NÚMERO DE ESPÉCIES EM CADA CENSO
a <- rowSums(waterbird[,15:39]!=0)
waterbird$num.spp <- apply(waterbird[,15:39],1,function(x)sum(x != 0))

## ABUNDÂNCIA TOTAL EM CADA CENSO (fixar como uma coluna na planilha)
waterbird$totabund <- apply(waterbird[,15:39], 1, sum)
hist(waterbird$totabund)

# PLOT SIMPLES
hist(waterbird$totabund, main = "Número de observações", xlab = "Abundância por censo", ylab = "Frequência")  # notar a distribuição não normal (típica de dados de contagem)

# PLOT FIRULA
library(tidyverse)
library(hrbrthemes)
hist.counts <- ggplot(waterbird, aes(x=waterbird$totabund)) + 
  geom_histogram(binwidth=40, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Countings distribution")
hist.counts

## CALCULANDO OCORRÊNCIA E INSERINDO UMA NOVA COLUNA NA OUTRA MATRIZ
library(plyr)
spp.occ <- ldply(waterbird[,15:39], function(c) sum(c >0))
write.csv(spp.occ,"waterbird.sp.row.csv", row.names= TRUE)
water.spp.row <- read.table(file="waterbird.sp.row.csv", sep=",", h=T)
water.spp.row$occ <- water.spp.row$V1
names(water.spp.row)
head(water.spp.row)
water.spp.row <- subset(water.spp.row, select = -(c(V1)))
water.spp.row$Species <- water.spp.row$.id
water.spp.row <- subset(water.spp.row, select = -(c(occ..id, X)))
water.spp.row$occ <- spp.occ
write.csv(water.spp.row,"waterbird.sp.row.csv", row.names= TRUE)
names(water.spp.row)[names(water.spp.row) == "occ.V1"] <- "occ" ## renomear
water.spp.row <- read.table(file="waterbird.sp.row.csv", sep=",", h=T)
head(water.spp.row)

water.spp.row$spp.occ.cabras <- ldply(waterbird.cabras[,15:39], function(c) sum(c >0))
water.spp.row$spp.occ.cabras <- subset(water.spp.row$spp.occ.cabras, select = -(c(.id)))
water.spp.row$spp.occ.ntdai <- ldply(waterbird.ntdai[,15:39], function(c) sum(c >0))
water.spp.row$spp.occ.ntdai <- subset(water.spp.row$spp.occ.ntdai, select = -(c(.id)))

## CALCULANDO FREQUÊNCIA DE OCORRÊNCIA
dim(waterbird)  #para identificar o número de observações = 143 (i.e. 100%)
water.spp.row$freq_occ <- (water.spp.row$occ*100)/143
sort(water.spp.row$freq_occ) 
sort_freq_occ <- water.spp.row[order(water.spp.row[,"freq_occ"]), , drop=FALSE] #rankear

dim(waterbird.cabras)  #para identificar o número de observações nas Cabras = 66 (i.e. 100%)
water.spp.row$freq_occ.cabras <- (water.spp.row$spp.occ.cabras*100)/66
dim(waterbird.ntdai)  #para identificar o número de observações em N Tramandaí = 77 (i.e. 100%)
water.spp.row$freq_occ.ntdai <- (water.spp.row$spp.occ.ntdai*100)/77
names(water.spp.row)
sort_freq_occ.cabras <- water.spp.row[order(water.spp.row[,"freq_occ.cabras"]), , drop=FALSE] #rankear
sort_freq_occ.ntdai <- water.spp.row[order(water.spp.row[,"freq_occ.ntdai"]), , drop=FALSE] #rankear

## ABUNDÂNCIA GLOBAL

water.spp.row$sp.abund <- apply(waterbird[,15:39], 2, sum, na.rm=TRUE)
sort_abund <- water.spp.row[order(water.spp.row[,"sp.abund"]), , drop=FALSE] #rankear


## ABUNDÂNCIA RELATIVA GLOBAL
head(water.spp.row)
sum(water.spp.row$sp.abund)
water.spp.row$tot.rel.abu <- ((water.spp.row$sp.abund*100)/10355) #calcular e inserir como uma nova coluna
sort_rel.abu <- water.spp.row[order(water.spp.row[,"tot.rel.abu"]), , drop=FALSE] #rankear
sum(water.spp.row$tot.rel.abu)
water.spp.row <- subset(water.spp.row, select = -(c(X)))

## ABUNDÂNCIA RELATIVA CABRAS
head(spp.row)
sum(spp.row$ab_Cabras)
spp.row$rel.abu.cabras <- ((spp.row$ab_Cabras*100)/6594) #calcular e inserir como uma nova coluna
sort_rel.abu <- spp.row[order(spp.row[,"rel.abu.cabras"]), , drop=FALSE] #rankear
write.csv(spp.row,"sp.row.csv", row.names= TRUE) # salvar

## ABUNDÂNCIA RELATIVA NOVA TRAMANDAÍ
head(spp.row)
sum(spp.row$ab_NTdai)
spp.row$rel.abu.ntdai <- ((spp.row$ab_NTdai*100)/4243) #calcular e inserir como uma nova coluna
sort_rel.abu <- spp.row[order(spp.row[,"rel.abu.ntdai"]), , drop=FALSE] #rankear
write.csv(spp.row,"sp.row.csv", row.names= TRUE) # salvar

## Basic stats

library(vegan)
names(waterbird)

#DEFININDO ORDEM CRONOLÓGICA
circa.time <- factor(waterbird$Time, levels=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"))
circa.time.cabras <- factor(waterbird.cabras$Time, levels=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"))
circa.time.ntdai <- factor(waterbird.ntdai$Time, levels=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"))

## DIVERSIDADE DE SHANNON
waterbird$shannon.div <- diversity(waterbird[,15:39], index = "shannon", MARGIN = 1, base = exp(1))
boxplot(waterbird$shannon.div~circa.time)
shannon.div.cabras <- diversity(waterbird.cabras[,15:39], index = "shannon", MARGIN = 1, base = exp(1))
shannon.div.ntdai <- diversity(waterbird.ntdai[,15:39], index = "shannon", MARGIN = 1, base = exp(1))
par(mfrow=c(1,2))
boxplot(shannon.div.cabras~circa.time.cabras, main = "Diversidade de Shannon - Praia das Cabras")
boxplot(shannon.div.ntdai~circa.time.ntdai, main = "Diversidade de Shannon - Nova Tramandaí")
mean(shannon.div.cabras)
mean(shannon.div.ntdai)
hist(waterbird$shannon.div) ## distribuição não normal = análise não paramétrica
hist(shannon.div.cabras) 
hist(shannon.div.ntdai)

## DIVERSIDADE DE SIMPSON
waterbird$simpson.div <- diversity(waterbird[,15:39], index = "simpson", MARGIN = 1, base = exp(1))
boxplot(waterbird$simpson.div~circa.time)
boxplot(waterbird$simpson.div~waterbird$Beach)
simpson.div.cabras <- diversity(waterbird.cabras[,15:39], index = "simpson", MARGIN = 1, base = exp(1))
simpson.div.ntdai <- diversity(waterbird.ntdai[,15:39], index = "simpson", MARGIN = 1, base = exp(1))
par(mfrow=c(1,2))
boxplot(simpson.div.cabras~circa.time.cabras, main = "Diversidade de Simpson - Praia das Cabras")
boxplot(simpson.div.ntdai~circa.time.ntdai, main = "Diversidade de Simpson - Nova Tramandaí")
mean(simpson.div.cabras)
mean(simpson.div.ntdai)

## Teste de diferença estatística entre praias

# Abundância

names(waterbird)
Utest.ab.praias <- wilcox.test(waterbird$totabund ~ waterbird$Beach) 
Utest.ab.praias # W = 2861, p-value = 0.1957; não há diferença significativa na abundância entre as praias

# Número de espécies

names(waterbird)
Utest.riq.praias <- wilcox.test(waterbird$num.spp ~ waterbird$Beach) 
Utest.riq.praias # W = 3093, p-value = 0.02474; há diferença significativa (mas fraca) no número de espécies entre praias
bp <- ggplot(bird, aes(x=waterbird$Beach, y=waterbird$num.spp, fill=waterbird$Beach)) + 
  geom_boxplot()
bp + guides(fill=guide_legend(title=NULL))
bp <- bp + scale_fill_discrete(name="Beach",
                               labels=c("Cabras", "Nova Tramandaí"))
bp <- bp + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
bp <- bp + theme(legend.position="none") + labs(x = "", y = "Number of species")
bp <- bp + ggtitle("W = 3093, p-value = 0.02474")
bp

# Diversidade

names(waterbird)
Utest.shannon.div.praias <- wilcox.test(waterbird$shannon.div ~ waterbird$Beach) 
Utest.shannon.div.praias # W = 2762, p-value = 0.3718; não há diferença significativa na diversidade de Shannon entre praias

names(waterbird)
Utest.simpson.div.praias <- wilcox.test(waterbird$simpson.div ~ waterbird$Beach) 
Utest.simpson.div.praias # W = 2821, p-value = 0.2577; não há diferença significativa na diversidade de Simpson entre praias

## NÚMERO DE ESPÉCIES EM CADA HORÁRIO E PRAIA

library(vegan)
specnumber(waterbird[,15:39], bird$Time, MARGIN = 1)
specnumber(bird[,15:39], bird$Beach, MARGIN = 1)

bp <- ggplot(waterbird, aes(x=circa.time, y=waterbird$num.spp, fill=waterbird$Beach)) + 
  geom_boxplot()
bp + guides(fill=guide_legend(title=NULL))
bp <- bp + scale_fill_discrete(name="Area",
                               labels=c("Non urban", "Urban"))
bp <- bp + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
bp <- bp + theme(legend.position=c(0.9,0.9)) + labs(x = "", y = "Number of species")
bp

## NÚMERO DE AVES EM CADA HORÁRIO E PRAIA

bp <- ggplot(waterbird, aes(x=circa.time, y=log(waterbird$totabund), fill=waterbird$Beach)) + 
  geom_boxplot()
bp + guides(fill=guide_legend(title=NULL))
bp <- bp + scale_fill_discrete(name="Area",
                               labels=c("Non urban", "Urban"))
bp <- bp + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
bp <- bp + theme(legend.position=c(0.9,0.9)) + labs(x = "", y = "Total abundance")
bp

## DIVERSIDADE DE AVES EM CADA HORÁRIO E PRAIA

bp <- ggplot(waterbird, aes(x=circa.time, y=waterbird$shannon.div, fill=waterbird$Beach)) + 
  geom_boxplot()
bp + guides(fill=guide_legend(title=NULL))
bp <- bp + scale_fill_discrete(name="Area",
                               labels=c("Non urban", "Urban"))
bp <- bp + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
bp <- bp + theme(legend.position=c(0.9,0.9)) + labs(x = "", y = "Species diversity (Shannon index)")
bp # diversidade é maior em Nova Tramandaí apenas à noite

## CURVA DE ACUMULAÇÃO DE ESPÉCIES - Function 'specaccum' finds species accumulation curves or the number of species for a certain number of sampled sites or individuals.
sac <- specaccum(waterbird[,15:39], method = "random")
plot(sac, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", main = "All dataset")
boxplot(sac, col="yellow", add=TRUE, pch="+")

# Cabras
sac.cabras <- specaccum(waterbird.cabras[,15:39], method = "random")
plot(sac.cabras, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", main = "Cabras")
boxplot(sac.cabras, col="yellow", add=TRUE, pch="+")

# Nova Tramandaí
sac.ntdai <- specaccum(waterbird.ntdai[,15:39], method = "random")
plot(sac.ntdai, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", main = "Nova Tdai")
boxplot(sac.ntdai, col="yellow", add=TRUE, pch="+")

par(mfrow=c(3,1))
plot(sac, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", main = "All dataset")
boxplot(sac, col="yellow", add=TRUE, pch="+")
plot(sac.cabras, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", main = "Cabras")
boxplot(sac.cabras, col="yellow", add=TRUE, pch="+")
plot(sac.ntdai, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", main = "Nova Tdai")
boxplot(sac.ntdai, col="yellow", add=TRUE, pch="+")

## Pielou's evenness J = H0= log(S)
names(waterbird)
library(vegan)
waterbird$pielou.even <- waterbird$shannon.div/log(specnumber(waterbird[,15:39]))
boxplot(waterbird$pielou.even~waterbird$Time)
pielou.even.cabras <- waterbird.cabras$shannon.div/log(specnumber(waterbird.cabras[,15:39]))
pielou.even.ntdai <- waterbird.ntdai$shannon.div/log(specnumber(waterbird.ntdai[,15:39]))
boxplot(pielou.even.cabras~circa.time.cabras) ##maior ocorrência de espécies dominantes em evening
boxplot(pielou.even.ntdai~circa.time.ntdai)
kruskal.test.cabras <- kruskal.test(pielou.even.cabras~circa.time.cabras) # sem diferença estatística significativa
kruskal.test.ntdai <- kruskal.test(pielou.even.ntdai~circa.time.ntdai) # sem diferença estatística significativa

library(ggplot2)
bp <- ggplot(waterbird, aes(x=circa.time, y=waterbird$pielou.even, fill=waterbird$Beach)) + 
  geom_boxplot()
bp + guides(fill=guide_legend(title=NULL))
bp <- bp + scale_fill_discrete(name="Area",
                               labels=c("Non urban", "Urban"))
bp <- bp + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
bp <- bp + theme(legend.position=c(0.1,0.1)) + labs(x = "", y = "Pielou's evenness")

## Riqueza de espécies com rarefação
#### Rarefaction curves often are seen as an objective solution for comparing 
# species richness with different sample sizes. However, rank orders typically 
# differ among different rarefaction sample sizes, rarefaction curves can cross.

# Número de espécies
specnumber(waterbird[,15:39]) # número de espécies em cada censo
max(specnumber(waterbird[,15:39])) # número máximo de espécies observadas em um único censo
min(specnumber(waterbird[,15:39])) # número mínimo de espécies observadas em um único censo

# Número de espécies - Cabras
specnumber(waterbird.cabras[,15:39]) # número de espécies em cada censo
max(specnumber(waterbird.cabras[,15:39])) # número máximo de espécies observadas em um único censo
min(specnumber(waterbird.cabras[,15:39])) # número mínimo de espécies observadas em um único censo

# Número de espécies - Nova Tramandaí
specnumber(waterbird.ntdai[,15:39]) # número de espécies em cada censo
max(specnumber(waterbird.ntdai[,15:39])) # número máximo de espécies observadas em um único censo
min(specnumber(waterbird.ntdai[,15:39])) # número mínimo de espécies observadas em um único censo


waterbird[,15:39>0] <- 1
waterbird[waterbird > 0] <- 1

## CRIANDO UMA MATRIZ BINÁRIA

waterbin <- ifelse(waterbird[,15:39]>0, 1, 0)
head(waterbin)
summary(waterbin)
dim(waterbin)
str(waterbin)

waterbin <- as.numeric(waterbin)

waterbin$Time <- waterbird$Time
waterbin$Beach <- waterbird$Beach
names(waterbin)

library(plyr)
waterbin$Beach <- revalue(waterbin$Beach, c("Cabras"="Non_urban", "Nova_Tdai"="Urban"))
levels(waterbin$Beach)

write.csv(waterbin,"waterbin", row.names= TRUE) # salvar
waterbin <- read.table(file="waterbin.csv", sep=";", h=T)
names(waterbin)

waterbin.cabras <- subset(waterbin[which(waterbin$Beach=="Cabras"),])
names(waterbin.cabras)
waterbin.cabras$Beach

waterbin.ntdai <- subset(waterbin[which(waterbin$Beach=="Nova_Tdai"),])
names(waterbin.ntdai)
waterbin.ntdai$Beach
head(waterbin)

# Identificando e removendo conso com zero contagens

a <- rowSums(waterbin[,3:27]!=0)
waterbin <- waterbin[-c(60, 66), ]

## Calculando distância

dist.beach <- subset(waterbin, select = -c(Time))
names(dist.beach)

dist.time <- subset(waterbin, select = -c(Beach))
names(dist.time)

dist.time.cabras <- subset(waterbin[which(waterbin$Beach=="Cabras"),])
dist.time.cabras <- subset(dist.time.cabras, select = -c(Beach))

dist.time.ntdai <- subset(waterbin[which(waterbin$Beach=="Nova_Tdai"),])
dist.time.ntdai <- subset(dist.time.ntdai, select = -c(Beach))

### nMDS

library(vegan)
names(waterbird)

# Remover censos sem observação
a <- rowSums(waterbird[,15:39]!=0)
waterbird <- waterbird[-c(60, 66), ]

# Transformação de dados
Data2 <- decostand(waterbird[,15:39], "max") #divide todos os valores pelo valor máximo encontrado em uma linha
NMDSdata <- decostand(waterbird[,15:39], "total") #divide todos os valores pelo valor máximo encontrado nas colunas
log.waterbird <- log(waterbird[,15:39])

# Fazendo com dados transformados = decostand "total"

initial_nMDS1 <- metaMDS(NMDSdata, distance="bray", k=7, trymax=100)
nMDS1 <- metaMDS(NMDSdata, previous.best = initial_nMDS1, k=7, trymax=100)

# Plotar o stress

stressplot(nMDS1)

op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=2, pch=21, 
     bg="skyblue3", xlab='NMDS1', ylab='NMDS2', cex=1.6, main="", 
     xlim=c(-1.5, 2), ylim=c(-2, 2))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
par(op)
op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=4, pch=21, 
     bg=c("black", "orange", "yellow", "violet", "green", "red3", "blue")
     [waterbird$Time], xlab='NMDS1', ylab='NMDS2', cex=1.4, main="", 
     xlim=c(-0.8, 1.3), ylim=c(-2, 1.3))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
legend("bottomright", c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"), col=c("black", "orange", "yellow", "violet", "green", "red3", "blue"),
         text.col = "black", pch=16, cex=0.9, xpd=T)

# Fazendo com dados brutos


initial_nMDS1 <- metaMDS(waterbird[,15:39], distance="jaccard", k=2, trymax=1000)
nMDS1 <- metaMDS(waterbird[,15:39], previous.best = initial_nMDS1, k=2, trymax=1000)

# Plotar o stress

stressplot(nMDS1)

op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=2, pch=21, 
     bg=c("black", "orange", "yellow", "violet", "green", "red3", "blue")
     [waterbird$Time], xlab='NMDS1', ylab='NMDS2', cex=1.4, main="", 
     xlim=c(-1.5, 1.3), ylim=c(-1.5, 1.3))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
legend("bottomright", c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"), col=c("black", "orange", "yellow", "violet", "green", "red3", "blue"),
       text.col = "black", pch=16, cex=0.9, xpd=T)

# Fazendo com dados brutos entre praias


initial_nMDS1 <- metaMDS(waterbird[,15:39], distance="jaccard", k=2, trymax=1000)
nMDS1 <- metaMDS(waterbird[,15:39], previous.best = initial_nMDS1, k=2, trymax=1000)

# Plotar o stress

stressplot(nMDS1)

op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=2, pch=21, 
     bg=c("black", "orange")
     [waterbird$Beach], xlab='NMDS1', ylab='NMDS2', cex=1.4, main="", 
     xlim=c(-1.5, 1.3), ylim=c(-1.5, 1.3))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
legend("bottomright", c("Non_urban", "Urban"), col=c("black", "orange"),
       text.col = "black", pch=16, cex=0.9, xpd=T)

# Fazendo com dados brutos para a praia das Cabras separadamente

names(waterbird.cabras)
initial_nMDS1 <- metaMDS(waterbird.cabras[,15:39], distance="jaccard", k=2, trymax=1000)
nMDS1 <- metaMDS(waterbird.cabras[,15:39], previous.best = initial_nMDS1, k=2, trymax=1000)

# Plotar o stress

stressplot(nMDS1)

op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=2, pch=21, 
     bg=c("black", "orange", "yellow", "violet", "green", "red3", "blue")
     [waterbird$Time], xlab='NMDS1', ylab='NMDS2', cex=1.4, main="", 
     xlim=c(-1.5, 2.2), ylim=c(-1.3, 1.1))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
legend("bottomright", c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"), col=c("black", "orange", "yellow", "violet", "green", "red3", "blue"),
       text.col = "black", pch=16, cex=1.1, xpd=T)

# Fazendo com dados brutos para a praia de Nova Tramandaí separadamente

initial_nMDS1 <- metaMDS(waterbird.ntdai[,15:39], distance="jaccard", k=2, trymax=1000)
nMDS1 <- metaMDS(waterbird.ntdai[,15:39], previous.best = initial_nMDS1, k=2, trymax=1000)

# Plotar o stress

stressplot(nMDS1)

op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=2, pch=21, 
     bg=c("black", "orange", "yellow", "violet", "green", "red3", "blue")
     [waterbird$Time], xlab='NMDS1', ylab='NMDS2', cex=1.4, main="", 
     xlim=c(-1.3, 1.3), ylim=c(-2, 1.5))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
legend("bottomright", c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"), col=c("black", "orange", "yellow", "violet", "green", "red3", "blue"),
       text.col = "black", pch=16, cex=0.9, xpd=T)

# Fazendo com dados brutos e matriz binária
waterbin <- waterbin[-c(60, 66), ]

names(waterbin)
initial_nMDS1 <- metaMDS(waterbin[,3:27], distance="jaccard", k=2, trymax=1000)
nMDS1 <- metaMDS(waterbin[,3:27], previous.best = initial_nMDS1, k=2, trymax=1000)

# Plotar o stress

stressplot(nMDS1)

op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=2, pch=21, 
     bg=c("black", "orange", "yellow", "violet", "green", "red3", "blue")
     [waterbird$Time], xlab='NMDS1', ylab='NMDS2', cex=1.4, main="", 
     xlim=c(-1.5, 1.3), ylim=c(-1.5, 1.3))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
legend("bottomright", c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"), col=c("black", "orange", "yellow", "violet", "green", "red3", "blue"),
       text.col = "black", pch=16, cex=0.9, xpd=T)

# Fazendo com dados brutos e matriz binária entre praias

library(vegan)
initial_nMDS1 <- metaMDS(waterbin[,3:27], distance="jaccard", k=2, trymax=1000)
nMDS1 <- metaMDS(waterbin[,3:27], previous.best = initial_nMDS1, k=2, trymax=1000)

# Plotar o stress

stressplot(nMDS1)

op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=2, pch=21, 
     bg=c("black", "orange", "yellow")
     [waterbird$Beach], xlab='NMDS1', ylab='NMDS2', cex=1.4, main="", 
     xlim=c(-2, 1.3), ylim=c(-1.2, 1))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
legend("bottomleft", c("Non_urban", "Urban"), 
       col=c("black", "orange"),
       text.col = "black", pch=16, cex=0.9, xpd=T)

# Fazendo com dados brutos e matriz binária da praia das Cabras

library(vegan)
initial_nMDS1 <- metaMDS(waterbin.cabras[,3:27], distance="jaccard", k=2, trymax=1000)
nMDS1 <- metaMDS(waterbin.cabras[,3:27], previous.best = initial_nMDS1, k=2, trymax=1000)

# Plotar o stress

stressplot(nMDS1)

op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=2, pch=21, 
     bg=c("black", "orange", "yellow", "violet", "green", "red3", "blue")
     [waterbird$Time], xlab='NMDS1', ylab='NMDS2', cex=1.4, main="", 
     xlim=c(-2, 1.3), ylim=c(-1.2, 1))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
legend("bottomleft", c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", 
                        "Evening"), 
       col=c("black", "orange", "yellow", "violet", "green", "red3", "blue"),
       text.col = "black", pch=16, cex=0.9, xpd=T)

# Fazendo com dados brutos e matriz binária da praia de Nova Tramandaí


initial_nMDS1 <- metaMDS(waterbin.ntdai[,3:27], distance="jaccard", k=2, trymax=1000)
nMDS1 <- metaMDS(waterbin.ntdai[,3:27], previous.best = initial_nMDS1, k=2, trymax=1000)

# Plotar o stress

stressplot(nMDS1)

op <- par(mar=c(4,4,1,1))
plot(nMDS1$points[,1], nMDS1$points[,2], font.lab=2, pch=21, 
     bg=c("black", "orange", "yellow", "violet", "green", "red3", "blue")
     [waterbird$Time], xlab='NMDS1', ylab='NMDS2', cex=1.4, main="", 
     xlim=c(-1.4, 1.4), ylim=c(-1, 1))
mtext(side=3, line=-1, adj=0.98, paste('STRESS=',round(nMDS1$stress, 3),sep=''), cex=1.0)
legend("bottomleft", c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", 
                       "Evening"), 
       col=c("black", "orange", "yellow", "violet", "green", "red3", "blue"),
       text.col = "black", pch=16, cex=0.9, xpd=T)


### FAZER ORDIHULLLL

ordihull(nMDS1,groups=waterbin$Time,draw="polygon",col="grey90",
         label=T, air=2)

ordiellipse(nMDS1,groups=waterbird.cabras$Time,draw="polygon",col="grey90",
         label=T, conf=0.7, kind="sd", alpha = 30)


### Using ggplot to plot NMDS

library(ggplot2)

# Entre praias

data.scores <- as.data.frame(scores(nMDS1))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- waterbin$Beach  #  add the grp variable created earlier
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(nMDS1, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame

species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

grp.a <- data.scores[data.scores$grp == "Non_urban", ][chull(data.scores[data.scores$grp == 
                                                                       "Non_urban", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- data.scores[data.scores$grp == "Urban", ][chull(data.scores[data.scores$grp == 
                                                                         "Urban", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b
hull.data

hull <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.3) + # add the convex hulls
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=grp),size=1.3) + # add the point markers
  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=""),size=8,vjust=0,hjust=0) +
  scale_colour_manual(breaks=c("Non_urban", "Urban"),
                      values=c("Non_urban" = "black", "Urban" = "orange")) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=12), # remove x-axis labels
        axis.title.y = element_text(size=12), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
hull <- hull + theme(legend.title=element_blank())
hull <- hull + geom_text(x=3, y=30, paste('STRESS=',round(nMDS1$stress, 3),sep=''))
hull

# Entre horários
data.scores <- as.data.frame(scores(nMDS1))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- waterbird$Beach  #  add the grp variable created earlier
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(nMDS1, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame

species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

grp.a <- data.scores[data.scores$grp == "Night", ][chull(data.scores[data.scores$grp == 
                                                                   "Night", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- data.scores[data.scores$grp == "Sunrise", ][chull(data.scores[data.scores$grp == 
                                                                   "Sunrise", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
grp.c <- data.scores[data.scores$grp == "Morning", ][chull(data.scores[data.scores$grp == 
                                                                         "Morning", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
grp.d <- data.scores[data.scores$grp == "Midday", ][chull(data.scores[data.scores$grp == 
                                                                         "Midday", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
grp.e <- data.scores[data.scores$grp == "Afternoon", ][chull(data.scores[data.scores$grp == 
                                                                         "Afternoon", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
grp.f <- data.scores[data.scores$grp == "Sunset", ][chull(data.scores[data.scores$grp == 
                                                                         "Sunset", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
grp.g <- data.scores[data.scores$grp == "Evening", ][chull(data.scores[data.scores$grp == 
                                                                         "Evening", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

hull.data <- rbind(grp.a, grp.b,grp.c,grp.d,grp.e,grp.f,grp.g)  #combine grp.a and grp.b
hull.data

hull <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30) + # add the convex hulls
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=grp),size=1.5) + # add the point markers
  scale_colour_manual(breaks=c("Night", "Sunrise", "Morning", "Midday", "Afternoon", "Sunset", "Evening"), 
                      values=c("Night" = "black", "Sunrise" = "orange", "Morning" = "yellow", "Midday" = "violet", "Afternoon" = "green", "Sunset" = "red", "Evening" = "blue")) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=12), # remove x-axis labels
        axis.title.y = element_text(size=12), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
hull <- hull + scale_fill_discrete(guide=FALSE)
hull <- hull + theme(legend.title=element_blank())
hull <- hull + geom_text(x=3, y=30, label=nMDS1$stress)
hull

### PERMANOVA GLOBAL

### Separando por praia

waterbird.cabras <- waterbird[which(waterbird$Beach=="Non_urban"),]
waterbird.cabras
waterbird.ntdai <- waterbird[which(waterbird$Beach=="Urban"),]
waterbird.ntdai

names(waterbird)
library(vegan)

# Global
permanova <- adonis(waterbird[,16:40] ~ waterbird$Beach + waterbird$Time, permutations=999, 
                    distance='jaccard')
permanova ## há diferença significativa na composição das comunidades entre praias e entre horários (p < 0.001; F = 15,4633 (praia) e F = 4,3593 (hor))

# Por tempo nas Cabras
permanova.cabras <- adonis(waterbird.cabras[,16:40] ~ waterbird.cabras$Time, permutations=999, 
                    distance='jaccard')
permanova.cabras ## há diferença significativa na composição das comunidades ntre horários na praia das Cabras (p < 0.001; F = 4,32)

# Por tempo em N_Tdai
permanova.ntdai <- adonis(waterbird.ntdai[,16:40] ~ waterbird.ntdai$Time, permutations=999, 
                           distance='jaccard')
permanova.ntdai ## há diferença significativa na composição das comunidades entre horários em N Tdai (p < 0.001; F = 2,29)

## Posthoc para PERMANOVA (comparações par a par)

library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis", force=TRUE) 
library(pairwiseAdonis)

pair.mod.cabras<-pairwise.adonis(waterbird.cabras[,16:40],factors=waterbird.cabras$Time)
pair.mod.cabras

pair.mod.ntdai<-pairwise.adonis(waterbird.ntdai[,16:40],factors=waterbird.ntdai$Time)
pair.mod.ntdai

