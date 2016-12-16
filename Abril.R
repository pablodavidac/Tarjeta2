
Desertores910<-read.table("Desertores910.csv",
                        header=TRUE,
                        sep=",",
                        na.strings = "NA", 
                        colClasses =c(
                          "integer",
                          "factor",
                          "integer"))
Desertores910<-Desertores910[,-2]

Abril %>% count(Mesdesercion)

Abril<-left_join(Abril,Desertores910,by=c("IdCuentaTarjeta"))

Abril[which(Abril$Filtro==9),]$Mesdesercion<-Abril[which(Abril$Filtro==9),]$Filtro
Abril[which(Abril$Filtro==10),]$Mesdesercion<-Abril[which(Abril$Filtro==10),]$Filtro

Abril[which(Abril$Mesdesercion<=10),]$Objetivo<-1
Abril %>% count(Objetivo)
########################Variable provincia#############################
table(Abril$ProvinciaDomicilio)
Abril["Provincia"]<-"NA"
Abril[which(Abril$ProvinciaDomicilio=="AZUAY"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="EL ORO"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="LOJA"),]$Provincia<-1
Abril[which(Abril$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-1


Abril[which(Abril$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="CARCHI"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="MANABI"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="NAPO"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-0
Abril[which(Abril$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-0

Abril[which(Abril$Provincia=="NA"),]$Provincia<-0




Abril["Afinidad"]<-"NA"
Abril[which(Abril$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
Abril[which(Abril$AfinidadTarjeta=="TARJETA ALIA - CUOTA FACIL"),]$Afinidad<-1
Abril[which(Abril$AfinidadTarjeta=="TARJETA ALIA - GOLD"),]$Afinidad<-1

Abril[which(Abril$AfinidadTarjeta=="TARJETA ALIA - BLACK"),]$Afinidad<-0
Abril[which(Abril$AfinidadTarjeta=="TARJETA ALIA - PLATINUM"),]$Afinidad<-0
Abril[which(Abril$Afinidad=="NA"),]$Afinidad<-0


View(table(Abril$Afinidad,Abril$Objetivo))

Abril["TieneSuperAvance12M"]<-"NA"
Abril[which(Abril$NumeroConsumosSuperAvance12M>0),]$TieneSuperAvance12M<-1
Abril[which(Abril$NumeroConsumosSuperAvance12M==0),]$TieneSuperAvance12M<-0
table(Abril$TieneSuperAvance12M)


Abril["CodEstadoCivil"]<-"NA"
Abril[which(Abril$EstadoCivil=="CASADO"),]$CodEstadoCivil<-1
Abril[which(Abril$EstadoCivil=="DIVORCIADO"),]$CodEstadoCivil<-1


Abril[which(Abril$EstadoCivil=="SOLTERO"),]$CodEstadoCivil<-0
Abril[which(Abril$EstadoCivil=="UNIÓN LIBRE"),]$CodEstadoCivil<-0
Abril[which(Abril$EstadoCivil=="VIUDO"),]$CodEstadoCivil<-0

Abril[which(Abril$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1


Abril["TieneBlack"]<-0
Abril[which(Abril$AfinidadTarjeta=="TARJETA ALIA - BLACK"),]$TieneBlack<-1

Abril["TieneGold"]<-0
Abril[which(Abril$AfinidadTarjeta=="TARJETA ALIA - GOLD"),]$TieneGold<-1


Abril["TienePlatinum"]<-0
Abril[which(Abril$AfinidadTarjeta=="TARJETA ALIA - PLATINUM"),]$TienePlatinum<-1



Abril["SectorComercio"]<-0
Abril[which(Abril$Sector=="COMERCIO"),]$SectorComercio<-1

Abril["SectorDependencia"]<-0
Abril[which(Abril$Sector=="DEPENDENCIA"),]$SectorDependencia<-1

Abril["SectorPersonal"]<-0
Abril[which(Abril$Sector=="PERSONAL"),]$SectorPersonal<-1

Abril["SectorProduccion"]<-0
Abril[which(Abril$Sector=="PRODUCCION"),]$SectorProduccion<-1

Abril["SectorServicios"]<-0
Abril[which(Abril$Sector=="SERVICIOS"),]$SectorServicios<-1


Abril["CodEstadoCivil"]<-1
Abril[which(Abril$EstadoCivil=="SOLTERO"),]$CodEstadoCivil<-0
Abril[which(Abril$EstadoCivil=="UNIÓN LIBRE"),]$CodEstadoCivil<-0
Abril[which(Abril$EstadoCivil=="VIUDO"),]$CodEstadoCivil<-0

Abril["Instruccion"]<-1
Abril[which(Abril$NivelInstruccion=="FORMACION INTERMEDIA"),]$Instruccion<-0
Abril[which(Abril$NivelInstruccion=="SECUNDARIA"),]$Instruccion<-0
Abril[which(Abril$NivelInstruccion=="UNIVERSIDAD"),]$Instruccion<-0

Abril["HastaPrimaria"]<-0
Abril[which(Abril$NivelInstruccion=="PRIMARIA"),]$HastaPrimaria<-1

Abril["HastaUniversidad"]<-0
Abril[which(Abril$NivelInstruccion=="UNIVERSIDAD"),]$HastaUniversidad<-1

Abril["Consume2omasAvances12M"]<-0
Abril[which(Abril$NumeroConsumosAvance12M>2.5),]$Consume2omasAvances12M<-1

Abril["CupoUsadoAvancesHasta"]<-0
Abril[which(Abril$`Cupo.Ut.Avances/Cupo.Aprobado`<0.041),]$CupoUsadoAvancesHasta<-1

Abril["CodT6CupoUtilizado"]<-1
Abril[which(Abril$T6.CupoUtilizado>(-0.35)),]$CodT6CupoUtilizado<-0


Abril["TieneMenos300dSinConsumo"]<-1
Abril[which(Abril$Dias.sin.consumo>300),]$TieneMenos300dSinConsumo<-0



Abril["CodCupoUtilizado"]<-0
Abril[which(Abril$CupoUtilizado<439),]$CodCupoUtilizado<-1



Abril["CodPromCupoUtilizado3M"]<-0
Abril[which(Abril$Prom.CupoUtilizado3<542),]$CodPromCupoUtilizado3M<-1



Abril["NormCupoUtilizado"]<-as.numeric(scale(Abril$CupoUtilizado))
Abril["NormPromCupoUtilizado3M"]<-as.numeric(scale(Abril$Prom.CupoUtilizado3))


Abril["NormPromCupoUtili3MxTipoClienteActivo"]<-0
Abril[which(Abril$TipoCliente=="ACTIVO"),]$NormPromCupoUtili3MxTipoClienteActivo<-Abril[which(Abril$TipoCliente=="ACTIVO"),]$NormPromCupoUtilizado3M


Abril["NormPromCupoUtili3MxTipoClienteInactivo"]<-0
Abril[which(Abril$TipoCliente=="INACTIVO"),]$NormPromCupoUtili3MxTipoClienteInactivo<-Abril[which(Abril$TipoCliente=="INACTIVO"),]$NormPromCupoUtilizado3M


Abril["TieneMas300DiasSinConsumo"]<-0
Abril[which(Abril$Dias.sin.consumo>300),]$TieneMas300DiasSinConsumo<-1


Abril["TieneMas920DiasSinConsumo"]<-1
Abril[which(Abril$Dias.sin.consumo<920),]$TieneMas920DiasSinConsumo<-0


Abril["TieneMenos920DiasSinConsumo"]<-1
Abril[which(Abril$Dias.sin.consumo>920),]$TieneMenos920DiasSinConsumo<-0

Abril["TieneMenos150DiasSinConsumo"]<-1
Abril[which(Abril$Dias.sin.consumo>150),]$TieneMenos150DiasSinConsumo<-0

Abril["Menos8Autorizaciones12M"]<-0
Abril[which(Abril$Autorizacion12M<8.5),]$Menos8Autorizaciones12M<-1

table(Abril$NormPromCupoUtilizado3M)
(table(Abril$CodT6CupoUtilizado,Abril$Objetivo))
table(Abril$AfinidadTarjeta)
#################################################################################
Abril %>% count(TipoCliente)

Abril$Objetivo<-0
Abril[which(Abril$Mesdesercion<=10),]$Objetivo<-1
#Abril[which(Abril$Mesdesercion<=4),]$Objetivo<-0
Abril %>% count(Objetivo)
#Abril<-subset(Abril,TipoCliente=="INACTIVO")

TrainA <- createDataPartition(Abril$IdCuentaTarjeta, p=0.7, list=FALSE)
trainingA <- Abril[ TrainA, ]
testingA<- Abril[ -TrainA, ]
trainingA %>% count(Objetivo)
testingA %>% count(Objetivo)
####################Balancear el conjunto de entrenamiento#################################
muestra<-sample_n(subset(trainingA, Objetivo==0),1278,replace = FALSE)
muestra["filtro"]<-1
muestra<-subset(muestra, select = c("IdCuentaTarjeta","filtro"))
Buenos<-subset(trainingA, Objetivo==0) %>%left_join(muestra,by=c("IdCuentaTarjeta" = "IdCuentaTarjeta"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-236]
########################Malos####################
n<-38
Malos<-do.call("rbind", replicate(n, subset(trainingA, Objetivo==1), simplify = FALSE))

BalanceadaA<-rbind(Buenos,Malos)
BalanceadaA %>% count(Objetivo)

###########################################################################################

modA <- glm(Objetivo~ 
               NormCupoUtilizado+
              #CodPromCupoUtilizado3M+
             # NormPromCupoUtili3MxTipoClienteActivo+
              #NormPromCupoUtili3MxTipoClienteInactivo+
             PagosVencidos+
              Otra_Tarjeta+
              SVidaDesgravamen+
              STotalFamiliar+
              SoloSeguro
            
              #TipoCliente
            #+PromMaxDiasSinconsumo3M
             +Provincia
            
           
            +CodEstadoCivil

            +TieneGold
            +TieneBlack
            +TienePlatinum
            
            #+Numero_tarjetasT6
            +CodT6CupoUtilizado
            
            # +CargasFamiliares
            
            +CupoUsadoAvancesHasta
            #+Consume2omasAvances12M
            #+Instruccion
            #+HastaPrimaria
            #+HastaUniversidad
            #+TieneMenos300dSinConsumo
            #+TieneMenos300dSinConsumo
            #+TieneMenos150DiasSinConsumo
            +TieneMenos920DiasSinConsumo
            +Menos8Autorizaciones12M
            , data=BalanceadaA,
            family="binomial"(link = "logit"))

summary(modA)


##########################AUC ROC#############################
fitted.results <- predict(modA,newdata=testingA,type='response')
pr <- prediction(fitted.results, testingA$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
str(fitted.results)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

########################Matriz de confusión####################
fitted.results <- predict(modA,newdata=testingA,type='response')
fitted.results <- ifelse(fitted.results> 0.5,1,0)
confusionMatrix(testingA$Objetivo, fitted.results)

#######################K-S test#############################
################KS - prueba##################33
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plotROC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")


fitted.results <- predict(modA,newdata=testingA,type='response')
aux<-cbind(testingA,fitted.results)

sample1<-subset(aux,Objetivo==1)$fitted.results
sample2<-subset(aux,Objetivo==0)$fitted.results

cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(sample1, sample2,na.rm = TRUE), max(sample1, sample2,na.rm = TRUE), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 
y1-y0

# png(file = "c:/temp/ks.png", width = 1024, height = 768, type="cairo-png")
ggplot(aux, aes(x = fitted.results, group = Objetivo, color = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  stat_ecdf(size=1) +
  theme_bw(base_size = 14) +
  theme(legend.position ="top") +
  xlab("Score") +
  ylab("Distribución acumulada") +
  #geom_line(size=1) +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=3) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=3) +
  #scale_x_continuous(breaks=seq(0,1,by=0.1))+
  ggtitle("Prueba K-S") +
  annotate("text", x = .75, y = .25, 
           label = paste("K-S =", round(y1-y0, 3))) +
  theme(legend.title=element_blank())

#######################Curva Roc######################################
fitted.results <- predict(modA,newdata=testingA,type='response')


aux<-cbind(testingA,fitted.results)
aux$Objetivo<-as.numeric(aux$Objetivo)
aux[which(aux$Objetivo==1),]$Objetivo<-0
aux[which(aux$Objetivo==2),]$Objetivo<-1
head(aux$Objetivo)


basicplot <- ggplot(aux, aes(d = Objetivo, m = fitted.results)) +  geom_roc(labels = FALSE)



basicplot+  
  style_roc(guide = TRUE, xlab = "1- Tasa de Falsos Desertores", 
            ylab = "Tasa de Verdaderos Desertores", theme = theme_bw)+
  #theme_bw(base_size = 14)+
  labs(title="Curva ROC") +
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =",round(calc_auc(basicplot)$AUC, 3))) 

####################Deciles###########################
fitted.results <- predict(modA,newdata=Abril,type='response')

#quan<-as.vector(quan)

Abril["Score"]<-predict(modA,newdata=Abril,type='response')
View(quan<-quantile(Abril$Score, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)
Abril["Decil"]<-"NA"

#aux<-subset(Abril,Score==quan[2])
################Codificaccion de P1 - P10 ###########################
Abril[which(Abril$Score <= quan[2]),]$Decil<-"P1"
Abril[which(Abril$Score <=quan[3] & Abril$Score >quan[2]),]$Decil<-"P2"
Abril[which(Abril$Score <=quan[4] & Abril$Score >quan[3]),]$Decil<-"P3"
Abril[which(Abril$Score <=quan[5] & Abril$Score > quan[4]),]$Decil<-"P4"
Abril[which(Abril$Score <=quan[6] & Abril$Score > quan[5]),]$Decil<-"P5"
Abril[which(Abril$Score <=quan[7] & Abril$Score > quan[6]),]$Decil<-"P6"
Abril[which(Abril$Score <=quan[8] & Abril$Score > quan[7]),]$Decil<-"P7"
Abril[which(Abril$Score <=quan[9] & Abril$Score > quan[8]),]$Decil<-"P8"
Abril[which(Abril$Score <=quan[10] & Abril$Score > quan[9]),]$Decil<-"P9"
Abril[which(Abril$Score >quan[10]),]$Decil<-"P10"

Abril[which(Abril$Score >0.825),]$Decil<-"P20"

View(table(Abril$Decil,Abril$Objetivo))
Abril %>% count(Objetivo)



aux<-subset(Abril,Decil=="P9")
View(table(aux$Mesdesercion))


########################R2########################################
library("pscl", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("BaylorEdPsych", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
PseudoR2(modA)


###############Gráficos Error tipo I y II###########################
fitted.results <- predict(modA,newdata=Abril,type='response')
Aux<-cbind(Abril,fitted.results)
qplot(fitted.results, data = Aux, geom = "density",
      color = Objetivo)+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  
  xlab("Score")+
  ylab("Densidad")+ 
  ggtitle("Error Tipo I y II")+
  theme_bw()



fitted.results <- predict(modA,newdata=Abril,type='response')
Aux<-cbind(Abril,fitted.results)
qplot(fitted.results, data = Aux, geom = "histogram",
      color = Objetivo)+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  xlab("Score")+
  ylab("Densidad")+ 
  ggtitle("Error Tipo I y II")+
  theme_bw()

###############################################3

##########################Distancia de Mahalanobis#####################
Aux<-subset(Abril,Objetivo==0)
c1<-cut(Aux$Score,breaks = seq(from = 0, to = 1, by =0.01,right=FALSE))
c1<-as.data.frame(table(c1))
c1$Freq
Aux<-subset(Abril,Objetivo==1)
c2<-cut(Aux$Score,breaks = seq(from = 0, to = 1, by =0.01,right=FALSE))
c2<-as.data.frame(table(c2))
c2$Freq
x<-seq(from=0.01,to=1,by=0.01)
c<-as.data.frame(cbind(x,c1$Freq,c2$Freq))


##################Especificidad vs Sencibilidad#############
z<-c(
  0.85,
  0.82,
  0.8,
  0.79,
  0.78,
  0.76,
  0.74,
  0.72,
  0.7,
  0.6,
  0.5,
  0.4,
  0.3,
  0.2,
  0.1
)
y<-array(0,dim = c(15,2))
for (i in 0:14) {
  fitted.results <- predict(modA,newdata=Abril,type='response')
  fitted.results <- ifelse(fitted.results > z[15-i],1,0)
  #confusionMatrix(Diciembre$Objetivo, fitted.results)
  y[15-i,1]<-table(Abril$Objetivo, fitted.results)[1,2]
  y[15-i,2]<-table(Abril$Objetivo, fitted.results)[2,2]}
View(y)
###########################################################

fitted.results <- predict(modA,newdata=Abril,type='response')
fitted.results <- ifelse(fitted.results> 0.5,1,0)
confusionMatrix(Abril$Objetivo, fitted.results)

