Desertores910<-read.table("Desertores910.csv",
                          header=TRUE,
                          sep=",",
                          na.strings = "NA", 
                          colClasses =c(
                            "integer",
                            "factor",
                            "integer"))
Desertores910<-Desertores910[,-2]

Diciembre %>% count(Mesdesercion)

Diciembre<-left_join(Diciembre,Desertores910,by=c("IdCuentaTarjeta"))

Diciembre[which(Diciembre$Filtro==9),]$Mesdesercion<-Diciembre[which(Diciembre$Filtro==9),]$Filtro
Diciembre[which(Diciembre$Filtro==10),]$Mesdesercion<-Diciembre[which(Diciembre$Filtro==10),]$Filtro

Diciembre$Objetivo<-0
Diciembre[which(Diciembre$Mesdesercion<=10),]$Objetivo<-1

Diciembre %>% count(Objetivo)

######################Reemplazo NA##########################################

###############################Tratamiento de Datos Perdidos###########################

summary(Diciembre$NumeroConsumosAvance3M)
Diciembre[which(is.na(Diciembre$NumeroConsumosAvance12M)),]$NumeroConsumosAvance12M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumosAvance6M)),]$NumeroConsumosAvance6M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumosAvance3M)),]$NumeroConsumosAvance3M<-0

summary(Diciembre$NumeroConsumosSuperAvance3M)
Diciembre[which(is.na(Diciembre$NumeroConsumosSuperAvance12M)),]$NumeroConsumosSuperAvance12M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumosSuperAvance6M)),]$NumeroConsumosSuperAvance6M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumosSuperAvance3M)),]$NumeroConsumosSuperAvance3M<-0


summary(Diciembre$NumeroConsumoConInteres6M)
Diciembre[which(is.na(Diciembre$NumeroConsumoConInteres12M)),]$NumeroConsumoConInteres12M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumoConInteres6M)),]$NumeroConsumoConInteres6M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumoConInteres3M)),]$NumeroConsumoConInteres3M<-0


summary(Diciembre$NumeroConsumoSinInteres6M)
Diciembre[which(is.na(Diciembre$NumeroConsumoSinInteres12M)),]$NumeroConsumoSinInteres12M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumoSinInteres6M)),]$NumeroConsumoSinInteres6M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumoSinInteres3M)),]$NumeroConsumoSinInteres3M<-0


summary(Diciembre$NumeroConsumoCorriente6M)
Diciembre[which(is.na(Diciembre$NumeroConsumoCorriente12M)),]$NumeroConsumoCorriente12M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumoCorriente6M)),]$NumeroConsumoCorriente6M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumoCorriente3M)),]$NumeroConsumoCorriente3M<-0


summary(Diciembre$NumeroConsumos3M)
Diciembre[which(is.na(Diciembre$NumeroConsumos12M)),]$NumeroConsumos12M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumos6M)),]$NumeroConsumos6M<-0
Diciembre[which(is.na(Diciembre$NumeroConsumos3M)),]$NumeroConsumos3M<-0



summary(Diciembre$MedioPagoPOS12M)
Diciembre[which(is.na(Diciembre$MedioPagoVoucher12M)),]$MedioPagoVoucher12M<-0
Diciembre[which(is.na(Diciembre$MedioPagoPOS12M)),]$MedioPagoPOS12M<-0



summary(Diciembre$T3.CupoUtilizado)
NROW(Diciembre[which(is.na(Diciembre$T3.CupoUtilizado)),])
NROW(Diciembre[which(is.infinite(Diciembre$T3.CupoUtilizado)),])
Diciembre[which(is.na(Diciembre$T3.CupoUtilizado)),]$T3.CupoUtilizado=(-1)
Diciembre[which(is.infinite(Diciembre$T3.CupoUtilizado)),]$T3.CupoUtilizado<-(Diciembre[which(is.infinite(Diciembre$T3.CupoUtilizado)),]$CupoUtilizado-1)




NROW(Diciembre[which(is.na(Diciembre$T6.CupoUtilizado)),])
NROW(Diciembre[which(is.infinite(Diciembre$T6.CupoUtilizado)),])
Diciembre[which(is.na(Diciembre$T6.CupoUtilizado)),]$T6.CupoUtilizado=(-1)
Diciembre[which(is.infinite(Diciembre$T6.CupoUtilizado)),]$T6.CupoUtilizado<-(Diciembre[which(is.infinite(Diciembre$T6.CupoUtilizado)),]$CupoUtilizado-1)



NROW(Diciembre[which(is.na(Diciembre$T36.CupoUtilizado)),])
NROW(Diciembre[which(is.infinite(Diciembre$T36.CupoUtilizado)),])
Diciembre[which(is.na(Diciembre$T36.CupoUtilizado)),]$T36.CupoUtilizado=(-1)
Diciembre[which(is.infinite(Diciembre$T36.CupoUtilizado)),]$T36.CupoUtilizado<-(Diciembre[which(is.infinite(Diciembre$T36.CupoUtilizado)),]$CupoUtilizado-1)







NROW(Diciembre[which(is.na(Diciembre$`Cupo.Utilizado/Cupo.Aprobado`)),])
NROW(Diciembre[which(is.infinite(Diciembre$`Cupo.Utilizado/Cupo.Aprobado`)),])
Diciembre[which(is.na(Diciembre$`Cupo.Utilizado/Cupo.Aprobado`)),]$`Cupo.Utilizado/Cupo.Aprobado`<-Diciembre[which(is.na(Diciembre$`Cupo.Utilizado/Cupo.Aprobado`)),]$CupoUtilizado
#Diciembre[which(is.infinite(Diciembre$`Cupo.Utilizado/Cupo.Aprobado`)),]$`Cupo.Utilizado/Cupo.Aprobado`<-(Diciembre[which(is.infinite(Diciembre$T36.CupoUtilizado)),]$CupoUtilizado-1)



NROW(Diciembre[which(is.na(Diciembre$`Cupo.Ut.Avances/Cupo.Aprobado`)),])
NROW(Diciembre[which(is.infinite(Diciembre$`Cupo.Ut.Avances/Cupo.Aprobado`)),])
Diciembre[which(is.na(Diciembre$`Cupo.Ut.Avances/Cupo.Aprobado`)),]$`Cupo.Ut.Avances/Cupo.Aprobado`<-Diciembre[which(is.na(Diciembre$`Cupo.Ut.Avances/Cupo.Aprobado`)),]$CupoUtilizadoAvance


summary(Diciembre$Edad.Cuenta.Tc)
NROW(Diciembre[which(is.na(Diciembre$Edad.Cuenta.Tc)),])
Diciembre[which(is.na(Diciembre$Edad.Cuenta.Tc)),]$Edad.Cuenta.Tc<-mean(Diciembre$Edad.Cuenta.Tc,na.rm=TRUE)


summary(Diciembre$Dias.sin.consumo)
NROW(Diciembre[which(is.na(Diciembre$Dias.sin.consumo)),])
NROW(Diciembre[which(is.infinite(Diciembre$Dias.sin.consumo)),])

Diciembre[which(is.na(Diciembre$Dias.sin.consumo)),]$Dias.sin.consumo<-(as.numeric(Diciembre[which(is.na(Diciembre$Dias.sin.consumo)),]$Edad.Cuenta.Tc)*30)


NROW(Diciembre[which(is.na(Diciembre$Edad.Cliente)),])
Diciembre[which(is.na(Diciembre$Edad.Cliente)),]$Edad.Cliente<-mean(Diciembre$Edad.Cliente,na.rm=TRUE)


NROW(Diciembre[which(is.na(Diciembre$CargasFamiliares)),])
Diciembre[which(is.na(Diciembre$CargasFamiliares)),]$CargasFamiliares<-1





######################################################################





########################Variable provincia#############################
table(Diciembre$ProvinciaDomicilio)
Diciembre["Provincia"]<-"NA"
Diciembre[which(Diciembre$ProvinciaDomicilio=="AZUAY"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="CHIMBORAZO"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="COTOPAXI"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="EL ORO"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="ESMERALDAS"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="IMBABURA"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="LOJA"),]$Provincia<-1
Diciembre[which(Diciembre$ProvinciaDomicilio=="PICHINCHA"),]$Provincia<-1


Diciembre[which(Diciembre$ProvinciaDomicilio=="BOLIVAR"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="CAÑAR"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="CARCHI"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="GALAPAGOS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="GUAYAS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="MANABI"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="TUNGURAHUA"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="SUCUMBIOS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="ZAMORA CHINCHIPE"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="SANTO DOMINGO DE LOS TSACHILAS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="SANTA ELENA"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="LOS RIOS"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="MORONA SANTIAGO"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="NAPO"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="ORELLANA"),]$Provincia<-0
Diciembre[which(Diciembre$ProvinciaDomicilio=="PASTAZA"),]$Provincia<-0

Diciembre[which(Diciembre$Provincia=="NA"),]$Provincia<-0




Diciembre["Afinidad"]<-"NA"
Diciembre[which(Diciembre$AfinidadTarjeta=="CUOTA FACIL"),]$Afinidad<-1
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - CUOTA FACIL"),]$Afinidad<-1
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - GOLD"),]$Afinidad<-1

Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - BLACK"),]$Afinidad<-0
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - PLATINUM"),]$Afinidad<-0
Diciembre[which(Diciembre$Afinidad=="NA"),]$Afinidad<-0


View(table(Diciembre$Afinidad,Diciembre$Objetivo))

Diciembre["TieneSuperAvance12M"]<-"NA"
Diciembre[which(Diciembre$NumeroConsumosSuperAvance12M>0),]$TieneSuperAvance12M<-1
Diciembre[which(Diciembre$NumeroConsumosSuperAvance12M==0),]$TieneSuperAvance12M<-0
table(Diciembre$TieneSuperAvance12M)


Diciembre["CodEstadoCivil"]<-"NA"
Diciembre[which(Diciembre$EstadoCivil=="CASADO"),]$CodEstadoCivil<-1
Diciembre[which(Diciembre$EstadoCivil=="DIVORCIADO"),]$CodEstadoCivil<-1


Diciembre[which(Diciembre$EstadoCivil=="SOLTERO"),]$CodEstadoCivil<-0
Diciembre[which(Diciembre$EstadoCivil=="UNIÓN LIBRE"),]$CodEstadoCivil<-0
Diciembre[which(Diciembre$EstadoCivil=="VIUDO"),]$CodEstadoCivil<-0

Diciembre[which(Diciembre$CodEstadoCivil=="NA"),]$CodEstadoCivil<-1


Diciembre["TieneBlack"]<-0
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - BLACK"),]$TieneBlack<-1

Diciembre["TieneGold"]<-0
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - GOLD"),]$TieneGold<-1


Diciembre["TienePlatinum"]<-0
Diciembre[which(Diciembre$AfinidadTarjeta=="TARJETA ALIA - PLATINUM"),]$TienePlatinum<-1



Diciembre["SectorComercio"]<-0
Diciembre[which(Diciembre$Sector=="COMERCIO"),]$SectorComercio<-1

Diciembre["SectorDependencia"]<-0
Diciembre[which(Diciembre$Sector=="DEPENDENCIA"),]$SectorDependencia<-1

Diciembre["SectorPersonal"]<-0
Diciembre[which(Diciembre$Sector=="PERSONAL"),]$SectorPersonal<-1

Diciembre["SectorProduccion"]<-0
Diciembre[which(Diciembre$Sector=="PRODUCCION"),]$SectorProduccion<-1

Diciembre["SectorServicios"]<-0
Diciembre[which(Diciembre$Sector=="SERVICIOS"),]$SectorServicios<-1


Diciembre["CodEstadoCivil"]<-1
Diciembre[which(Diciembre$EstadoCivil=="SOLTERO"),]$CodEstadoCivil<-0
Diciembre[which(Diciembre$EstadoCivil=="UNIÓN LIBRE"),]$CodEstadoCivil<-0
Diciembre[which(Diciembre$EstadoCivil=="VIUDO"),]$CodEstadoCivil<-0

Diciembre["Instruccion"]<-1
Diciembre[which(Diciembre$NivelInstruccion=="FORMACION INTERMEDIA"),]$Instruccion<-0
Diciembre[which(Diciembre$NivelInstruccion=="SECUNDARIA"),]$Instruccion<-0
Diciembre[which(Diciembre$NivelInstruccion=="UNIVERSIDAD"),]$Instruccion<-0

Diciembre["HastaPrimaria"]<-0
Diciembre[which(Diciembre$NivelInstruccion=="PRIMARIA"),]$HastaPrimaria<-1

Diciembre["HastaUniversidad"]<-0
Diciembre[which(Diciembre$NivelInstruccion=="UNIVERSIDAD"),]$HastaUniversidad<-1

Diciembre["Consume2omasAvances12M"]<-0
Diciembre[which(Diciembre$NumeroConsumosAvance12M<2.5),]$Consume2omasAvances12M<-1

Diciembre["CupoUsadoAvancesHasta"]<-0
Diciembre[which(Diciembre$`Cupo.Ut.Avances/Cupo.Aprobado`<0.0054),]$CupoUsadoAvancesHasta<-1

Diciembre["CodT6CupoUtilizado"]<-1
Diciembre[which(Diciembre$T6.CupoUtilizado>(-0.31)),]$CodT6CupoUtilizado<-0


Diciembre["TieneMenos300dSinConsumo"]<-1
Diciembre[which(Diciembre$Dias.sin.consumo>300),]$TieneMenos300dSinConsumo<-0



Diciembre["CodCupoUtilizado"]<-0
Diciembre[which(Diciembre$CupoUtilizado<439),]$CodCupoUtilizado<-1



Diciembre["CodPromCupoUtilizado3M"]<-0
Diciembre[which(Diciembre$Prom.CupoUtilizado3<661),]$CodPromCupoUtilizado3M<-1

Diciembre["NormCupoUtilizado"]<-as.numeric(scale(Diciembre$CupoUtilizado))
Diciembre["NormPromCupoUtilizado3M"]<-as.numeric(scale(Diciembre$Prom.CupoUtilizado3))


Diciembre["NormPromCupoUtili3MxTipoClienteActivo"]<-0
Diciembre[which(Diciembre$TipoCliente=="ACTIVO"),]$NormPromCupoUtili3MxTipoClienteActivo<-Diciembre[which(Diciembre$TipoCliente=="ACTIVO"),]$NormPromCupoUtilizado3M


Diciembre["NormPromCupoUtili3MxTipoClienteInactivo"]<-0
Diciembre[which(Diciembre$TipoCliente=="INACTIVO"),]$NormPromCupoUtili3MxTipoClienteInactivo<-Diciembre[which(Diciembre$TipoCliente=="INACTIVO"),]$NormPromCupoUtilizado3M


Diciembre["TieneMas300DiasSinConsumo"]<-0
Diciembre[which(Diciembre$Dias.sin.consumo>300),]$TieneMas300DiasSinConsumo<-1


Diciembre["TieneMas920DiasSinConsumo"]<-1
Diciembre[which(Diciembre$Dias.sin.consumo<946),]$TieneMas920DiasSinConsumo<-0


Diciembre["TieneMenos920DiasSinConsumo"]<-1
Diciembre[which(Diciembre$Dias.sin.consumo>920),]$TieneMenos920DiasSinConsumo<-0

Diciembre["TieneMenos150DiasSinConsumo"]<-1
Diciembre[which(Diciembre$Dias.sin.consumo>150),]$TieneMenos150DiasSinConsumo<-0

Diciembre["Menos8Autorizaciones12M"]<-0
Diciembre[which(Diciembre$Autorizacion12M<8.5),]$Menos8Autorizaciones12M<-1

Diciembre["codCupoUti3MXDiasSinCon"]<-0
Diciembre$codCupoUti3MXDiasSinCon<-Diciembre$TieneMenos920DiasSinConsumo*Diciembre$CodPromCupoUtilizado3M
table(Diciembre$codCupoUti3MXDiasSinCon)


table(Diciembre$NormPromCupoUtilizado3M)
(table(Diciembre$CodT6CupoUtilizado,Diciembre$Objetivo))
table(Diciembre$AfinidadTarjeta)
#################################################################################
Diciembre[which(Diciembre$Mesdesercion>0),]$Objetivo<-0
Diciembre[which(Diciembre$Mesdesercion<=8),]$Objetivo<-1
Diciembre$Objetivo<-as.factor(Diciembre$Objetivo)

Diciembre %>% count(Objetivo)
Diciembre %>% count(Mesdesercion)
Diciembre %>% count(TipoCliente)
Diciembre<-subset(Diciembre,TipoCliente=="INACTIVO")

TrainD <- createDataPartition(Diciembre$IdCuentaTarjeta, p=0.7, list=FALSE)
trainingD <- Diciembre[ TrainD, ]
testingD <- Diciembre[ -TrainD, ]
trainingD %>% count(Objetivo)
testingD %>% count(Objetivo)
####################Balancear el conjunto de entrenamiento#################################
muestra<-sample_n(subset(trainingD, Objetivo==0),32100,replace = FALSE)
#muestra<-sample_n(subset(trainingD, Objetivo==0), 2213,replace = FALSE)
muestra["filtro"]<-1
muestra<-subset(muestra, select = c("IdCuentaTarjeta","filtro"))
Buenos<-subset(trainingD, Objetivo==0) %>%left_join(muestra,by=c("IdCuentaTarjeta" = "IdCuentaTarjeta"))
Buenos$filtro[is.na(Buenos$filtro)]<-0
Buenos<-subset(Buenos,filtro==0)
Buenos<-Buenos[,-237]
########################Malos####################
n<-4
Malos<-do.call("rbind", replicate(n, subset(trainingD, Objetivo==1), simplify = FALSE))

BalanceadaD<-rbind(Buenos,Malos)
BalanceadaD %>% count(Objetivo)

###########################################################################################

modD <- glm(Objetivo~ 
             NormCupoUtilizado+
             # CodPromCupoUtilizado3M+
              #Max_Dias_sin_consumos_3M+
              #NormPromCupoUtilizado3M+
              #NormPromCupoUtili3MxTipoClienteActivo+
              #NormPromCupoUtili3MxTipoClienteInactivo+
            PagosVencidos+
              Otra_Tarjeta+
              SVidaDesgravamen+
             STotalFamiliar+
              SoloSeguro
            #+
              #+TipoCliente
            #+PromMaxDiasSinconsumo3M
            +Afinidad
            +Provincia
            
            #+`Cupo.Utilizado/Cupo.Aprobado`
            #+T3.CupoUtilizado
            #+T6.CupoUtilizado
            #+`Cupo.Ut.Avances/Cupo.Aprobado`
            #+NumeroConsumosAvance3M
            #+Dias.sin.consumo
            #+Prom.DiasMora3
            #+NumeroConsumoConInteres3M
            #+NumeroConsumoSinInteres3M
            #+NumeroConsumosSuperAvance12M
            #+TieneSuperAvance12M
            #+MontoSeguro
            #+EstadoCivil
            #+NivelInstruccion
            #+Sector
            #+Edad.Cliente
            #+CargasFamiliares
            +CodEstadoCivil
            #+SectorPersonal
            #+SectorComercio
            #+SectorDependencia
            #+SectorServicios
            #+SectorProduccion
            #+TieneGold
            #+TieneBlack
            #+TienePlatinum
            
            #+Numero_tarjetasT6
            #+CodT6CupoUtilizado
            
            #+CargasFamiliares
            
           # +CupoUsadoAvancesHasta
            #+Consume2omasAvances12M
            #+Instruccion
            #+HastaPrimaria
            #+HastaUniversidad
            #+TieneMenos300dSinConsumo
            #+TieneMenos300dSinConsumo
            #+TieneMenos150DiasSinConsumo
            #+TieneMenos920DiasSinConsumo
            #+Menos8Autorizaciones12M
            #+codCupoUti3MXDiasSinCon
           #+Dias.sin.consumo
           +Edad.Cuenta.Tc
            , data=BalanceadaD,
            family="binomial"(link = "logit"))

summary(modD)

BalanceadaD %>% count(CodT6CupoUtilizado)

##########################AUC ROC#############################
fitted.results <- predict(modD,newdata=testingD,type='response')
pr <- prediction(fitted.results, testingD$Objetivo)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
str(fitted.results)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

########################Matriz de confusión####################
fitted.results <- predict(modD,newdata=testingD,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
confusionMatrix(testingD$Objetivo, fitted.results)


table(Diciembre$Objetivo, fitted.results)[1,2]
table(Diciembre$Objetivo, fitted.results)[2,2]


#####################
library("ResourceSelection", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")

hoslem.test(modD$y, fitted(modD), g=10)
#######################Desviación estandar#############################

##############################NagelkerkeR2 ######################
library("fmsb", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")

NagelkerkeR2(mod1)

########################R2########################################
library("pscl", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("BaylorEdPsych", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
PseudoR2(modD)
pR2(modD)

######################VIF#####################
vif(modD)
sqrt(vif(modD))

#######################K-S test#############################
################KS - prueba##################33
library("ggplot2", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")
library("plotROC", lib.loc="d:/Users_info/ALBANPD/My Documents/R/R-3.3.2/library")


fitted.results <- predict(modD,newdata=testingD,type='response')
aux<-cbind(testingD,fitted.results)

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
  scale_x_continuous(breaks=seq(0,1,by=0.1))+
  ggtitle("Prueba K-S") +
  annotate("text", x = .75, y = .25, 
           label = paste("K-S =", round(y1-y0, 3))) +
  theme(legend.title=element_blank())
#######################Curva Roc######################################

#######################Curva Roc######################################
fitted.results <- predict(modD,newdata=testingD,type='response')


aux<-cbind(testingD,fitted.results)
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
#fitted.results <- predict(mod1,newdata=Diciembre,type='response')

#quan<-as.vector(quan)

Diciembre["Score"]<-predict(modD,newdata=Diciembre,type='response')
View(quan<-quantile(Diciembre$Score, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)
Diciembre["Decil"]<-"NA"

#aux<-subset(Diciembre,Score==quan[2])
################Codificaccion de P1 - P10 ###########################
Diciembre[which(Diciembre$Score <= quan[2]),]$Decil<-"P1"
Diciembre[which(Diciembre$Score <=quan[3] & Diciembre$Score >quan[2]),]$Decil<-"P2"
Diciembre[which(Diciembre$Score <=quan[4] & Diciembre$Score >quan[3]),]$Decil<-"P3"
Diciembre[which(Diciembre$Score <=quan[5] & Diciembre$Score > quan[4]),]$Decil<-"P4"
Diciembre[which(Diciembre$Score <=quan[6]& Diciembre$Score > quan[5]),]$Decil<-"P5"
Diciembre[which(Diciembre$Score <=quan[7] & Diciembre$Score > quan[6]),]$Decil<-"P6"
Diciembre[which(Diciembre$Score <=quan[8] & Diciembre$Score > quan[7]),]$Decil<-"P7"
Diciembre[which(Diciembre$Score <=quan[9] & Diciembre$Score > quan[8]),]$Decil<-"P8"
Diciembre[which(Diciembre$Score <=quan[10] & Diciembre$Score > quan[9]),]$Decil<-"P9"
Diciembre[which(Diciembre$Score >quan[10]),]$Decil<-"P10"

#Diciembre[which(Diciembre$Score >0.778),]$Decil<-"P20"


aux<-subset(Diciembre,Decil=="NA")
View(table(Diciembre$Decil,Diciembre$Objetivo))
View(table(Diciembre$Mesdesercion))

aux<-subset(Diciembre,Decil=="P9")

View(aux %>% count(Mesdesercion))
View(aux %>% count(Objetivo))

View(aux %>% count(TipoCliente))
View(table(aux$TipoCliente,aux$Objetivo))

View(Diciembre %>% count(Mesdesercion))
###################################
fitted.results <- predict(modD,newdata=Diciembre,type='response')
fitted.results <- ifelse(fitted.results > 0.63,1,0)
confusionMatrix(Diciembre$Objetivo, fitted.results)

###############Gráficos Error tipo I y II###########################
fitted.results <- predict(modD,newdata=Diciembre,type='response')
Aux<-cbind(Diciembre,fitted.results)
qplot(fitted.results, data = Aux, geom = "density",
      color = Objetivo)+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  
  xlab("Score")+
  ylab("Densidad")+ 
  ggtitle("Error Tipo I y II")+
  theme_bw()



fitted.results <- predict(modD,newdata=Diciembre,type='response')
Aux<-cbind(Diciembre,fitted.results)
qplot(fitted.results, data = Aux, geom = "histogram",
      color = Objetivo)+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  xlab("Score")+
  ylab("Densidad")+ 
  ggtitle("Error Tipo I y II")+
  theme_bw()


##########################P promedio####################
aux<-subset(Diciembre,Decil=="P10")
ddply(aux, .(Mesdesercion), summarize,  Rate1=mean(Score))
table(aux$Score,aux$Mesdesercion)
mean(aux$Score)
###############################################3

##########################Distancia de Mahalanobis#####################
Aux<-subset(Diciembre,Objetivo==0)
c1<-cut(Aux$Score,breaks = seq(from = 0, to = 1, by =0.01,right=FALSE))
c1<-as.data.frame(table(c1))
c1$Freq
Aux<-subset(Diciembre,Objetivo==1)
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
  fitted.results <- predict(modD,newdata=Diciembre,type='response')
  fitted.results <- ifelse(fitted.results > z[15-i],1,0)
  #confusionMatrix(Diciembre$Objetivo, fitted.results)
  y[15-i,1]<-table(Diciembre$Objetivo, fitted.results)[1,2]
  y[15-i,2]<-table(Diciembre$Objetivo, fitted.results)[2,2]}
View(y)
###########################################################




################Arbol#############################
fit = rpart(Objetivo~ 
              NumeroConsumosAvance12M,
            method="class",
data=BalanceadaD)


rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)

################Particion por intervalos#################

aux<-subset(Diciembre,Objetivo==1)
summary(aux$NumeroConsumosAvance12M)
c1<-cut(aux$NumeroConsumosAvance12M,breaks=c(-1.1,2.5,71))
View(table(c1))



Abril<-anti_join(Abril,aux,by=c("Identificion"))


##############Conjunto de Entrenamiento################################

trainingD["Score"]<-predict(modD,newdata=trainingD,type='response')
View(quan<-quantile(trainingD$Score, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)
trainingD["Decil"]<-"NA"

#aux<-subset(Diciembre,Score==quan[2])
################Codificaccion de P1 - P10 ###########################
trainingD[which(trainingD$Score <= quan[2]),]$Decil<-"P1"
trainingD[which(trainingD$Score <=quan[3] & trainingD$Score >quan[2]),]$Decil<-"P2"
trainingD[which(trainingD$Score <=quan[4] & trainingD$Score >quan[3]),]$Decil<-"P3"
trainingD[which(trainingD$Score <=quan[5] & trainingD$Score > quan[4]),]$Decil<-"P4"
trainingD[which(trainingD$Score <=quan[6] & trainingD$Score > quan[5]),]$Decil<-"P5"
trainingD[which(trainingD$Score <=quan[7] & trainingD$Score > quan[6]),]$Decil<-"P6"
trainingD[which(trainingD$Score <=quan[8] & trainingD$Score > quan[7]),]$Decil<-"P7"
trainingD[which(trainingD$Score <=quan[9] & trainingD$Score > quan[8]),]$Decil<-"P8"
trainingD[which(trainingD$Score <=quan[10] & trainingD$Score > quan[9]),]$Decil<-"P9"
trainingD[which(trainingD$Score >quan[10]),]$Decil<-"P10"

View(table(trainingD$Decil,trainingD$Objetivo))


##############Conjunto de Prueba################################

testingD["Score"]<-predict(modD,newdata=testingD,type='response')
View(quan<-quantile(testingD$Score, prob = seq(0, 1, length = 11), type = 5))
quan<-as.vector(quan)
testingD["Decil"]<-"NA"

#aux<-subset(Diciembre,Score==quan[2])
################Codificaccion de P1 - P10 ###########################
testingD[which(testingD$Score <= quan[2]),]$Decil<-"P1"
testingD[which(testingD$Score <=quan[3] & testingD$Score >quan[2]),]$Decil<-"P2"
testingD[which(testingD$Score <=quan[4] & testingD$Score >quan[3]),]$Decil<-"P3"
testingD[which(testingD$Score <=quan[5] & testingD$Score > quan[4]),]$Decil<-"P4"
testingD[which(testingD$Score <=quan[6] & testingD$Score > quan[5]),]$Decil<-"P5"
testingD[which(testingD$Score <=quan[7] & testingD$Score > quan[6]),]$Decil<-"P6"
testingD[which(testingD$Score <=quan[8] & testingD$Score > quan[7]),]$Decil<-"P7"
testingD[which(testingD$Score <=quan[9] & testingD$Score > quan[8]),]$Decil<-"P8"
testingD[which(testingD$Score <=quan[10] & testingD$Score > quan[9]),]$Decil<-"P9"
testingD[which(testingD$Score >quan[10]),]$Decil<-"P10"

View(table(testingD$Decil,testingD$Objetivo))