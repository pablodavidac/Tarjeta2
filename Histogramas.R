####################Histogramas de variables##########################
summary(Diciembre$CupoUtilizado)

Aux<-subset(Base,Objetivo==0 & is.finite(Base$Edad.Cuenta.Tc))


#######################Cupo Utilizado################################

################Dsitribución acumulada##################33
ggplot(Diciembre, aes(x = Edad.Cuenta.Tc)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Cupo Utilizado", x="Dólares", y="Densidad acumulada") +
 # scale_x_continuous(breaks=seq(0,12000,by=1000))+
  #guide_legend(label.position="bottom")+
  theme(legend.position="bottom")+
  theme_bw()


###############Densindades###########################

qplot(CupoUtilizado, data = Diciembre, geom = "density",
      color = Objetivo)+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  
  xlab("Dólares")+
  ylab("Densidad")+ 
  ggtitle("Cupo Utilizado")+
  theme_bw()



################################################################

#################Pagos vencidos#####################3
################Dsitribución acumulada##################33
ggplot(Diciembre, aes(x = PagosVencidos)) + 
  stat_ecdf(aes(group = Objetivo, colour = Objetivo))+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  labs(title="Pagos Vencidos", x="", y="Densidad acumulada") +
  # scale_x_continuous(breaks=seq(0,12000,by=1000))+
  #guide_legend(label.position="bottom")+
  theme(legend.position="bottom")+
  theme_bw()


###############Densindades###########################

qplot(PagosVencidos, data = Diciembre, geom = "density",
      color = Objetivo)+
  scale_color_manual(values=c("orange","blue"),
                     labels=c("No desertores","Desertores"))+
  
  xlab("")+
  ylab("Densidad")+ 
  ggtitle("Pagos Vencidos")+
  theme_bw()


############################################################3



##########################Puntos de corte codificacion Diciembre 9 meses################



##########Prom Cupo Utilizado 3 meses##############
fit = rpart(Objetivo~ 
              Prom.CupoUtilizado6,
            method="class",
            #control=rpart.control(minsplit=30, cp=0.01),
            data=BalanceadaD)



rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)

#############################3


########## Cupo Utilizado t/t-6##############
fit = rpart(Objetivo~ 
              T6.CupoUtilizado,
            method="class",
            #control=rpart.control(minsplit=30, cp=0.01),
            data=BalanceadaD)



rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)

########## Cupo Utilizado en avances##############
fit = rpart(Objetivo~ 
              `Cupo.Ut.Avances/Cupo.Aprobado`,
            method="class",
            #control=rpart.control(minsplit=30, cp=0.01),
            data=BalanceadaD)



rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)


########## Dias sin consumo##############
fit = rpart(Objetivo~ 
              Dias.sin.consumo,
            method="class",
            #control=rpart.control(minsplit=30, cp=0.01),
            data=BalanceadaD)



rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)



########## Autorizaciones 12 M##############
fit = rpart(Objetivo~ 
              Autorizacion12M,
            method="class",
            #control=rpart.control(minsplit=30, cp=0.01),
            data=BalanceadaD)



rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)
#############################3




########## Autorizaciones 12 M##############
fit = rpart(Objetivo~ 
              Prom.DiasMora,
            method="class",
            #control=rpart.control(minsplit=30, cp=0.01),
            data=BalanceadaD)



rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)
#############################3
fit = rpart(Objetivo~ 
              Dias.sin.consumo,
              method="class",
            #control=rpart.control(minsplit=30, cp=0.01),
            data=BalanceadaD)



rpart.plot(fit,box.palette="Blues", branch.lty=2, nn=TRUE,extra=101,
           cex=0.8,
           tweak=0.75)


