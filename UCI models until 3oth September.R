#Package#
library(readr)
library(ggplot2)
library(dplyr)
#library(gtable)
library(gridExtra)
library(cowplot)
library(gamlss)
library(zoo)

#Data base#
pacientes_UCI_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto9/HospitalizadosUCIEtario_std.csv"
UCI_edad = read_csv(url(pacientes_UCI_edad))
colnames(UCI_edad) = c("Edades", "Fecha", "Datos")

casos_rango_etario = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto16/CasosGeneroEtario_std.csv"
casos_por_edad = read_csv(url(casos_rango_etario))
colnames(casos_por_edad) = c("Grupo_edad", "Sexo", "Fecha", "Datos")

vacunas_por_region = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto76/vacunacion_std.csv"
vacunacion_regional = read_csv(url(vacunas_por_region))

vacunados_1dosis_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_1eraDosis_std.csv"
vac_1dosis = read_csv(url(vacunados_1dosis_edad))
vac_1dosis[is.na(vac_1dosis)] = 0

vacunados_2dosis_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_2daDosis_std.csv"
vac_2dosis = read_csv(url(vacunados_2dosis_edad))
vac_2dosis[is.na(vac_2dosis)] = 0

fallecidos_por_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto10/FallecidosEtario_std.csv"
fallecidos_etario = read_csv(url(fallecidos_por_edad))

vacunas_totales = vacunacion_regional %>% filter(Region=="Total",Fecha>="2021-02-03") %>% 
  group_by(Fecha) %>% summarise(datos = sum(Cantidad))

vacunas.acumuladas1 = vacunacion_regional %>% filter(Dosis=="Primera", Region=="Total",Fecha>="2021-02-03")

vacunas.acumuladas2 = vacunacion_regional %>% filter(Dosis=="Segunda", Region=="Total",Fecha>="2021-02-03")

dosis.1 = c(rep("Primera dosis", length(vac_1dosis$`Primera Dosis`)))
dosis.1 = cbind(dosis.1, vac_1dosis)
colnames(dosis.1) = c("Dosis", "Edad", "Fecha", "Vacunas")

dosis.2 = c(rep("Primera dosis", length(vac_2dosis$`Segunda Dosis`)))
dosis.2 = cbind(dosis.2, vac_2dosis)
colnames(dosis.2) = c("Dosis", "Edad", "Fecha", "Vacunas")

vac.t = rbind(dosis.1, dosis.2)
vac.total = vac.t %>% group_by(Fecha, Edad) %>% summarise(Vacunas = sum(Vacunas))

vacunas.diarias.1 = vac_1dosis %>% filter(Fecha>="2021-02-03") %>% 
  group_by(Fecha) %>% summarise(prim.dosis = sum(`Primera Dosis`))


#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#WEEKLY CASES UNDER 39
poblacion.18_39 = 6645912

me39sem = casos_por_edad %>% filter(Grupo_edad<="35 - 39 años") %>% group_by(Fecha) %>% 
  summarise(Casos = sum(Datos))

me39_diarios = c()
for (i in 1:length(me39sem$Fecha)){
  c = me39sem$Casos[i] - me39sem$Casos[i-1]
  me39_diarios = append(me39_diarios,c)
} 
me39_diarios[is.na(me39_diarios)] = 0
me39_diarios = as.numeric(me39_diarios)
me39_diarios = data.frame(me39_diarios)
Rango_etario = c(rep("<=39", length(me39_diarios$me39_diarios)))
Rango_etario = data.frame(Rango_etario)
me39_diarios = cbind(Rango_etario, me39sem$Fecha[2:length(me39sem$Fecha)], me39_diarios)
colnames(me39_diarios) = c("Rango_etario","Fechas", "Datos")
sum(me39_diarios$Datos)
me39_diarios = me39_diarios %>% filter(Fechas>="2021-02-01")

Fecha = c()
for (i in seq(1,length(me39_diarios$Fechas), by=2)){
  c = (me39_diarios$Fechas[i])
  print(c)
  Fecha = append(Fecha, c)
}
Fecha = data.frame(Fecha)

me39_sem = c()
for (i in seq(1,length(me39_diarios$Datos), by=2)){
  c = (me39_diarios$Datos[i]+me39_diarios$Datos[i+1])
  me39_sem = append(me39_sem, c)
}
me39_sem = data.frame(me39_sem)
me39_sem = na.omit(me39_sem)

vacunas1_semanal = vac_1dosis %>% filter(Edad>="18", Edad<="39", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.1d = sum(`Primera Dosis`))
sum(vacunas1_semanal$vac.1d)
v = c()
c = 0
for (i in 1:length(vacunas1_semanal$vac.1d)) {
  vacunas1_semanal$vac.1d[i] + c
  c = vacunas1_semanal$vac.1d[i] + c
  v = append(v,c)}
a = cbind(vacunas1_semanal[,1], v)

vacunas.1dosis.sem = c()
for (i in seq(1,length(vacunas1_semanal$vac.1d),by=7)){
  c = (a$v[i+2])
  vacunas.1dosis.sem = append(vacunas.1dosis.sem,c)
} 
vacunas.1dosis.sem = data.frame(vacunas.1dosis.sem)

vacunas2_semanal = vac_2dosis %>% filter(Edad>="18", Edad<="39", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.2d = sum(`Segunda Dosis`))
v = c()
c = 0
for (i in 1:length(vacunas2_semanal$vac.2d)) {
  vacunas2_semanal$vac.2d[i] + c
  c = vacunas2_semanal$vac.2d[i] + c
  v = append(v,c)
}
b = cbind(vacunas2_semanal[,1], v)
vacunas.2dosis.sem = c()
for (i in seq(1,length(vacunas2_semanal$vac.2d),by=7)){
  c = (b$v[i+2])
  vacunas.2dosis.sem = append(vacunas.2dosis.sem,c)
} 
vacunas.2dosis.sem = data.frame(vacunas.2dosis.sem)

uci_me39_semanal = UCI_edad %>% filter(Fecha>="2021-01-30", Edades=="<=39") %>% 
  group_by(Fecha) %>% summarise(camas.me39 = sum(Datos))
uci_me39.sem = c()
for (i in seq(1,length(uci_me39_semanal$camas.me39),by=7)){
  c = ((uci_me39_semanal$camas.me39[i]+uci_me39_semanal$camas.me39[i+1]+uci_me39_semanal$camas.me39[i+2]+
          uci_me39_semanal$camas.me39[i+3]+uci_me39_semanal$camas.me39[i+4]+uci_me39_semanal$camas.me39[i+5]+
          uci_me39_semanal$camas.me39[i+6])/7)
  uci_me39.sem = append(uci_me39.sem,c)
} 
uci_me39.sem = data.frame(uci_me39.sem)

vacunados.total.sem = vac.total %>% filter(Edad<="39", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vacunados = sum(Vacunas))
v = c()
c = 0
for (i in 1:length(vacunados.total.sem$vacunados)) {
  vacunados.total.sem$vacunados[i] + c
  c = vacunados.total.sem$vacunados[i] + c
  v = append(v,c)
}
d = cbind(vacunados.total.sem[,1], v)
vac.me39.sem = c()
for (i in seq(1,length(vacunados.total.sem$vacunados),by=7)){
  c = (d$v[i+2])
  vac.me39.sem = append(vac.me39.sem,c)
} 
vac.me39.sem = data.frame(vac.me39.sem)

me39.sem = cbind(Fecha, me39_sem[1:length(Fecha$Fecha),], uci_me39.sem[1:length(Fecha$Fecha),], 
                 vac.me39.sem[1:length(Fecha$Fecha),], vacunas.1dosis.sem[1:length(Fecha$Fecha),], 
                 vacunas.2dosis.sem[1:length(Fecha$Fecha),])
colnames(me39.sem) = c("Fecha", "Casos", "Camas", "Vac.totales", "Vac.1d", "Vac.2d")
me39.sem = na.omit(me39.sem)
me39.sem = me39.sem %>% filter(Fecha<="2021-09-30")



me39.sem$Vac.2d[19]/poblacion.18_39
me39.sem$Vac.1d[length(me39.sem$Vac.1d)]/poblacion.18_39
me39.sem$Vac.2d[length(me39.sem$Vac.2d)]/poblacion.18_39

ggplot(me39.sem, aes(Vac.totales, Camas))+
  geom_point()+
  geom_smooth()

ggplot(me39.sem, aes(Fecha, Camas))+
  geom_point()+
  geom_smooth()


#Models
mod.me39.1 = gamlss(formula = Camas~Casos,
                    family=PIG, trace=F,data = me39.sem)
mod.me39.2 = gamlss(formula = Camas~Vac.totales,
                    family=PIG, trace=F,data = me39.sem)
mod.me39.3 = gamlss(formula = Camas~Casos+Vac.totales,
                    family=PIG, trace=F,data = me39.sem)
mod.me39.4 = gamlss(formula = Camas~Vac.1d+Vac.2d,
                    family=PIG, trace=F,data = me39.sem)
mod.me39.5 = gamlss(formula = Camas~Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                    family=PIG, trace=F,data = me39.sem)
mod.me39.6 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d,
                    family=PIG, trace=F,data = me39.sem)
mod.me39.7 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                    family=PIG, trace=F,data = me39.sem)

#Evaluations
GAIC(mod.me39.1, mod.me39.2, mod.me39.3, mod.me39.4, mod.me39.5, mod.me39.6, mod.me39.7)
VC.test(mod.me39.5, mod.me39.7)
r1_me39 = Rsq(mod.me39.5, type = c("Cox Snell","Cragg Uhler","both")); r1_me39
r2_me39 = Rsq(mod.me39.4, type = c("Cox Snell","Cragg Uhler","both")); r2_me39
summary(mod.me39.5)
plot(mod.me39.5)
wp(mod.me39.5, ylim.all = 0.8)
drop1(mod.me39.5, parallel = "multicore", ncpus = 4)
VC.test(mod.me39.5, mod.me39.4) #No hay diferencias
term.plot(mod.me39.5, pages = 1, ask = FALSE, rug = TRUE)
me39.sem = me39.sem %>% mutate(pred_me39 = fitted(mod.me39.5))
me39.sem = me39.sem %>% mutate(pred_me39.malo = fitted(mod.me39.4))

#Graphics
a1 = ggplot(me39.sem, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred_me39), se=TRUE, method = "gam",color="#41b6c4")+
  labs(x="Dates",
       y="ICU beds occupied weekly",
       tag = "A")+
  annotate(geom="text",x=as.Date("2021-04-02"),
           y=1000,label=paste("R\u00b2 = ", round(r1_me39,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 1200))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
a1

a2 = ggplot(me39.sem, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred_me39.malo), se=TRUE, method = "gam", color="#41b6c4")+
  labs(x="Dates",
       y="ICU beds occupied weekly")+
  annotate(geom="text",x=as.Date("2021-04-02"),
           y=1000,label=paste("R\u00b2 = ",round(r2_me39,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 1200))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
a2

me39.sem$Vac.1d[18]/poblacion.18_39
me39.sem$Vac.2d[18]/poblacion.18_39

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#WEEKLY CASES BETWEEN 40 and 49 years
poblacion.de40_49 = 2618520

casos40_49 = casos_por_edad %>% filter(Grupo_edad>="40 - 44 años", Grupo_edad<="45 - 49 años") %>% 
  group_by(Fecha) %>% summarise(Casos = sum(Datos))

de40_49.diarios = c()
for (i in 1:length(casos40_49$Fecha)){
  c = casos40_49$Casos[i] - casos40_49$Casos[i-1]
  de40_49.diarios = append(de40_49.diarios,c)
} 
de40_49.diarios[is.na(de40_49.diarios)] = 0
de40_49.diarios = as.numeric(de40_49.diarios)
de40_49.diarios = data.frame(de40_49.diarios)
Rango_etario = c(rep("40-49", length(de40_49.diarios$de40_49.diarios)))
Rango_etario = data.frame(Rango_etario)
de40_49.diarios = cbind(Rango_etario, casos40_49$Fecha[2:length(casos40_49$Fecha)], de40_49.diarios)
colnames(de40_49.diarios) = c("Rango_etario","Fechas", "Datos")
sum(de40_49.diarios$Datos)
de40_49.diarios = de40_49.diarios %>% filter(Fechas>="2021-02-01")

Fecha = c()
for (i in seq(1,length(de40_49.diarios$Fechas), by=2)){
  c = (de40_49.diarios$Fechas[i])
  Fecha = append(Fecha, c)
}
Fecha = data.frame(Fecha)

de40_49.sem = c()
for (i in seq(1,length(de40_49.diarios$Datos), by=2)){
  c = (de40_49.diarios$Datos[i]+de40_49.diarios$Datos[i+1])
  de40_49.sem = append(de40_49.sem, c)
}
de40_49.sem = data.frame(de40_49.sem)
de40_49.sem = na.omit(de40_49.sem)

vacunas1_semanal = vac_1dosis %>% filter(Edad>="40", Edad<="49", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.1d = sum(`Primera Dosis`))
sum(vacunas1_semanal$vac.1d)
v = c()
c = 0
for (i in 1:length(vacunas1_semanal$vac.1d)) {
  vacunas1_semanal$vac.1d[i] + c
  c = vacunas1_semanal$vac.1d[i] + c
  v = append(v,c)}
a = cbind(vacunas1_semanal[,1], v)

vacunas.1dosis.sem = c()
for (i in seq(1,length(vacunas1_semanal$vac.1d),by=7)){
  c = (a$v[i+2])
  vacunas.1dosis.sem = append(vacunas.1dosis.sem,c)
} 
vacunas.1dosis.sem = data.frame(vacunas.1dosis.sem)

vacunas2_semanal = vac_2dosis %>% filter(Edad>="40", Edad<="49", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.2d = sum(`Segunda Dosis`))
v = c()
c = 0
for (i in 1:length(vacunas2_semanal$vac.2d)) {
  vacunas2_semanal$vac.2d[i] + c
  c = vacunas2_semanal$vac.2d[i] + c
  v = append(v,c)
}
b = cbind(vacunas2_semanal[,1], v)
vacunas.2dosis.sem = c()
for (i in seq(1,length(vacunas2_semanal$vac.2d),by=7)){
  c = (b$v[i+2])
  vacunas.2dosis.sem = append(vacunas.2dosis.sem,c)
} 
vacunas.2dosis.sem = data.frame(vacunas.2dosis.sem)

vacunados.total.sem = vac.total %>% filter(Edad>="40", Edad<="49", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vacunados = sum(Vacunas))
v = c()
c = 0
for (i in 1:length(vacunados.total.sem$vacunados)) {
  vacunados.total.sem$vacunados[i] + c
  c = vacunados.total.sem$vacunados[i] + c
  v = append(v,c)
}
d = cbind(vacunados.total.sem[,1], v)
vac.40_49.sem = c()
for (i in seq(1,length(vacunados.total.sem$vacunados),by=7)){
  c = (d$v[i+2])
  vac.40_49.sem = append(vac.40_49.sem,c)
} 
vac.40_49.sem = data.frame(vac.40_49.sem)


uci.40_49.semanal = UCI_edad %>% filter(Fecha>="2021-01-30", Edades=="40-49") %>% 
  group_by(Fecha) %>% summarise(camas = sum(Datos))
uci40_49.sem = c()
for (i in seq(1,length(uci.40_49.semanal$camas),by=7)){
  c = ((uci.40_49.semanal$camas[i]+uci.40_49.semanal$camas[i+1]+uci.40_49.semanal$camas[i+2]+
          uci.40_49.semanal$camas[i+3]+uci.40_49.semanal$camas[i+4]+uci.40_49.semanal$camas[i+5]+
          uci.40_49.semanal$camas[i+6])/7)
  uci40_49.sem = append(uci40_49.sem,c)
} 
uci40_49.sem = data.frame(uci40_49.sem)


de40_49.sem = cbind(Fecha, de40_49.sem[1:length(Fecha$Fecha),], uci40_49.sem[1:length(Fecha$Fecha),], 
                    vac.40_49.sem[1:length(Fecha$Fecha),], vacunas.1dosis.sem[1:length(Fecha$Fecha),], 
                    vacunas.2dosis.sem[1:length(Fecha$Fecha),])
colnames(de40_49.sem) = c("Fecha", "Casos", "Camas", "Vac.totales", "Vac.1d", "Vac.2d")
de40_49.sem = na.omit(de40_49.sem)
de40_49.sem = de40_49.sem %>% filter(Fecha<="2021-09-30")


de40_49.sem$Vac.2d[17]/poblacion.de40_49
de40_49.sem$Vac.1d[length(de40_49.sem$Vac.1d)]/poblacion.de40_49
de40_49.sem$Vac.2d[length(de40_49.sem$Vac.2d)]/poblacion.de40_49

ggplot(de40_49.sem, aes(Fecha, Camas))+
  geom_point()+
  geom_smooth()

#Models
mod.40_49.1 = gamlss(formula = Camas~Casos,
                    family=PIG, trace=F,data = de40_49.sem)
mod.40_49.2 = gamlss(formula = Camas~Vac.totales,
                    family=PIG, trace=F,data = de40_49.sem)
mod.40_49.3 = gamlss(formula = Camas~Casos+Vac.totales,
                    family=PIG, trace=F,data = de40_49.sem)
mod.40_49.4 = gamlss(formula = Camas~Vac.1d+Vac.2d,
                    family=PIG, trace=F,data = de40_49.sem)
mod.40_49.5 = gamlss(formula = Camas~Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                    family=PIG, trace=F,data = de40_49.sem)
mod.40_49.6 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d,
                    family=PIG, trace=F,data = de40_49.sem)
mod.40_49.7 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                    family=PIG, trace=F,data = de40_49.sem)

#Evaluation
GAIC(mod.40_49.1, mod.40_49.2, mod.40_49.3, mod.40_49.4, mod.40_49.5, mod.40_49.6, mod.40_49.7)
VC.test(mod.40_49.7, mod.40_49.3)
r1_40_49 = Rsq(mod.40_49.7, type = c("Cox Snell","Cragg Uhler","both")); r1_40_49
r2_40_49 = Rsq(mod.40_49.6, type = c("Cox Snell","Cragg Uhler","both")); r2_40_49
summary(mod.40_49.7)
plot(mod.40_49.7)
wp(mod.40_49.7, ylim.all = 0.8)
drop1(mod.40_49.7, parallel = "multicore", ncpus = 4)
VC.test(mod.40_49.7, mod.40_49.6) #Hay diferencias entre modelos
term.plot(mod.40_49.7, pages = 1, ask = FALSE, rug = TRUE)
de40_49.sem = de40_49.sem %>% mutate(pred.40_49 = fitted(mod.40_49.7))
de40_49.sem = de40_49.sem %>% mutate(pred.40_49.def = fitted(mod.40_49.6))

#Graphics
b1 = ggplot(de40_49.sem, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred.40_49), se=TRUE, method = "gam", color="#41b6c4")+
  labs(x = "Dates",
       y = "ICU beds occupied weekly",
       tag = "B")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=700,label=paste("R\u00b2 = ", round(r1_40_49,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 800))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
b1

b2 = ggplot(de40_49.sem, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred.40_49.def), se=TRUE, method = "gam", color="#41b6c4")+
  labs(x = "Dates",
       y = "ICU beds occupied weekly")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=700,label=paste("R\u00b2 = ", round(r2_40_49,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 800))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
b2

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#WEEKLY CASES BETWEEN 50 and 59 years
poblacion.50_59 = 2369901

casos50_59 = casos_por_edad %>% filter(Grupo_edad>="50 - 54 años", Grupo_edad<="55 - 59 años") %>% 
  group_by(Fecha) %>% summarise(Casos = sum(Datos))

de50_59.diarios = c()
for (i in 1:length(casos50_59$Fecha)){
  c = casos50_59$Casos[i] - casos50_59$Casos[i-1]
  de50_59.diarios = append(de50_59.diarios,c)
} 
de50_59.diarios[is.na(de50_59.diarios)] = 0
de50_59.diarios = as.numeric(de50_59.diarios)
de50_59.diarios = data.frame(de50_59.diarios)
Rango_etario = c(rep("50-59", length(de50_59.diarios$de50_59.diarios)))
Rango_etario = data.frame(Rango_etario)
de50_59.diarios = cbind(Rango_etario, casos50_59$Fecha[2:length(casos50_59$Fecha)], de50_59.diarios)
colnames(de50_59.diarios) = c("Rango_etario","Fechas", "Datos")
sum(de50_59.diarios$Datos)
de50_59.diarios = de50_59.diarios %>% filter(Fechas>="2021-02-01")

Fecha = c()
for (i in seq(1,length(de50_59.diarios$Fechas), by=2)){
  c = (de50_59.diarios$Fechas[i])
  Fecha = append(Fecha, c)
}
Fecha = data.frame(Fecha)

de50_59.sem = c()
for (i in seq(1,length(de50_59.diarios$Datos), by=2)){
  c = (de50_59.diarios$Datos[i]+de50_59.diarios$Datos[i+1])
  de50_59.sem = append(de50_59.sem, c)
}
de50_59.sem = data.frame(de50_59.sem)
de50_59.sem = na.omit(de50_59.sem)

vacunas1_semanal = vac_1dosis %>% filter(Edad>="50", Edad<="59", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.1d = sum(`Primera Dosis`))
sum(vacunas1_semanal$vac.1d)
v = c()
c = 0
for (i in 1:length(vacunas1_semanal$vac.1d)) {
  vacunas1_semanal$vac.1d[i] + c
  c = vacunas1_semanal$vac.1d[i] + c
  v = append(v,c)}
a = cbind(vacunas1_semanal[,1], v)

vacunas.1dosis.sem = c()
for (i in seq(1,length(vacunas1_semanal$vac.1d),by=7)){
  c = (a$v[i+2])
  vacunas.1dosis.sem = append(vacunas.1dosis.sem,c)
} 
vacunas.1dosis.sem = data.frame(vacunas.1dosis.sem)

vacunas2_semanal = vac_2dosis %>% filter(Edad>="50", Edad<="59", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.2d = sum(`Segunda Dosis`))
v = c()
c = 0
for (i in 1:length(vacunas2_semanal$vac.2d)) {
  vacunas2_semanal$vac.2d[i] + c
  c = vacunas2_semanal$vac.2d[i] + c
  v = append(v,c)
}
b = cbind(vacunas2_semanal[,1], v)
vacunas.2dosis.sem = c()
for (i in seq(1,length(vacunas2_semanal$vac.2d),by=7)){
  c = (b$v[i+2])
  vacunas.2dosis.sem = append(vacunas.2dosis.sem,c)
} 
vacunas.2dosis.sem = data.frame(vacunas.2dosis.sem)

vacunados.total.sem = vac.total %>% filter(Edad>="50", Edad<="59", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vacunados = sum(Vacunas))
v = c()
c = 0
for (i in 1:length(vacunados.total.sem$vacunados)) {
  vacunados.total.sem$vacunados[i] + c
  c = vacunados.total.sem$vacunados[i] + c
  v = append(v,c)
}
d = cbind(vacunados.total.sem[,1], v)
vac.50_59.sem = c()
for (i in seq(1,length(vacunados.total.sem$vacunados),by=7)){
  c = (d$v[i+2])
  vac.50_59.sem = append(vac.50_59.sem,c)
} 
vac.50_59.sem = data.frame(vac.50_59.sem)


uci.50_59.semanal = UCI_edad %>% filter(Fecha>="2021-01-30", Edades=="50-59") %>% 
  group_by(Fecha) %>% summarise(camas = sum(Datos))
uci50_59.sem = c()
for (i in seq(1,length(uci.50_59.semanal$camas),by=7)){
  c = ((uci.50_59.semanal$camas[i]+uci.50_59.semanal$camas[i+1]+uci.50_59.semanal$camas[i+2]+
          uci.50_59.semanal$camas[i+3]+uci.50_59.semanal$camas[i+4]+uci.50_59.semanal$camas[i+5]+
          uci.50_59.semanal$camas[i+6])/7)
  uci50_59.sem = append(uci50_59.sem,c)
} 
uci50_59.sem = data.frame(uci50_59.sem)


de50_59.sem = cbind(Fecha, de50_59.sem[1:length(Fecha$Fecha),], uci50_59.sem[1:length(Fecha$Fecha),], 
                    vac.50_59.sem[1:length(Fecha$Fecha),], vacunas.1dosis.sem[1:length(Fecha$Fecha),], 
                    vacunas.2dosis.sem[1:length(Fecha$Fecha),])
colnames(de50_59.sem) = c("Fecha", "Casos", "Camas", "Vac.totales", "Vac.1d", "Vac.2d")
de50_59.sem = na.omit(de50_59.sem)
de50_59.sem = de50_59.sem %>% filter(Fecha<="2021-09-30")

de50_59.sem$Vac.2d[13]/poblacion.50_59
de50_59.sem$Vac.1d[length(de50_59.sem$Vac.1d)]/poblacion.50_59
de50_59.sem$Vac.2d[length(de50_59.sem$Vac.2d)]/poblacion.50_59

ggplot(de50_59.sem, aes(Fecha, Camas))+
  geom_point()+
  geom_smooth()


#Models
mod.50_59.1 = gamlss(formula = Camas~Casos,
                     family=PIG, trace=F,data = de50_59.sem)
mod.50_59.2 = gamlss(formula = Camas~Vac.totales,
                     family=PIG, trace=F,data = de50_59.sem)
mod.50_59.3 = gamlss(formula = Camas~Casos+Vac.totales,
                     family=PIG, trace=F,data = de50_59.sem)
mod.50_59.4 = gamlss(formula = Camas~Vac.1d+Vac.2d,
                     family=PIG, trace=F,data = de50_59.sem)
mod.50_59.5 = gamlss(formula = Camas~Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                     family=PIG, trace=F,data = de50_59.sem)
mod.50_59.6 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d,
                     family=PIG, trace=F,data = de50_59.sem)
mod.50_59.7 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                     family=PIG, trace=F,data = de50_59.sem)

#Evaluations
GAIC(mod.50_59.1, mod.50_59.2, mod.50_59.3, mod.50_59.4, mod.50_59.5, mod.50_59.6, mod.50_59.7)
VC.test(mod.50_59.7, mod.50_59.6)
r1_50_59 = Rsq(mod.50_59.7, type = c("Cox Snell","Cragg Uhler","both")); r1_50_59
r2_50_59 = Rsq(mod.50_59.6, type = c("Cox Snell","Cragg Uhler","both")); r2_50_59
summary(mod.50_59.7)
plot(mod.50_59.7)
wp(mod.50_59.7, ylim.all = 0.8)
drop1(mod.50_59.7, parallel = "multicore", ncpus = 4)
VC.test(mod.50_59.7, mod.50_59.6) #Ha diferencias entre modelos
term.plot(mod.50_59.7, pages = 1, ask = FALSE, rug = TRUE)
de50_59.sem = de50_59.sem %>% mutate(pred.50_59 = fitted(mod.50_59.7))
de50_59.sem = de50_59.sem %>% mutate(pred.50_59.def = fitted(mod.50_59.6))


#Graphics
c1 = ggplot(de50_59.sem, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred.50_59), se=TRUE, method = "gam", color="#41b6c4")+
  labs(x = "Dates",
       y = "ICU beds occupied weekly",
       tag = "C")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=1100,label=paste("R\u00b2 = ", round(r1_50_59,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 1200))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
c1

c2 = ggplot(de50_59.sem, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred.50_59.def), se=TRUE, method = "gam", color="#41b6c4")+
  labs(x = "Dates",
       y = "ICU beds occupied weekly")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=1100,label=paste("R\u00b2 = ", round(r2_50_59,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 1200))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
c2

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#WEEKLY CASES BETWEEN 60 and 69 years old
poblacion.60_69 = 1804002

casos60_69 = casos_por_edad %>% filter(Grupo_edad>="60 - 64 años", Grupo_edad<="65 - 69 años") %>% 
  group_by(Fecha) %>% summarise(Casos = sum(Datos))

de60_69.diarios = c()
for (i in 1:length(casos60_69$Fecha)){
  c = casos60_69$Casos[i] - casos60_69$Casos[i-1]
  de60_69.diarios = append(de60_69.diarios,c)
} 
de60_69.diarios[is.na(de60_69.diarios)] = 0
de60_69.diarios = as.numeric(de60_69.diarios)
de60_69.diarios = data.frame(de60_69.diarios)
Rango_etario = c(rep("60-69", length(de60_69.diarios$de60_69.diarios)))
Rango_etario = data.frame(Rango_etario)
de60_69.diarios = cbind(Rango_etario, casos60_69$Fecha[2:length(casos60_69$Fecha)], de60_69.diarios)
colnames(de60_69.diarios) = c("Rango_etario","Fechas", "Datos")
sum(de60_69.diarios$Datos)
de60_69.diarios = de60_69.diarios %>% filter(Fechas>="2021-02-01")

Fecha = c()
for (i in seq(1,length(de60_69.diarios$Fechas), by=2)){
  c = (de60_69.diarios$Fechas[i])
  Fecha = append(Fecha, c)
}
Fecha = data.frame(Fecha)

de60_69.sem = c()
for (i in seq(1,length(de60_69.diarios$Datos), by=2)){
  c = (de60_69.diarios$Datos[i]+de60_69.diarios$Datos[i+1])
  de60_69.sem = append(de60_69.sem, c)
}
de60_69.sem = data.frame(de60_69.sem)
de60_69.sem = na.omit(de60_69.sem)

vacunas1_semanal = vac_1dosis %>% filter(Edad>="60", Edad<="69", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.1d = sum(`Primera Dosis`))
sum(vacunas1_semanal$vac.1d)
v = c()
c = 0
for (i in 1:length(vacunas1_semanal$vac.1d)) {
  vacunas1_semanal$vac.1d[i] + c
  c = vacunas1_semanal$vac.1d[i] + c
  v = append(v,c)}
a = cbind(vacunas1_semanal[,1], v)

vacunas.1dosis.sem = c()
for (i in seq(1,length(vacunas1_semanal$vac.1d),by=7)){
  c = (a$v[i+2])
  vacunas.1dosis.sem = append(vacunas.1dosis.sem,c)
} 
vacunas.1dosis.sem = data.frame(vacunas.1dosis.sem)

vacunas2_semanal = vac_2dosis %>% filter(Edad>="60", Edad<="69", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.2d = sum(`Segunda Dosis`))
v = c()
c = 0
for (i in 1:length(vacunas2_semanal$vac.2d)) {
  vacunas2_semanal$vac.2d[i] + c
  c = vacunas2_semanal$vac.2d[i] + c
  v = append(v,c)
}
b = cbind(vacunas2_semanal[,1], v)
vacunas.2dosis.sem = c()
for (i in seq(1,length(vacunas2_semanal$vac.2d),by=7)){
  c = (b$v[i+2])
  vacunas.2dosis.sem = append(vacunas.2dosis.sem,c)
} 
vacunas.2dosis.sem = data.frame(vacunas.2dosis.sem)

vacunados.total.sem = vac.total %>% filter(Edad>="60", Edad<="69", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vacunados = sum(Vacunas))
v = c()
c = 0
for (i in 1:length(vacunados.total.sem$vacunados)) {
  vacunados.total.sem$vacunados[i] + c
  c = vacunados.total.sem$vacunados[i] + c
  v = append(v,c)
}
d = cbind(vacunados.total.sem[,1], v)
vac.60_69.sem = c()
for (i in seq(1,length(vacunados.total.sem$vacunados),by=7)){
  c = (d$v[i+2])
  vac.60_69.sem = append(vac.60_69.sem,c)
} 
vac.60_69.sem = data.frame(vac.60_69.sem)


uci.60_69.semanal = UCI_edad %>% filter(Fecha>="2021-01-30", Edades=="60-69") %>% 
  group_by(Fecha) %>% summarise(camas = sum(Datos))
uci60_69.sem = c()
for (i in seq(1,length(uci.60_69.semanal$camas),by=7)){
  c = ((uci.60_69.semanal$camas[i]+uci.60_69.semanal$camas[i+1]+uci.60_69.semanal$camas[i+2]+
          uci.60_69.semanal$camas[i+3]+uci.60_69.semanal$camas[i+4]+uci.60_69.semanal$camas[i+5]+
          uci.60_69.semanal$camas[i+6])/7)
  uci60_69.sem = append(uci60_69.sem,c)
} 
uci60_69.sem = data.frame(uci60_69.sem)

de60_69.sem = cbind(Fecha, de60_69.sem[1:length(Fecha$Fecha),], uci60_69.sem[1:length(Fecha$Fecha),],
                    vac.60_69.sem[1:length(Fecha$Fecha),], vacunas.1dosis.sem[1:length(Fecha$Fecha),],
                    vacunas.2dosis.sem[1:length(Fecha$Fecha),])
colnames(de60_69.sem) = c("Fecha", "Casos", "Camas", "Vac.totales", "Vac.1d", "Vac.2d")
de60_69.sem = na.omit(de60_69.sem)
de60_69.sem = de60_69.sem %>% filter(Fecha<="2021-09-30")


de60_69.sem$Vac.2d[12]/poblacion.60_69
de60_69.sem$Vac.1d[length(de60_69.sem$Vac.1d)]/poblacion.60_69
de60_69.sem$Vac.2d[length(de60_69.sem$Vac.2d)]/poblacion.60_69

ggplot(de60_69.sem, aes(Fecha, Camas))+
  geom_point()+
  geom_smooth()+
  theme_bw()

#Models
mod.60_69.1 = gamlss(formula = Camas~Casos,
                     family=PIG, trace=F,data = de60_69.sem)
mod.60_69.2 = gamlss(formula = Camas~Vac.totales,
                     family=PIG, trace=F,data = de60_69.sem)
mod.60_69.3 = gamlss(formula = Camas~Casos+Vac.totales,
                     family=PIG, trace=F,data = de60_69.sem)
mod.60_69.4 = gamlss(formula = Camas~Vac.1d+Vac.2d,
                     family=PIG, trace=F,data = de60_69.sem)
mod.60_69.5 = gamlss(formula = Camas~Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                     family=PIG, trace=F,data = de60_69.sem)
mod.60_69.6 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d,
                     family=PIG, trace=F,data = de60_69.sem)
mod.60_69.7 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                     family=PIG, trace=F,data = de60_69.sem)

#Evaluation
GAIC(mod.60_69.1, mod.60_69.2, mod.60_69.3, mod.60_69.4, mod.60_69.5, mod.60_69.6, mod.60_69.7)
VC.test(mod.60_69.1, mod.60_69.7)
r1_60_69 = Rsq(mod.60_69.1, type = c("Cox Snell","Cragg Uhler","both")); r1_60_69
r2_60_69 = Rsq(mod.60_69.3, type = c("Cox Snell","Cragg Uhler","both")); r2_60_69
summary(mod.60_69.1)
plot(mod.60_69.1)
wp(mod.60_69.1, ylim.all = 0.8)
drop1(mod.60_69.1, parallel = "multicore", ncpus = 4)
VC.test(mod.60_69.1, mod.60_69.7) #Hay diferencias entre modelos
term.plot(mod.60_69.1, pages = 1, ask = FALSE, rug = TRUE)
de60_69.sem = de60_69.sem %>% mutate(pred.60_69 = fitted(mod.60_69.1))
de60_69.sem = de60_69.sem %>% mutate(pred.60_69.def = fitted(mod.60_69.7))


#Graphics
d1 = ggplot(de60_69.sem, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred.60_69), se=TRUE, method = "gam", color="#41b6c4")+
  labs(x = "Dates",
       y = "ICU beds occupied weekly",
       tag = "D")+
  annotate(geom="text",x=as.Date("2021-08-02"),
           y=900,label=paste("R\u00b2 = ", round(r1_60_69,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 1000))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
d1

d2 = ggplot(de60_69.sem, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred.60_69.def), se=TRUE, method = "gam", color="#41b6c4")+
  labs(x = "Dates",
       y = "ICU beds occupied weekly")+
  annotate(geom="text",x=as.Date("2021-08-02"),
           y=900,label=paste("R\u00b2 = ", round(r2_60_69, 4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 1000))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
d2

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#WEEKLY CASES OVER 70 years old
poblacion.ma70 = 1544008

ma70sem = casos_por_edad %>% filter(Grupo_edad<="70 - 74 años") %>% group_by(Fecha) %>% 
  summarise(Casos = sum(Datos))

ma70_diarios = c()
for (i in 1:length(ma70sem$Fecha)){
  c = ma70sem$Casos[i] - ma70sem$Casos[i-1]
  ma70_diarios = append(ma70_diarios,c)
} 
ma70_diarios[is.na(ma70_diarios)] = 0
ma70_diarios = as.numeric(ma70_diarios)
ma70_diarios = data.frame(ma70_diarios)
Rango_etario = c(rep(">=70", length(ma70_diarios$ma70_diarios)))
Rango_etario = data.frame(Rango_etario)
ma70_diarios = cbind(Rango_etario, ma70sem$Fecha[2:length(ma70sem$Fecha)], ma70_diarios)
colnames(ma70_diarios) = c("Rango_etario","Fechas", "Datos")
sum(ma70_diarios$Datos)
ma70_diarios = ma70_diarios %>% filter(Fechas>="2021-02-01")

Fecha = c()
for (i in seq(1,length(ma70_diarios$Fechas), by=2)){
  c = (ma70_diarios$Fechas[i])
  Fecha = append(Fecha, c)
}
Fecha = data.frame(Fecha)

ma70_sem = c()
for (i in seq(1,length(ma70_diarios$Datos), by=2)){
  c = (ma70_diarios$Datos[i]+ma70_diarios$Datos[i+1])
  ma70_sem = append(ma70_sem, c)
}
ma70_sem = data.frame(ma70_sem)
ma70_sem = na.omit(ma70_sem)

vacunas1_semanal = vac_1dosis %>% filter(Edad>="70", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.1d = sum(`Primera Dosis`))
sum(vacunas1_semanal$vac.1d)
v = c()
c = 0
for (i in 1:length(vacunas1_semanal$vac.1d)) {
  vacunas1_semanal$vac.1d[i] + c
  c = vacunas1_semanal$vac.1d[i] + c
  v = append(v,c)}
a = cbind(vacunas1_semanal[,1], v)

vacunas.1dosis.sem = c()
for (i in seq(1,length(vacunas1_semanal$vac.1d),by=7)){
  c = (a$v[i+2])
  vacunas.1dosis.sem = append(vacunas.1dosis.sem,c)
} 
vacunas.1dosis.sem = data.frame(vacunas.1dosis.sem)

vacunas2_semanal = vac_2dosis %>% filter(Edad>="70", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vac.2d = sum(`Segunda Dosis`))
v = c()
c = 0
for (i in 1:length(vacunas2_semanal$vac.2d)) {
  vacunas2_semanal$vac.2d[i] + c
  c = vacunas2_semanal$vac.2d[i] + c
  v = append(v,c)
}
b = cbind(vacunas2_semanal[,1], v)
vacunas.2dosis.sem = c()
for (i in seq(1,length(vacunas2_semanal$vac.2d),by=7)){
  c = (b$v[i+2])
  vacunas.2dosis.sem = append(vacunas.2dosis.sem,c)
} 
vacunas.2dosis.sem = data.frame(vacunas.2dosis.sem)

vacunados.total.sem = vac.total %>% filter(Edad>="70", Fecha>="2021-01-30") %>% 
  group_by(Fecha) %>% summarise(vacunados = sum(Vacunas))
v = c()
c = 0
for (i in 1:length(vacunados.total.sem$vacunados)) {
  vacunados.total.sem$vacunados[i] + c
  c = vacunados.total.sem$vacunados[i] + c
  v = append(v,c)
}
d = cbind(vacunados.total.sem[,1], v)
vac.ma70.sem = c()
for (i in seq(1,length(vacunados.total.sem$vacunados),by=7)){
  c = (d$v[i+2])
  vac.ma70.sem = append(vac.ma70.sem,c)
} 
vac.ma70.sem = data.frame(vac.ma70.sem)

uci_ma70_semanal = UCI_edad %>% filter(Fecha>="2021-01-30", Edades==">=70") %>% 
  group_by(Fecha) %>% summarise(camas.ma70 = sum(Datos))
length(uci_ma70_semanal$Fecha)
uci_ma70.sem = c()
for (i in seq(1,length(uci_ma70_semanal$Fecha),by=7)){
  c = ((uci_ma70_semanal$camas.ma70[i]+uci_ma70_semanal$camas.ma70[i+1]+uci_ma70_semanal$camas.ma70[i+2]+
          uci_ma70_semanal$camas.ma70[i+3]+uci_ma70_semanal$camas.ma70[i+4]+uci_ma70_semanal$camas.ma70[i+5]+
          uci_ma70_semanal$camas.ma70[i+6])/7)
  uci_ma70.sem = append(uci_ma70.sem,c)
} 
uci_ma70.sem = data.frame(uci_ma70.sem)

ma70.sem = cbind(Fecha, ma70_sem[1:length(Fecha$Fecha),], uci_ma70.sem[1:length(Fecha$Fecha),], 
                 vac.ma70.sem[1:length(Fecha$Fecha),], vacunas.1dosis.sem[1:length(Fecha$Fecha),],
                 vacunas.2dosis.sem[1:length(Fecha$Fecha),])
colnames(ma70.sem) = c("Fecha", "Casos", "Camas", "Vac.totales", "Vac.1d", "Vac.2d")
ma70.sem = na.omit(ma70.sem)
ma70.sem = ma70.sem %>% filter(Fecha<="2021-09-30")


ma70.sem$Vac.2d[7]/poblacion.ma70
ma70.sem$Vac.1d[length(ma70.sem$Vac.1d)]/poblacion.ma70
ma70.sem$Vac.2d[length(ma70.sem$Vac.2d)]/poblacion.ma70

ggplot(ma70.sem, aes(Fecha, Camas))+
  geom_point()+
  geom_smooth()


#Models
mod.ma70.1 = gamlss(formula = Camas~Casos,
                     family=PIG, trace=F,data = ma70.sem)
mod.ma70.2 = gamlss(formula = Camas~Vac.totales,
                     family=PIG, trace=F,data = ma70.sem)
mod.ma70.3 = gamlss(formula = Camas~Casos+Vac.totales,
                     family=PIG, trace=F,data = ma70.sem)
mod.ma70.4 = gamlss(formula = Camas~Vac.1d+Vac.2d,
                     family=PIG, trace=F,data = ma70.sem)
mod.ma70.5 = gamlss(formula = Camas~Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                     family=PIG, trace=F,data = ma70.sem)
mod.ma70.6 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d,
                     family=PIG, trace=F,data = ma70.sem)
mod.ma70.7 = gamlss(formula = Camas~Casos+Vac.1d+Vac.2d+Vac.1d:Vac.2d,
                     family=PIG, trace=F,data = ma70.sem)

#Evaluations
GAIC(mod.ma70.1, mod.ma70.2, mod.ma70.3, mod.ma70.4, mod.ma70.5, mod.ma70.6, mod.ma70.7)
VC.test(mod.ma70.7, mod.ma70.3)
r1_ma70 = Rsq(mod.ma70.3, type = c("Cox Snell","Cragg Uhler","both")); r1_ma70
r2_ma70 = Rsq(mod.ma70.7, type = c("Cox Snell","Cragg Uhler","both")); r2_ma70
summary(mod.ma70.3)
plot(mod.ma70.3)
wp(mod.ma70.3, ylim.all = 0.8)
drop1(mod.ma70.3, parallel = "multicore", ncpus = 4)
VC.test(mod.ma70.3, mod.ma70.1) #NO hay diferencias entre modelos
term.plot(mod.ma70.3, pages = 1, ask = FALSE, rug = TRUE)
ma70.sem1 = ma70.sem %>% mutate(pred_ma70 = fitted(mod.ma70.3))
ma70.sem1 = ma70.sem1 %>% mutate(pred_ma70.def = fitted(mod.ma70.7))


#Graphics
e1 = ggplot(ma70.sem1, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred_ma70), se=TRUE, method = "gam",color="#41b6c4")+
  labs(x = "Dates",
       y = "ICU beds occupied weekly",
       tag = "E")+
  annotate(geom="text",x=as.Date("2021-04-02"),
           y=200,label=paste("R\u00b2 = ", round(r1_ma70,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 600))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
e1

e2 = ggplot(ma70.sem1, aes(Fecha, Camas))+
  geom_point(color="#225ea8", size=0.5)+
  geom_smooth(mapping = aes(Fecha, pred_ma70.def), se=TRUE, method = "gam",color="#41b6c4")+
  labs(x = "Dates",
       y = "ICU beds occupied weekly")+
  annotate(geom="text",x=as.Date("2021-04-02"),
           y=200,label=paste("R\u00b2 = ", round(r2_ma70,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 600))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 10))
e2


#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

f = ggplot() + theme_void()

grid.arrange(arrangeGrob(a1,f,a2,f,b1,f,b2, ncol = 7, widths = c(4,0.3,4,1,4,0.3,4)),
             arrangeGrob(f, ncol = 1, widths = c(18)),
             arrangeGrob(c1,f,c2,f,d1,f,d2, ncol=7,widths = c(4,0.5,4,1,4,0.5,4)),
             arrangeGrob(f, ncol = 1, widths = c(18)),
             arrangeGrob(e1,f,e2,f, ncol=4,widths = c(4,0.5,4,9.5)),
             heights=c(2.5, 0.3, 2.5, 0.3, 2.5), ncol=1)
