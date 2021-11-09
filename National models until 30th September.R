#Packages
library(readr)
library(ggplot2)
library(dplyr)
library(gtable)
library(gridExtra)
library(cowplot)
library(gamlss)
library(zoo)

#-------------------------------------------------------------------------------#

#Data Base
pacientes_UCI_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto9/HospitalizadosUCIEtario_std.csv"
UCI_edad = read_csv(url(pacientes_UCI_edad))
colnames(UCI_edad) = c("Edades", "Fecha", "Datos")

casos_rango_etario = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto16/CasosGeneroEtario_std.csv"
casos_por_edad = read_csv(url(casos_rango_etario))
colnames(casos_por_edad) = c("Grupo_edad", "Sexo", "Fecha", "Datos")

vacunas_por_region = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto76/vacunacion_std.csv"
vacunacion_regional = read_csv(url(vacunas_por_region))

casos_por_region = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/TotalesPorRegion_std.csv"
casos_regionales = read_csv(url(casos_por_region))

vacunados_1dosis_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_1eraDosis_std.csv"
vac_1dosis = read_csv(url(vacunados_1dosis_edad))
vac_1dosis[is.na(vac_1dosis)] = 0

vacunados_2dosis_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_2daDosis_std.csv"
vac_2dosis = read_csv(url(vacunados_2dosis_edad))
vac_2dosis[is.na(vac_2dosis)] = 0

fallecidos_por_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto10/FallecidosEtario_std.csv"
fallecidos_etario = read_csv(url(fallecidos_por_edad))

casos_nuevos_pais = casos_regionales %>% filter(Region=="Total",Categoria=="Casos nuevos totales",
                                                Fecha>="2021-02-03", Fecha<="2021-09-30")

casos_activos = casos_regionales %>% filter(Region=="Total",Categoria=="Casos activos confirmados",
                                            Fecha>="2021-02-03", Fecha<="2021-09-30")

uci_total = UCI_edad %>% filter(Fecha>="2021-02-03", Fecha<="2021-09-30") %>% 
  group_by(Fecha) %>% 
  summarise(camas = sum(Datos))

vacunas_totales = vacunacion_regional %>% filter(Region=="Total", Fecha>="2021-02-03", Fecha<="2021-09-30") %>% 
  group_by(Fecha) %>% summarise(datos = sum(Cantidad))

vacunas.acumuladas1 = vacunacion_regional %>% filter(Dosis=="Primera", Region=="Total",
                                                     Fecha>="2021-02-03", Fecha<="2021-09-30")

vacunas.acumuladas2 = vacunacion_regional %>% filter(Dosis=="Segunda", Region=="Total", 
                                                     Fecha>="2021-02-03", Fecha<="2021-09-30")

fallecidos.total = fallecidos_etario %>% group_by(fecha) %>% summarise(muertos = sum(`Casos confirmados`))

F_diarios = c()
for (i in 1:length(fallecidos.total$fecha)){
  f = fallecidos.total$muertos[i] - fallecidos.total$muertos[i-1]
  F_diarios = append(F_diarios,f)
} 
F_diarios[is.na(F_diarios)] = 0
F_diarios = as.numeric(F_diarios)
F_diarios = data.frame(F_diarios)

f = cbind(fallecidos.total$fecha[2:length(fallecidos.total$fecha)], F_diarios)
colnames(f) = c("Fechas", "Fallecidos")

f = f %>% mutate(srate_ma01 = rollmean(Fallecidos, k = 7, fill = NA))

fallecidos_diarios = f %>% filter(Fechas>="2021-02-03", Fechas<="2021-09-30")
fallecidos_diarios = na.omit(fallecidos_diarios)

limite = length(fallecidos_diarios$srate_ma01)

df_nacional = cbind(casos_nuevos_pais[1:limite,3:4], casos_activos[1:limite,4], 
                    uci_total[1:limite,2], vacunas_totales[1:limite,2], 
                    vacunas.acumuladas1[1:limite,4], vacunas.acumuladas2[1:limite,4], 
                    fallecidos_diarios[1:limite,3])
colnames(df_nacional) = c("Fechas", "casos.nuevos", "casos.activos","camas.uci", "vacunas.totales", 
                          "primera.dosis", "segunda.dosis", "fallecidos")
f = ggplot() + theme_void()

var(df_nacional$casos.nuevos)
mean(df_nacional$casos.nuevos)


#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#Model of new cases

modelo.nacional.1 = gamlss(formula = casos.nuevos~vacunas.totales,
                           family=PIG, trace=F,data = df_nacional)
modelo.nacional.2 = gamlss(formula = casos.nuevos~primera.dosis,
                           family=PIG, trace=F,data = df_nacional)
modelo.nacional.3 = gamlss(formula = casos.nuevos~segunda.dosis,
                           family=PIG, trace=F,data = df_nacional)
modelo.nacional.4 = gamlss(formula = casos.nuevos~vacunas.totales+primera.dosis:segunda.dosis,
                           family=PIG, trace=F,data = df_nacional)
modelo.nacional.5 = gamlss(formula = casos.nuevos~primera.dosis+segunda.dosis,
                           family=PIG, trace=F,data = df_nacional)
modelo.nacional.6 = gamlss(formula = casos.nuevos~primera.dosis+segunda.dosis+primera.dosis:segunda.dosis,
                           family=PIG, trace=F,data = df_nacional)

#Evaluatons
GAIC(modelo.nacional.1, modelo.nacional.2, modelo.nacional.3, modelo.nacional.4,
     modelo.nacional.5, modelo.nacional.6)
VC.test(modelo.nacional.6, modelo.nacional.5) #Hay diferencias entre ambos modelos
summary(modelo.nacional.6)
plot(modelo.nacional.6)
wp(modelo.nacional.6, ylim.all = 1)
wp(modelo.nacional.5, ylim.all = 1)
r1_nuevos = Rsq(modelo.nacional.6, type = c("Cox Snell","Cragg Uhler","both")); r1_nuevos
r2_nuevos = Rsq(modelo.nacional.5, type = c("Cox Snell","Cragg Uhler","both")); r2_nuevos
drop1(modelo.nacional.6, parallel = "multicore", ncpus = 4)
term.plot(modelo.nacional.6, pages = 1, ask = FALSE, rug = TRUE)
casos_nacional = df_nacional %>% mutate(pred.nuevos.nacional = fitted(modelo.nacional.6))
casos_nacional = casos_nacional %>% mutate(pred.def.nuevos.nacional = fitted(modelo.nacional.5))

#Graphics
w = ggplot(casos_nacional, aes(Fechas, casos.nuevos))+
  geom_point(col="#feb24c")+
  geom_smooth(mapping = aes(Fechas, pred.nuevos.nacional), se=TRUE, method = "gam", col="#fc4e2a")+
  labs(title = "Explanatory model of new cases",
       x="Date",
       y="Number of new cases daily",
       tag = "A")+
  annotate(geom="text",x=as.Date("2021-02-15"),
           y=7900,label=paste("R\u00b2 = ", round(r1_nuevos,4)),fontface="bold", size=5)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
w

w1 = ggplot(casos_nacional, aes(Fechas, casos.nuevos))+
  geom_point(col="#feb24c")+
  geom_smooth(mapping = aes(Fechas, pred.def.nuevos.nacional), se=TRUE, method = "gam", col="#fc4e2a")+
  labs(title = "Explanatory model of new cases \nwithout interaction between doses",
       x="Date",
       y="Number of new cases daily",
       tag = "A")+
  annotate(geom="text",x=as.Date("2021-08-01"),
           y=10000,label=paste("R\u00b2 = ", round(r2_nuevos,4)),fontface="bold", size=5)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
w1


w2 = ggplot(casos_nacional, aes(Fechas, casos.nuevos))+
  geom_point(col="#feb24c")+
  geom_smooth(mapping = aes(Fechas, pred.nuevos.nacional), se=TRUE, method = "gam", col="#fc4e2a")+
  geom_smooth(mapping = aes(Fechas, pred.def.nuevos.nacional), se=TRUE, method = "gam", col="#fc4e2a")+
  labs(title = "Explanatory model of new cases",
       x="Dates",
       y="Number of new cases daily",
       tag = "A")+
  annotate(geom="text",x=as.Date("2021-07-01"),
           y=11000,label=paste("R\u00b2 interaction = ", round(r1_nuevos,4)),fontface="bold", size=3)+
  annotate(geom="segment",x=as.Date("2021-08-02"), xend=as.Date("2021-08-10"),
           y=11000, yend = 11000, size=5)+
  annotate(geom="text",x=as.Date("2021-07-01"),
           y=9000,label=paste("R\u00b2 without interaction = ", round(r2_nuevos,4)),fontface="bold", size=3)+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
w2

w3 = ggplot(casos_nacional, aes(Fechas, casos.nuevos))+
  geom_point(col="#feb24c", size=0.5)+
  geom_smooth(mapping = aes(Fechas, pred.nuevos.nacional), se=TRUE, method = "gam",
              col="#fc4e2a", size=0.5, alpha=0.4)+
  labs(x="Date",
       y="Number of new cases daily",
       tag = "A")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=10000,label=paste("R\u00b2 = ", round(r1_nuevos,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 12500))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1))
w3

w4 = ggplot(casos_nacional, aes(Fechas, casos.nuevos))+
  geom_point(col="#feb24c", size=0.5)+
  geom_smooth(mapping = aes(Fechas, pred.def.nuevos.nacional), se=TRUE, method = "gam",
              col="#fc4e2a", size=0.5, alpha=0.4)+
  labs(x="Date",
       y="Number of new cases daily")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=10000,label=paste("R\u00b2 = ", round(r2_nuevos,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 12500))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1))
w4

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#Model of actives cases

modelo.nacional.0.2 = gamlss(formula = casos.activos~casos.nuevos,
                             family=PIG, trace=F,data = df_nacional)
modelo.nacional.1.2 = gamlss(formula = casos.activos~vacunas.totales,
                             family=PIG, trace=F,data = df_nacional)
modelo.nacional.2.2 = gamlss(formula = casos.activos~vacunas.totales+casos.nuevos,
                             family=PIG, trace=F,data = df_nacional)
modelo.nacional.3.2 = gamlss(formula = casos.activos~casos.nuevos+primera.dosis+segunda.dosis,
                             family=PIG, trace=F,data = df_nacional)
modelo.nacional.4.2 = gamlss(formula = casos.activos~casos.nuevos+primera.dosis+segunda.dosis+primera.dosis:segunda.dosis,
                             family=PIG, trace=F,data = df_nacional)
modelo.nacional.5.2 = gamlss(formula = casos.activos~primera.dosis+segunda.dosis,
                             family=PIG, trace=F,data = df_nacional)
modelo.nacional.6.2 = gamlss(formula = casos.activos~primera.dosis+segunda.dosis+primera.dosis:segunda.dosis,
                             family=PIG, trace=F,data = df_nacional)

#Evaluations
GAIC(modelo.nacional.0.2, modelo.nacional.1.2, modelo.nacional.2.2, modelo.nacional.3.2, 
     modelo.nacional.4.2, modelo.nacional.5.2, modelo.nacional.6.2)
VC.test(modelo.nacional.4.2, modelo.nacional.6.2)
summary(modelo.nacional.4.2)
plot(modelo.nacional.4.2)
wp(modelo.nacional.4.2, ylim.all = 1)
r1_activos = Rsq(modelo.nacional.4.2, type = c("Cox Snell","Cragg Uhler","both"));r1_activos
r2_activos = Rsq(modelo.nacional.3.2, type = c("Cox Snell","Cragg Uhler","both"));r2_activos
drop1(modelo.nacional.4.2, parallel = "multicore", ncpus = 4)
VC.test(modelo.nacional.4.2, modelo.nacional.3.2, sig.lev = 0.05) #Hay diferencias entre ambos modelos
term.plot(modelo.nacional.5.2, pages = 1, ask = FALSE, rug = TRUE)
casos.activos = df_nacional %>% mutate(pred.activos.nacional = fitted(modelo.nacional.4.2))
casos.activos = casos.activos %>% mutate(pred.def.activos.nacional = fitted(modelo.nacional.3.2))


#Graphics
x = ggplot(casos.activos, aes(Fechas, casos.activos))+
  geom_point(col="#08519c")+
  geom_smooth(mapping = aes(Fechas, pred.activos.nacional), se=TRUE, method = "gam", col="#9ecae1", alpha=0.4)+
  labs(title = "Explanatory model of active cases",
       x="Date",
       y="Daily active cases",
       tag = "B")+
  annotate(geom="text",x=as.Date("2021-02-15"),
           y=55000,label=paste("R\u00b2 = ", round(r1_activos,4)),fontface="bold", size=5)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
x

x1 = ggplot(casos.activos, aes(Fechas, casos.activos))+
  geom_point(col="#08519c")+
  geom_smooth(mapping = aes(Fechas, pred.def.activos.nacional), se=TRUE, method = "gam", col="#9ecae1", alpha=0.4)+
  labs(title = "Explanatory model of active cases \nwithout interaction between doses",
       x="Date",
       y="Daily active cases",
       tag = "B")+
  annotate(geom="text",x=as.Date("2021-02-15"),
           y=55000,label=paste("R\u00b2 =", round(r2_activos, 4)),fontface="bold",size=5)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
x1


x3 = ggplot(casos.activos, aes(Fechas, casos.activos))+
  geom_point(col="#08519c", size=0.5)+
  geom_smooth(mapping = aes(Fechas, pred.activos.nacional), se=TRUE, method = "gam",
              col="#9ecae1", alpha=0.4)+
  labs(x="Date",
       y="Daily active cases",
       tag = "B")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=55000,label=paste("R\u00b2 = ", round(r1_activos,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 60000))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1))
x3

x4 = ggplot(casos.activos, aes(Fechas, casos.activos))+
  geom_point(col="#08519c", size=0.5)+
  geom_smooth(mapping = aes(Fechas, pred.def.activos.nacional), se=TRUE, method = "gam",
              col="#9ecae1", alpha=0.4)+
  labs(x="Date",
       y="Daily active cases")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=55000,label=paste("R\u00b2 =", round(r2_activos, 4)),fontface="bold",size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 60000))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1))
x4

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#Model of occupied ICU beds

modelo.uci.01 = gamlss(formula = camas.uci~casos.nuevos,
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.02 = gamlss(formula = camas.uci~casos.activos,
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.03 = gamlss(formula = camas.uci~vacunas.totales,
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.04 = gamlss(formula = camas.uci~casos.nuevos+vacunas.totales,
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.05 = gamlss(formula = camas.uci~casos.activos+vacunas.totales,
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.06 = gamlss(formula = camas.uci~primera.dosis+segunda.dosis,
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.07 = gamlss(formula = camas.uci~primera.dosis+segunda.dosis+primera.dosis:segunda.dosis, 
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.08 = gamlss(formula = camas.uci~casos.nuevos+primera.dosis+segunda.dosis, 
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.09 = gamlss(formula = camas.uci~casos.activos+primera.dosis+segunda.dosis, 
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.10 = gamlss(formula = camas.uci~casos.nuevos+primera.dosis+segunda.dosis+primera.dosis:segunda.dosis, 
                       family=PIG, trace=F,data = df_nacional)
modelo.uci.11 = gamlss(formula = camas.uci~casos.activos+primera.dosis+segunda.dosis+primera.dosis:segunda.dosis, 
                       family=PIG, trace=F,data = df_nacional)

#Evaluations
GAIC(modelo.uci.01, modelo.uci.02, modelo.uci.03, modelo.uci.04, modelo.uci.05, modelo.uci.06,
     modelo.uci.07, modelo.uci.08, modelo.uci.09, modelo.uci.10, modelo.uci.11)
VC.test(modelo.uci.11, modelo.uci.07)
r1_uci = Rsq(modelo.uci.11, type = c("Cox Snell","Cragg Uhler","both")); r1_uci
r2_uci = Rsq(modelo.uci.09, type = c("Cox Snell","Cragg Uhler","both")); r2_uci
summary(modelo.uci.11)
plot(modelo.uci.11)
wp(modelo.uci.11, ylim.all = 0.8)
drop1(modelo.uci.11, parallel = "multicore", ncpus = 4)
VC.test(modelo.uci.11, modelo.uci.09) # Hay diferencias entre ambos modelos
term.plot(modelo.uci.11, pages = 1, ask = FALSE, rug = TRUE)
uci.nacional = df_nacional %>% mutate(pred.uci = fitted(modelo.uci.11))
uci.nacional = uci.nacional %>% mutate(pred.uci.mala = fitted(modelo.uci.09))

#Graphics
y = ggplot(uci.nacional, aes(Fechas, camas.uci))+
  geom_point(alpha=0.7, size=2, color="#807dba")+
  geom_smooth(mapping = aes(Fechas, pred.uci), se=TRUE, method = "gam", 
              size=0.5, alpha=0.8, color="#54278f", fill="#bcbddc")+
  labs(title = "Explanatory model of occupied ICU beds",
       x = "Date",
       y = "Occupied ICU beds",
       tag = "C")+
  annotate(geom="text",x=as.Date("2021-02-15"),
           y=3200,label=paste("R\u00b2 = ", round(r1_uci,4)),fontface="bold", size=5)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
y


y1 = ggplot(uci.nacional, aes(Fechas, camas.uci))+
  geom_point(alpha=0.7, size=2, color="#807dba")+
  geom_smooth(mapping = aes(Fechas, pred.uci.mala), se=TRUE, method = "gam", 
              size=0.5, alpha=0.8, color="#54278f", fill="#bcbddc")+
  labs(title = "Explanatory model of occupied ICU beds \nwithout interaction between doses",
       x = "Date",
       y = "Occupied ICU beds",
       tag = "C")+
  annotate(geom="text",x=as.Date("2021-02-15"),
           y=3500,label=paste("R\u00b2 = ", round(r2_uci, 4)),fontface="bold", size=5)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
y1


y3 = ggplot(uci.nacional, aes(Fechas, camas.uci))+
  geom_point(color="#807dba", size=0.5)+
  geom_smooth(mapping = aes(Fechas, pred.uci), se=TRUE, method = "gam", 
              size=0.5, alpha=0.4, color="#54278f")+
  labs(x = "Date",
       y = "Occupied ICU beds",
       tag = "C")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=3600,label=paste("R\u00b2 = ", round(r1_uci,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 4000))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1))
y3

y4 = ggplot(uci.nacional, aes(Fechas, camas.uci))+
  geom_point(color="#807dba", size=0.5)+
  geom_smooth(mapping = aes(Fechas, pred.uci.mala), se=TRUE, method = "gam", 
              size=0.5, alpha=0.4, color="#54278f")+
  labs(x = "Date",
       y = "Occupied ICU beds")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=3600,label=paste("R\u00b2 = ", round(r2_uci, 4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 4000))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1))
y4


#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

#Model of weekly moving average of deaths

mod.fallecidos.1 = gamlss(formula = fallecidos~casos.nuevos,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.2 = gamlss(formula = fallecidos~casos.activos,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.3 = gamlss(formula = fallecidos~camas.uci,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.4 = gamlss(formula = fallecidos~casos.nuevos+camas.uci,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.5 = gamlss(formula = fallecidos~casos.activos+camas.uci,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.6 = gamlss(formula = fallecidos~casos.nuevos+camas.uci+vacunas.totales,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.7 = gamlss(formula = fallecidos~casos.activos+camas.uci+vacunas.totales,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.8 = gamlss(formula = fallecidos~casos.nuevos+camas.uci+primera.dosis+segunda.dosis,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.9 = gamlss(formula = fallecidos~casos.activos+camas.uci+primera.dosis+segunda.dosis,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.10 = gamlss(formula = fallecidos~primera.dosis+segunda.dosis,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.11 = gamlss(formula = fallecidos~casos.nuevos+camas.uci+primera.dosis+segunda.dosis+primera.dosis:segunda.dosis,
                          family=IG, trace=F,data = df_nacional)
mod.fallecidos.12 = gamlss(formula = fallecidos~casos.activos+camas.uci+primera.dosis+segunda.dosis+primera.dosis:segunda.dosis,
                           family=IG, trace=F,data = df_nacional)
mod.fallecidos.13 = gamlss(formula = fallecidos~primera.dosis+segunda.dosis+primera.dosis:segunda.dosis,
                           family=IG, trace=F,data = df_nacional)
mod.fallecidos.14 = gamlss(formula = fallecidos~camas.uci+casos.activos+casos.nuevos,
                           family=IG, trace=F,data = df_nacional)
mod.fallecidos.15 = gamlss(formula = fallecidos~camas.uci+casos.activos+casos.nuevos+vacunas.totales,
                           family=IG, trace=F,data = df_nacional)
mod.fallecidos.16 = gamlss(formula = fallecidos~camas.uci+casos.activos+casos.nuevos+primera.dosis+segunda.dosis,
                           family=IG, trace=F,data = df_nacional)
mod.fallecidos.17 = gamlss(formula = fallecidos~camas.uci+casos.activos+casos.nuevos+primera.dosis+segunda.dosis+primera.dosis:segunda.dosis,
                           family=IG, trace=F,data = df_nacional)

#Evaluations
GAIC(mod.fallecidos.1, mod.fallecidos.2, mod.fallecidos.3, mod.fallecidos.4, mod.fallecidos.5,
     mod.fallecidos.6, mod.fallecidos.7, mod.fallecidos.8, mod.fallecidos.9, mod.fallecidos.10,
     mod.fallecidos.11, mod.fallecidos.12, mod.fallecidos.13, mod.fallecidos.14, mod.fallecidos.15,
     mod.fallecidos.16, mod.fallecidos.17)
VC.test(mod.fallecidos.12, mod.fallecidos.17)
r1_fallecidos = Rsq(mod.fallecidos.12, type = c("Cox Snell","Cragg Uhler","both"));r1_fallecidos
r2_fallecidos = Rsq(mod.fallecidos.9, type = c("Cox Snell","Cragg Uhler","both"));r2_fallecidos
summary(mod.fallecidos.12)
plot(mod.fallecidos.12)
wp(mod.fallecidos.12, ylim.all = 0.8)
drop1(mod.fallecidos.12, parallel = "multicore", ncpus = 4)
#La baja en la cantidad de casos esta fuertemente ligada a las camas UCI
VC.test(mod.fallecidos.12, mod.fallecidos.9) #Hay diferencias entre ambos modelos
term.plot(mod.fallecidos.12, pages = 1, ask = FALSE, rug = TRUE)
fallecidos.nacional = df_nacional %>% mutate(pred.fallecidos = fitted(mod.fallecidos.12))
fallecidos.nacional = fallecidos.nacional %>% mutate(pred.fallecidos.mala = fitted(mod.fallecidos.9))


#Graphics
z = ggplot(fallecidos.nacional, aes(Fechas, fallecidos))+
  geom_point(color="#78c679")+
  #geom_line(aes(y=pred.fallecidos))+
  geom_smooth(mapping = aes(Fechas, pred.fallecidos), se=TRUE, method = "gam", color="#004529")+
  labs(title = "Explanatory model of weekly moving average of deaths",
       x ="Date",
       y= "Weekly moving average of deaths",
       tag = "D")+
  annotate(geom="text",x=as.Date("2021-02-15"),
           y=200,label=paste("R\u00b2 =", round(r1_fallecidos,4)),fontface="bold", size=5)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
z


z1 = ggplot(fallecidos.nacional, aes(Fechas, fallecidos))+
  geom_point(color="#78c679")+
  geom_smooth(mapping = aes(Fechas, pred.fallecidos.mala), se=TRUE, method = "gam", color="#004529")+
  labs(title = "Explanatory model of weekly moving average of deaths \nwithout interaction between doses",
       x ="Date",
       y= "Weekly moving average of deaths",
       tag = "D")+
  annotate(geom="text",x=as.Date("2021-02-15"),
           y=130,label=paste("R\u00b2 =", round(r2_fallecidos,4)),fontface="bold", size=5)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
z1


z3 = ggplot(fallecidos.nacional, aes(Fechas, fallecidos))+
  geom_point(color="#78c679", size=0.5)+
  #geom_line(aes(y=pred.fallecidos))+
  geom_smooth(mapping = aes(Fechas, pred.fallecidos), se=TRUE, method = "gam",
              color="#004529", size=0.5, alpha=0.4)+
  labs(x ="Date",
       y= "Weekly moving average of deaths",
       tag = "D")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=200,label=paste("R\u00b2 =", round(r1_fallecidos,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 250))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1))
z3


z4 = ggplot(fallecidos.nacional, aes(Fechas, fallecidos))+
  geom_point(color="#78c679", size=0.5)+
  geom_smooth(mapping = aes(Fechas, pred.fallecidos.mala), se=TRUE, method = "gam",
              color="#004529", size=0.5, alpha=0.4)+
  labs(x ="Date",
       y= "Weekly moving average of deaths")+
  annotate(geom="text",x=as.Date("2021-07-15"),
           y=200,label=paste("R\u00b2 =", round(r2_fallecidos,4)),fontface="bold", size=4)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 250))+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1))
z4


#################################################################################


grid.arrange(w,x,y,z)

grid.arrange(w1,x1,y1,z1)

grid.arrange(arrangeGrob(w3,f,w4,f,x3,f,x4, ncol = 7, widths = c(4,0.5,4,2,4,0.5,4)),
             arrangeGrob(f, ncol = 1, widths = c(19)),
             arrangeGrob(y3,f,y4,f,z3,f,z4, ncol=7,widths = c(4,0.5,4,2,4,0.5,4)), 
             heights=c(2.5/4, 0.3/4, 2.5/4), ncol=1)

