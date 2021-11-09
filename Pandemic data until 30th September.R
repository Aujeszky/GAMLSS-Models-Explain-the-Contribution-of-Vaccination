#INCREMENTAL DE VACUNAS REGIONAL vs CASOS NUEVOS

library(readr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(gamlss)
library(zoo)

#-------------------------------------------------------------------------------#
#Importacion de los dataframe

vacunas_por_region = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto76/vacunacion_std.csv"
vacunacion_regional = read_csv(url(vacunas_por_region))

fabricantes_de_vacunas = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto76/fabricante_std.csv"
fabricantes = read_csv(url(fabricantes_de_vacunas))

casos_por_region = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/TotalesPorRegion_std.csv"
casos_regionales = read_csv(url(casos_por_region))

vacunas_prioritarias = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto79/total_vacunados_prioridad_std.csv"
prioridad_vacunas = read_csv(url(vacunas_prioritarias))

incidencia_por_comuna = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto18/TasaDeIncidencia_std.csv"
incidencia_comunal = read_csv(url(incidencia_por_comuna))

UCI_covid_por_region = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto8/UCI_std.csv"
UCI_regional = read_csv(url(UCI_covid_por_region))

vacunados_1dosis_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_1eraDosis_std.csv"
vac_1dosis = read_csv(url(vacunados_1dosis_edad))
vac_1dosis[is.na(vac_1dosis)] = 0

vacunados_2dosis_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_2daDosis_std.csv"
vac_2dosis = read_csv(url(vacunados_2dosis_edad))
vac_2dosis[is.na(vac_2dosis)] = 0

pacientes_UCI_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto9/HospitalizadosUCIEtario_std.csv"
UCI_edad = read_csv(url(pacientes_UCI_edad))
colnames(UCI_edad) = c("Edades", "Fecha", "Datos")

fallecidos_por_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto10/FallecidosEtario_std.csv"
fallecidos_etario = read_csv(url(fallecidos_por_edad))
colnames(fallecidos_etario) = c("Grupo_edad", "Fecha", "Fallecidos")

casos_rango_etario = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto16/CasosGeneroEtario_std.csv"
casos_por_edad = read_csv(url(casos_rango_etario))
colnames(casos_por_edad) = c("Grupo_edad", "Sexo", "Fecha", "Datos")

fallecidos_por_edad = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto10/FallecidosEtario_std.csv"
fallecidos_etario = read_csv(url(fallecidos_por_edad))

ventiladores_totales = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto20/NumeroVentiladores_std.csv"
ventiladores = read_csv(url(ventiladores_totales))
#-------------------------------------------------------------------------------#

poblacion_chilena = 19458310

### VOY A TRABAJAR HASTA EL 2021-09-30 ###

vacunas_totales = vacunacion_regional %>% filter(Region=="Total", Fecha<="2021-09-30") %>% 
  group_by(Fecha) %>% summarise(total = sum(Cantidad))

T.por.Fabricante1 = fabricantes %>% filter(Fabricante!="Total") %>% group_by(Fabricante) %>% 
  summarise(Vacunas = sum(Cantidad))

T.por.Fabricante1 = T.por.Fabricante1 %>% mutate(porcentaje = Vacunas/sum(Vacunas))


t_pri_dosis = prioridad_vacunas %>% group_by(Dosis) %>% summarise(sum(Cantidad, na.rm = TRUE))

cantidad_fabricante = fabricantes %>% filter(Fabricante!="Total", Fecha=="2021-09-30") %>% group_by(Fabricante) %>% summarise(t = sum(Cantidad))
total_fabricantes = sum(cantidad_fabricante$t)
t_fabricantes = (cantidad_fabricante$t/total_fabricantes)*100; t_fabricantes

###
vacunas_totales = vacunacion_regional %>% filter(Region=="Total",Fecha>="2021-02-03", Fecha<="2021-09-30") %>% 
  group_by(Fecha) %>% summarise(datos = sum(Cantidad))

casos_nuevos_pais = casos_regionales %>% filter(Region=="Total",Categoria=="Casos nuevos totales",
                                                Fecha>="2021-02-03", Fecha<="2021-09-30")

casos_activos = casos_regionales %>% filter(Region=="Total",Categoria=="Casos activos confirmados",
                                            Categoria=="Casos activos confirmados")

dosis.1 = c(rep("Primera dosis", length(vac_1dosis$`Primera Dosis`)))
dosis.1 = cbind(dosis.1, vac_1dosis)
colnames(dosis.1) = c("Dosis", "Edad", "Fecha", "Vacunas")

dosis.2 = c(rep("Primera dosis", length(vac_2dosis$`Segunda Dosis`)))
dosis.2 = cbind(dosis.2, vac_2dosis)
colnames(dosis.2) = c("Dosis", "Edad", "Fecha", "Vacunas")

vac.t = rbind(dosis.1, dosis.2)
vac.total = vac.t %>% group_by(Fecha, Edad) %>% summarise(Vacunas = sum(Vacunas))

vacunas.diarias.1 = vac_1dosis %>% filter(Fecha>="2021-02-03", Fecha<="2021-09-30") %>% 
  group_by(Fecha) %>% summarise(prim.dosis = sum(`Primera Dosis`))

vacunas.acumuladas1 = vacunacion_regional %>% filter(Fecha>="2021-02-03", Fecha<="2021-09-30",
                                                     Dosis=="Primera", Region=="Total")

vacunas.diarias.2 = vac_2dosis %>% filter(Fecha>="2021-02-03", Fecha<="2021-09-30") %>% 
  group_by(Fecha) %>% summarise(seg.dosis = sum(`Segunda Dosis`))

vacunas.acumuladas2 = vacunacion_regional %>% filter(Fecha>="2021-02-03", Fecha<="2021-09-30",
                                                     Dosis=="Segunda", Region=="Total")

sum(sum(vacunas.diarias.1$prim.dosis, vacunas.diarias.2$seg.dosis))

uci.nacional = UCI_edad %>% group_by(Fecha) %>% summarise(total = sum(Datos))
uci.nacional = uci.nacional %>% mutate(srate_ma01 = rollmean(total, k = 7, fill = NA))

###


#Grafico de vacunas totales por dosis
dosis_totales = vacunacion_regional %>% filter(Region=="Total", Fecha<="2021-09-30")
ggplot(dosis_totales, aes(Fecha, Cantidad, col=Dosis))+
  geom_line()+
  theme_minimal()

p_1dosis = dosis_totales$Cantidad[1081]/poblacion_chilena; p_1dosis
p_2dosis = dosis_totales$Cantidad[1082]/poblacion_chilena; p_2dosis
p_unica = dosis_totales$Cantidad[1083]/poblacion_chilena; p_unica
p_refuerzo = dosis_totales$Cantidad[1084]/poblacion_chilena; p_refuerzo

vac_ma18_1 = vac_1dosis %>% filter(Edad >="18") %>% summarise(sum(`Primera Dosis`))
vac_ma18_1/14982343
vac_ma18_2 = vac_2dosis %>% filter(Edad >="18") %>% summarise(sum(`Segunda Dosis`))
vac_ma18_2/14982343

#Grafico de vacunas por fabricante
total_vacunas = fabricantes %>% filter(Fabricante!="Total") %>% group_by(Fecha, Fabricante) %>% 
  summarise(t = sum(Cantidad))
total_vacunas_modificado = mutate(total_vacunas, simplificado_1000 = t/1000)
ggplot(total_vacunas_modificado, aes(Fecha, simplificado_1000, col=Fabricante))+
  geom_line()+
  theme_minimal()

  #Antes de hacer este gráfico revisar la ultima fecha de los datos
totales_por_fabricante = fabricantes %>% filter(Fecha=="2021-09-30", Fabricante!="Total") %>% 
  mutate(Cantidad, porcentaje = (Cantidad*100)/sum(Cantidad))
b = ggplot(totales_por_fabricante, aes(Fabricante, Cantidad, fill=Dosis))+
  geom_bar(position="dodge", stat="identity", color="black")+
  geom_text(aes(label=paste0(round(porcentaje,1),"%")),color="black", vjust=-0.5,position = position_dodge(width = 1),size=4)+
  scale_y_continuous(labels = scales::comma, limits = c(0, 12000000))+
  guides(fill=guide_legend(title="Dose"))+
  scale_fill_grey(labels=c("first", "second"))+
  labs(title = "Number of vaccines per dose \naccording to manufacturer",
       x="Date",
       y="Number of vaccines by manufacturers",
       tag = "B")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
b

fecha =  fabricantes$Fecha[length(fabricantes$Fecha)]
totales_por_fabricante1 = fabricantes %>% filter(Fecha==paste(fecha), Fabricante!="Total") %>% 
  mutate(Cantidad, simplificado_1000 = Cantidad/1000)
b1 = ggplot(totales_por_fabricante1, aes(Fabricante, Cantidad, fill=Dosis))+
  geom_bar(position="dodge", stat="identity", color="black")+
  geom_text(aes(label=paste0(round(Cantidad,1))),color="black", vjust=-0.5,position = position_dodge(width = 1),size=3)+
  scale_y_continuous(labels = scales::comma, limits = c(0, 12000000))+
  guides(fill=guide_legend(title="Dose"))+
  scale_fill_grey(labels=c("first", "second"))+
  labs(title = "Number of vaccines per dose \naccording to manufacturer",
       x="Date",
       y="Number of vaccines by manufacturers",
       tag = "B")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
b1

total = sum(totales_por_fabricante1$Cantidad)
sinovac = totales_por_fabricante1 %>% filter(Fabricante=="Sinovac") %>% 
  summarise(t = sum(Cantidad))
sinovac/total
pfizer = totales_por_fabricante1 %>% filter(Fabricante=="Pfizer") %>% 
  summarise(t = sum(Cantidad))
pfizer/total
cansino = totales_por_fabricante1 %>% filter(Fabricante=="CanSino") %>% 
  summarise(t = sum(Cantidad))
cansino/total
astrazeneca = totales_por_fabricante1 %>% filter(Fabricante=="Astra-Zeneca") %>% 
  summarise(t = sum(Cantidad))
astrazeneca/total

#Grafico de cantidad de vacunas por dosis
dosis_vacuna = vacunacion_regional %>% filter(Region=="Total", Fecha>="2021-02-03", Fecha<="2021-09-30",
                                              Dosis!="Unica", Dosis!="Refuerzo") %>% 
  mutate(Cantidad, simplificado_1000 = Cantidad/1000)
a = ggplot(dosis_vacuna, aes(Fecha, Cantidad, group=Dosis))+
  geom_area(aes(fill=Dosis), position = position_stack(reverse = TRUE))+
  xlab("Date")+
  ylab("Number of accumulated vaccines")+
  scale_y_continuous(labels = scales::comma, limits = c(0, 30000000))+
  guides(fill=guide_legend(title="Dose"))+
  scale_fill_grey(labels=c("first", "second"))+
  labs(title = "Cumulative number of vaccines per dose",
       x="Date",
       y="Number of accumulated vaccines",
       tag = "A")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
a

fech = vacunacion_regional$Fecha[length(vacunacion_regional$Fecha)]
dosis_vacuna1 = vacunacion_regional %>% filter(Region=="Total", Fecha>="2021-02-03", Fecha<=paste(fech),
                                              Dosis!="Unica") %>% 
  mutate(Cantidad, simplificado_1000 = Cantidad/1000)
a1 = ggplot(dosis_vacuna1, aes(Fecha, Cantidad, group=Dosis))+
  geom_area(aes(fill=Dosis), position = position_stack(reverse = TRUE))+
  scale_y_continuous(labels = scales::comma, limits = c(0, 32000000))+
  guides(fill=guide_legend(title="Dose"))+
  scale_fill_grey(labels=c("first", "second"))+
  labs(title = "Cumulative number of vaccines per dose",
       x="Date",
       y="Number of accumulated vaccines",
       tag = "a")+
  theme_cowplot()+
  theme(plot.title = element_text(hjust = 0.5))
a1

p.dosis = dosis_vacuna1 %>% filter(Dosis=="Primera") %>% summarise(total1 = sum(Cantidad))
s.dosis = dosis_vacuna1 %>% filter(Dosis=="Segunda") %>% summarise(total1 = sum(Cantidad))
(p.dosis-s.dosis)/poblacion_chilena
s.dosis/poblacion_chilena

legend = get_legend(a)
a = a + theme(legend.position="none")
b = b + theme(legend.position="none")
grid.arrange(a, b, legend,ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))

c = grid.arrange(a, b, legend, ncol=3, widths=c(2.5, 2.5, 0.4))


primera_dosis = vacunacion_regional %>% filter(Region=="Total", Fecha>="2021-02-03", Fecha<="2021-09-30",
                                               Dosis=="Primera")
primera_dosis$Cantidad/poblacion_chilena
segunda_dosis = vacunacion_regional %>% filter(Region=="Total", Fecha>="2021-02-03", Fecha<="2021-09-30", 
                                               Dosis=="Segunda")
(primera_dosis$Cantidad/poblacion_chilena)-(segunda_dosis$Cantidad/poblacion_chilena)


nuevo = c(casos_nuevos_pais[1:length(primera_dosis$Cantidad),3:4], 
          primera_dosis[4], segunda_dosis[4])
nuevo = data.frame(nuevo)
colnames(nuevo) = c("Fecha", "Casos", "primera_dosis", "segunda_dosis")

fabricantes$Cantidad[1735]+fabricantes$Cantidad[1736]

(19458310*(8300+8300+720))/1000000

df_nacional = cbind(casos_nuevos_pais[3:4], casos_activos[4], vacunas_totales[2], 
                    vacunas.acumuladas1[4], vacunas.acumuladas2[4])
colnames(df_nacional) = c("Fechas", "casos.nuevos", "casos.activos","vacunas.totales", 
                          "primera.dosis", "segunda.dosis")

#GRAFICO DE CASOS NUEVOS NACIONAL
Casos_totales = casos_regionales %>% filter(Region=="Total", Fecha<="2021-09-30",
                                            Categoria=="Casos nuevos totales")
mean(Casos_totales$Total[152:288])


media_movil = Casos_totales %>% 
  mutate(srate_ma01 = rollmean(Total, k = 7, fill = NA))

o = ggplot(media_movil, aes(x=Fecha))+
  geom_point(aes(y=Total), size=0.8, col="#feb24c")+
  geom_line(aes(y=srate_ma01), col="#fc4e2a", size=0.3)+
  geom_vline(xintercept=casos_regionales$Fecha[74511], color="red")+
  labs(title="Number of new cases daily",
       x ="Date",
       y="Number of new cases",
       tag = "A")+
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
o

casos_activos = casos_activos %>% mutate(srate_ma01 = rollmean(Total, k = 7, fill = NA))

p = ggplot(casos_activos, aes(Fecha, Total))+
  geom_point(col="#08519c", size=0.8)+
  geom_line(col="#9ecae1")+
  geom_vline(xintercept=casos_activos$Fecha[338], color="red")+
  labs(title="Number of active cases daily",
       x ="Date",
       y="Number of active cases",
       tag = "B")+
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
p


#Graficos UCI

q = ggplot(uci.nacional, aes(Fecha, total))+
  geom_point(size=1.2, color="#807dba")+
  geom_line(aes(y=srate_ma01), color="#54278f")+
  geom_vline(xintercept=uci.nacional$Fecha[309], color="red")+
  labs(title="Number of ICU beds occupied daily",
       x ="Date",
       y="ICU beds occupied",
       tag = "C")+
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
q

r = ggplot(UCI_edad, aes(Fecha, Datos, color=Edades))+
  geom_line(size=1)+
  geom_vline(xintercept=UCI_edad$Fecha[1542], color="red")+
  scale_color_discrete(breaks=c("<=39", "40-49", "50-59", "60-69", ">=70"), name="Ages")+
  labs(title="ICU beds occupied daily by age range",
       x ="Date",
       y="ICU beds occupied",
       tag = "D")+
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+
  theme_cowplot()+
  theme(legend.position = c(0.37, 0.7), legend.text=element_text(size=10))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
r

ggplot(UCI_edad, aes(Fecha, Datos, color=Edades))+
  geom_line(size=1)+
  geom_vline(xintercept=UCI_edad$Fecha[1542], color="red")+
  scale_color_discrete(breaks=c("<=39", "40-49", "50-59", "60-69", ">=70"), name="Ages")+
  labs(title="ICU beds occupied daily by age range",
       x ="Date",
       y="ICU beds occupied",
       tag = "D")+
  theme_cowplot()+
  theme(legend.position = c(0.05, 0.8), legend.text=element_text(size=20))+
  theme(plot.title = element_text(hjust = 0.5, size = 28))+
  theme(axis.text.x = element_text(angle=45, hjust = 1, size = 20),
        axis.text.y = element_text(hjust = 1, size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))


#Grafico Fallecidos
fallecidos.total = fallecidos_etario %>% group_by(fecha) %>% summarise(muertos = sum(`Casos confirmados`))
fallecidos.total.ma60 = fallecidos_etario %>% filter(`Grupo de edad`>="60-69") %>% group_by(fecha) %>% summarise(muertos = sum(`Casos confirmados`))

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

s = ggplot(f, aes(Fechas, Fallecidos))+
  geom_point(color="#78c679")+
  geom_line(aes(y=srate_ma01), color="#004529")+
  geom_vline(xintercept=f$Fechas[300], color="red")+
  scale_y_sqrt(limit=c(0, 1000))+
  labs(title="Weekly moving average of deaths",
       x ="Date",
       y="Moving average of deaths",
       tag = "E")+
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
s

f = ggplot() + theme_void()

grid.arrange(o, p, q, r, s)

grid.arrange(arrangeGrob(f,o,f,p,f, ncol = 5, widths = c(2.25,4,0.5,4,2.25)),
             arrangeGrob(f, ncol = 1, widths = c(13)),
             arrangeGrob(q,f,r,f,s, ncol=5,widths = c(4,0.5,4,0.5,4)), 
             heights=c(2.5/4, 0.3/4, 2.5/4), ncol=1)

liasofgrid.arrange(w, x, y, z)

grid.arrange(w1, x1, y1, z1)
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
