# Post II
options(scipen=999)

library(stringr)
library(magrittr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(scales)
library(earth)



casos<-fread("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos.csv")
fallecidos<-fread("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv")
uci<-fread("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_uci.csv")


#write.csv(fallecidos[CCAA=="Madrid"],"a.csv")

# exponencial -------------------------------------------------------------

periodos<-0:15
vector<- 2^periodos

crec_exp<-ggplot(data.table(vector,periodos), aes(x = periodos, y =vector)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = periodos,
    expand = c(0, 0.3)
  ) +
  theme_economist_white() +
  scale_y_continuous(
    breaks = seq(0, 40000, by = 5000),
    labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = "#E69F00") +
  geom_hline(yintercept = 0) +
  geom_label_repel(
    aes(label = round(vector, 0)),
    box.padding   = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  labs(
    title = "Ejemplo de un crecimiento exponencial",
    #subtitle = "Reporte oficial del Ministerio de Sanidad",
    x = "Día de evaluación",
    y = "Número a evaluar",
    caption = "Elaboración: Carlos Bort "
  )
crec_exp


# Deaths ------------------------------------------------------------------

fallecidos %>%
  melt( id.vars=c("cod_ine",  "CCAA")) -> casos_covid

setnames(casos_covid, 'variable', 'dia')
casos_covid[, dia := as.Date(dia, format='%Y-%m-%d')]
casos_covid <- casos_covid[CCAA != 'Total'] 
setorder(casos_covid, CCAA, dia)


# Como vemos faltan por parte del ministerio datos del 7 y 8 de Marzo
# Añadimos los

# Total días
dia = seq(min(casos_covid[["dia"]]),max(casos_covid[["dia"]]),by="day")
# Comunidades
ccaa_uniq = unique(casos_covid[["CCAA"]])

# Creamos una tabla con todos los días desde el inicio de contagios 
casos_nm=data.table(CCAA=rep(ccaa_uniq,each=length(dia)), dia=rep(dia,length(ccaa_uniq)))
## Merge para rellenar missings
casos_all<-merge(casos_nm,casos_covid,by=c("CCAA","dia"),all.x = TRUE)

# Reordenamos
setcolorder(casos_all,c("cod_ine","CCAA","dia","value"))


# Gráfico Madrid
max_value_mad = max(casos_all[CCAA=="Madrid"][["value"]],na.rm = T)
max_date_mad = max(casos_all[CCAA=="Madrid"][["dia"]],na.rm = T)


madrid_death_graph<- ggplot(casos_all[CCAA == "Madrid"], aes(x = dia, y = value, fill = CCAA)) +
  geom_col(width = 0.6) +
  scale_x_date(
    breaks = "day",
    expand = c(0, 0.3)
  ) +
  theme_economist_white() +
  scale_y_continuous(
    limits = c(0, max_value_mad),
    breaks = seq(0, max_value_mad, by = 50),
    labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = "#FF6666") +
  geom_hline(yintercept = 0) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, colour= "black")+
  geom_label_repel(
    aes(label = round(value, 0)),
    box.padding   = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  labs(
    title = "Numero de fallecimientos acumulados de CCAA Madrid",
    subtitle = "Reporte oficial del Ministerio de Sanidad",
    x = "Fecha",
    y = "Número de fallecimientos",
    caption = "Data Source: Datadista. Elaboración: Carlos Bort "
  )

madrid_death_graph
#ggsave(madrid_death_graph,filename = "img2/mortalidad_20.png",width = 13,height = 8,dpi = 320)


# Opcion atrás en el tiempo -----------------------------------------------


mad_death_cases<-casos_all[CCAA == "Madrid"]

# TOY EXAMPLE
muertos = 1
fatal_rate = 0.01
cases_caused = muertos/fatal_rate
infection_days = 17.3
doubling_time = 6.18
times_doubled = infection_days / doubling_time
total_cases = cases_caused*2^times_doubled
total_cases


death_to_contan<-function(fallecimientos,fatal_rate=0.01,infection_days=17.3,doubling_time=4){
  muertos = fallecimientos
  fatal_rate = fatal_rate
  cases_caused = muertos/fatal_rate
  infection_days = infection_days
  doubling_time = doubling_time
  times_doubled = infection_days / doubling_time
  total_cases = cases_caused*2^times_doubled
  return(total_cases)
}

death_to_contan_day<-function(fallecimientos, day="2020-03-04", fatal_rate = 0.01,date, infection_days = 17){
  muertos = fallecimientos
  fatal_rate = fatal_rate
  cases_caused = muertos/fatal_rate
  day_contan = as.Date(day) -infection_days
  return(list(day_contan,cases_caused))
}




col_names = c("date", "contag")

#Fatal_rate 0.01
## Aplicamos la fórmula
fatal_rate_case = 0.01
mad_death_cases[,c(col_names):=death_to_contan_day(fallecimientos=value, day = dia,fatal_rate = fatal_rate_case),by=dia]

contag_madrid_001<-mad_death_cases[,col_names,with=F]

contag_madrid_001[,type:=as.character(fatal_rate_case)]
#Columna numerica para fittear sin problema en fecha
contag_madrid_001[,numeric_t:=1:nrow(contag_madrid_001)]

#Fatal_rate 0.02
## Aplicamos la fórmula
fatal_rate_case = 0.02
mad_death_cases[,c(col_names):=death_to_contan_day(fallecimientos=value, day = dia,fatal_rate = fatal_rate_case),by=dia]

contag_madrid_002<-mad_death_cases[,col_names,with=F]

contag_madrid_002[,type:=as.character(fatal_rate_case)]
contag_madrid_002[,numeric_t:=1:nrow(contag_madrid_002)]

#Fatal_rate 0.0065
## Aplicamos la fórmula
fatal_rate_case = 0.0065
mad_death_cases[,c(col_names):=death_to_contan_day(fallecimientos=value, day = dia,fatal_rate = fatal_rate_case),by=dia]

contag_madrid_0065<-mad_death_cases[,col_names,with=F]

contag_madrid_0065[,type:=as.character(fatal_rate_case)]
contag_madrid_0065[,numeric_t:=1:nrow(contag_madrid_0065)]


mad_est_contag<-rbindlist(list(contag_madrid_001,contag_madrid_002,contag_madrid_0065),idcol = F,use.names = T)


mad_est_contag[,type_f:=as.factor(type)]

levels(mad_est_contag[["type_f"]])<-c("A - IFR 0,65%", "B - IFR 1%", "C - IFR 2%")


# graph -------------------------------------------------------------------

madrid_contag_proj<- 
  ggplot(mad_est_contag, aes(x = date, y = contag, fill = type)) +
  geom_col(width = 0.6,position = "dodge") +
  scale_x_date(
    breaks = "day",
    expand = c(0, 0.3)
  ) +
  theme_economist_white() +
  scale_y_continuous(
    #limits = c(0, max_value_mad),
    #breaks = seq(0, max_value_mad, by = 25),
    #labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = c("#FF6666","#6666FF","#75afff"),
                    labels = c("IFR 0,65%", "IFR 1%" , "IFR 2%")) +
  geom_hline(yintercept = 0) +
  #stat_smooth(geom='line', alpha=0.5, se=FALSE, colour= "black")+
  geom_label_repel(
    aes(label = round(contag, 0)),
    box.padding   = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50',
    show.legend = FALSE
  ) +
  labs(
    title = "Numero de congatiados estimados acumulados de CCAA Madrid ",
    subtitle = "Aproximación por número de contagiados en la CCAA de Madrid a través de las defunciones",
    fill = "Número contagiados",
    x = "Fecha",
    y = "Aproximación de contagiados",
    caption = "Data Source: Datadista. Elaboración: Carlos Bort "
  ) + 
  facet_grid(type_f~.)

madrid_contag_proj
#ggsave(madrid_contag_proj,filename = "img2/contagios_est.png",width = 13,height = 8,dpi = 400)


# Predicciones ------------------------------------------------------------

## Fechas
min_date <-min(contag_madrid_001[["date"]])
max_date <-max(contag_madrid_001[["date"]])
## Longitud pred
n<-nrow(contag_madrid_001)

next_days <- 1:7

# 
time = 1:n

# Models, a palo seco. Podría ser más limpio
fit_lm_1 = lm(log(contag+1) ~ numeric_t, data=contag_madrid_001)
fit_lm_2 = lm(log(contag+1) ~ numeric_t, data=contag_madrid_002)
fit_lm_3 = lm(log(contag+1) ~ numeric_t, data=contag_madrid_0065)

next_days <- 1:7
# 


mad_d <-
  data.table(
    day = seq(as.Date(min_date), as.Date(max_date), by = "day"),
    contag_01 = contag_madrid_001[["contag"]],
    preds_lm_001 = exp(predict(fit_lm_1, data.frame(numeric_t = c(1:n)))),
    contag_02 = contag_madrid_002[["contag"]],
    preds_lm_002 = exp(predict(fit_lm_2, data.frame(numeric_t = c(1:n)))),
    contag_03 = contag_madrid_0065[["contag"]],
    preds_lm_0065 = exp(predict(fit_lm_3, data.frame(numeric_t = c(1:n))))
  )
mad_d


mad_melted<-melt(mad_d,id=c(1))

mad_melted[,type := rep(c("contagt_001","contag_002","contag_0065"),each= n*2)]

mad_melted[,type_2 :=str_split(variable,"_")[[1]][1], by=.(day,variable)]


fit_2<-ggplot(mad_melted,aes(x=day,y=value, color=type_2)) + geom_line() + geom_point() + facet_grid(type~., scales="free")
fit_2

## No es necesario mucha cienca de datos para ver que esto NO fittea

#ggsave(madrid_contag_proj,filename = "img2/exponential_fit.png",width = 13,height = 8,dpi = 320)


##### NOTA:
## Aquí los modelos exponenciales, como exponenciales que son me explotan en número al intentar predecir
## una curva que me creo. La extrapolada de muertes. 
## Voy a probar otros ajustes no lineales



# nls ---------------------------------------------------------------------

# Sí, he copypasteado código a saco

## Fechas
min_date <-min(contag_madrid_001[["date"]])
max_date <-max(contag_madrid_001[["date"]])
## Longitud pred
n<-nrow(contag_madrid_001)

next_days <- 1:7

# 
time = 1:n

# Models, a palo seco. Podría ser más limpio

fit_nls_1 = nls(contag ~ (numeric_t ^ b1), start = c(b1 = 2), trace = T,data = contag_madrid_001)
fit_nls_2 = nls(contag ~ (numeric_t ^ b1), start = c(b1 = 2), trace = T,data = contag_madrid_002)
fit_nls_3 = nls(contag ~ (numeric_t ^ b1), start = c(b1 = 2), trace = T,data = contag_madrid_0065)


# R^2
1 - (deviance(fit_nls_1)/sum((contag_madrid_001[["contag"]]-mean(contag_madrid_001[["contag"]]))^2))
# R^2
1 - (deviance(fit_nls_1)/sum((contag_madrid_002[["contag"]]-mean(contag_madrid_002[["contag"]]))^2))
# R^2
1 - (deviance(fit_nls_1)/sum((contag_madrid_0065[["contag"]]-mean(contag_madrid_0065[["contag"]]))^2))

## NOTA: Esto fittea que alucinas

next_days <- 1:7
# 

mad_d <-
  data.table(
    day = seq(as.Date(min_date), as.Date(max_date), by = "day"),
    contag_03 = contag_madrid_0065[["contag"]],
    preds_nls_0065 = predict(fit_nls_3, data.frame(numeric_t = c(1:n))),
    contag_01 = contag_madrid_001[["contag"]],
    preds_nls_001 = predict(fit_nls_1, data.frame(numeric_t = c(1:n))),
    contag_02 = contag_madrid_002[["contag"]],
    preds_nls_002 = predict(fit_nls_2, data.frame(numeric_t = c(1:n)))
  )
mad_d


mad_melted<-melt(mad_d,id=c(1))

mad_melted[,type := rep(c("contag_0065","contag_001","contag_002"),each= n*2)]

mad_melted[,type_2 :=str_split(variable,"_")[[1]][1], by=.(day,variable)]


mad_melted[,type_f:=as.factor(type)]

levels(mad_melted[["type_f"]])<-c("B - IFR 1%", "C - IFR 2%", "A - IFR 0,65%")

#c("A - IFR 0,65%", "B - IFR 1%", "C - IFR 2%")

mad_melted$type_f = factor(mad_melted$type_f,c("A - IFR 0,65%", "B - IFR 1%", "C - IFR 2%"))

#levels(mad_melted[["type_f"]])<-factor(levels(mad_melted[["type_f"]]),c("Estim. 0.65%", "Estim. 1.00%", "Estim. 2.00%"))

# Graph

# scale_fill_manual(values = c("#FF6666","#6666FF","#75afff"),
#                   labels = c(" KoreaS 0,65%", "Estimada 1%" , "Penalizada 2%")) +

fit_nls <-
  ggplot(mad_melted, aes(x = day, y = value, color = type_2)) + geom_line() + geom_point() +
  scale_x_date(
    breaks = "day",
    expand = c(0, 0.3)
  ) +
  theme_economist_white() +
  scale_color_manual(values = c("#FF6666","#6666FF","#75afff"),
                     labels = c("Dato contagios extrapolado", "Predicciones")) +
  scale_y_continuous(
    labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = "#E69F00") +
  geom_hline(yintercept = 0) +
  
  labs(
    title = "Comparación entre predicción de contagiados y estimados anteriores en la CCAA de Madrid",
    subtitle = "Utilización de un modelo matemático para poder predecir los contagiados en los siguientes escenarios",
    color = "",
    x = "Fecha",
    y = "Número de contagiados estimados",
    caption = "Data Source: Datadista. Elaboración: Carlos Bort "
  ) + facet_grid(type_f ~ .)
fit_nls
#ggsave(fit_nls,filename = "img2/nls_fit.png",width = 13,height = 8,dpi = 320)



# Extrapolación -----------------------------------------------------------

estado_alarma <- as.Date("2020-03-16")

# Medidas en CCAA Madrid severas se tomaron el 

# Crece libremente, seguimos con el mismo modelo
## Medidas ayuso, bien hechas

days_free = estado_alarma - max_date


next_days <- 1:days_free

# Vamos a predecir por cada modelo y set de datos

mad_d <-
  data.table(
    contag_nls_0065 = c(contag_madrid_0065[["contag"]], predict(fit_nls_3, data.frame(numeric_t = c(n+next_days)))),
    contag_nls_001 =  c(contag_madrid_001[["contag"]], predict(fit_nls_1, data.frame(numeric_t = c(n+next_days)))),
    contag_nls_002 =  c(contag_madrid_002[["contag"]], predict(fit_nls_2, data.frame(numeric_t = c(n+next_days)))),
    day = seq(min_date, max_date+max(next_days), by = "day"),
    type = c(rep("real", n), rep("pred", max(next_days)))
  )


mad_melted_pred<-melt(mad_d,id=c(4,5))

mad_melted_pred[,type_2 :=str_split(variable,"_")[[1]][3], by=.(day,variable)]


# Modificamos orden
mad_melted_pred[,variable_f:=as.factor(variable)]

levels(mad_melted_pred[["variable_f"]])<-c("A - IFR 0,65%", "B - IFR 1%", "C - IFR 2%")


# 
mad_melted_pred[,type_f:=as.factor(type)]

#levels(mad_melted_pred[["type_f"]])<-c("real","pred")

mad_melted_pred$type_f = factor(mad_melted_pred$type_f,c("real", "pred"))


## Graph

models_3_estado_alarma<-ggplot(mad_melted_pred,aes(x=day,y=value, color=type_f)) + geom_line() + geom_point() +  
  scale_x_date(
    breaks = "day",
    expand = c(0, 0.3)
  ) +
  theme_economist_white() +
  # scale_color_manual(values = c("#FF6666","#6666FF","#75afff"),
  #                    labels = c("Dato contagios extrapolado", "Predicciones")) +
  scale_color_manual(values = c("#6666FF","#FF6666","#75afff"),
                     labels = c("Dato contagios extrapolado", "Predicciones de contagios")) +
  scale_y_continuous(
    #limits = c(0, NA),
    #breaks = seq(0, NA, by = 2500),
    labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = "#E69F00") +
  geom_hline(yintercept = 0) +
  geom_label_repel(
    aes(label = round(value, 0)),
    box.padding   = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50',
    show.legend = FALSE
  ) +
  labs(
    title = "Estimaciones contagiados totales en la CCAA de Madrid hasta el estado de alarma",
    subtitle = "Predicción por cada una de las diferentes escenarios de tasas de mortalidad",
    x = "Fecha",
    y = "Número de contagiados",
    color = "",
    caption = "Data Source: Datadista. Elaboración: Carlos Bort "
  ) +
  facet_grid(variable_f~.)
models_3_estado_alarma

#ggsave(models_3_estado_alarma,filename = "img2/pred_to_alarm_fit.png",width = 15,height = 8,dpi = 400)



# CFR IFR -----------------------------------------------------------------

# Predicciones anteriores:

contag_nls_0065 = c(contag_madrid_0065[["contag"]], predict(fit_nls_3, data.frame(numeric_t = c(n+next_days))))
contag_nls_001 =  c(contag_madrid_001[["contag"]], predict(fit_nls_1, data.frame(numeric_t = c(n+next_days))))
contag_nls_002 =  c(contag_madrid_002[["contag"]], predict(fit_nls_2, data.frame(numeric_t = c(n+next_days))))


# Hipótesis CFR e IFR

IFR<-c(0.5,0.4,0.075,0.025)
IFR_name<-rep("IFR",4)
IFR_type<-c("asymp","mild","severe","critical")


dt_ifr<-data.table(IFR_name,IFR_type,IFR)


# Escenarios

dt_ifr[,esc_0065:= round(IFR*max(contag_nls_0065),0)]
dt_ifr[,esc_001:= round(IFR*max(contag_nls_001),0)]
dt_ifr[,esc_002:= round(IFR*max(contag_nls_002),0)]

# Tabla artículo
write.table(dt_ifr,"img2/ifr_cfr.csv",sep = ";")

