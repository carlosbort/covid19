# Post Analysis
options(scipen=999)

library(magrittr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(scales)
library(earth)



casos<-fread("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos.csv")
fallecidos<-fread("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv")


# graph exponential -------------------------------------------------------

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
#ggsave(crec_exp,filename = "img/crec_exp.png",width = 13,height = 5,dpi = 320)

# graph_1 -----------------------------------------------------------------

casos %>%
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


# Graph 1

madrid_cont_graph<- ggplot(casos_all[CCAA == "Madrid"], aes(x = dia, y = value, fill = CCAA)) +
  geom_col(width = 0.6) +
  scale_x_date(
    breaks = "day",
    expand = c(0, 0.3)
  ) +
  theme_economist_white() +
  scale_y_continuous(
    limits = c(0, max_value_mad),
    breaks = seq(0, max_value_mad, by = 500),
    labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = "#E69F00") +
  geom_hline(yintercept = 0) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, colour= "black")+
  geom_label_repel(
    aes(label = round(value, 0)),
    box.padding   = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  labs(
    title = "Numero de contagiados acumulados de CCAA Madrid",
    subtitle = "Reporte oficial del Ministerio de Sanidad",
    x = "Fecha",
    y = "Número de contagiados",
    caption = "Data Source: Datadista. Elaboración: Carlos Bort "
  )

madrid_cont_graph

#ggsave(madrid_cont_graph,filename = "img/madrid_cont.png",width = 13,height = 8,dpi = 320)



# Extrapolacion_contagio --------------------------------------------------

# Número de casos a partir del cual hacemos el análisis
mad_casos_in = 400

# Cogemos los datos de Madrid 
mad_value<-casos_all[CCAA == "Madrid"][value>mad_casos_in][["value"]]
## Fechas
min_date <-min(casos_all[CCAA == "Madrid"][value>mad_casos_in][["dia"]])
max_date <-max(casos_all[CCAA == "Madrid"][value>mad_casos_in][["dia"]])  
## Longitud pred
n<-length(mad_value)

next_days <- 1:7

# 
time = 1:n

# Ajuste de una exponencial 
fit_lm = lm(log(mad_value) ~ time)

fit_nls = nls(mad_value ~ (time ^ b1), start = c(b1 = 2), trace = T)  #?nls
#fit_nls = nls(mad_value ~ b0 + (time ^ b1), start = c(b0=100,b1 = 2), trace = T)  #?nls

#

#
mad_d <-
  data.table(
    death = mad_value,
    day = seq(as.Date(min_date), as.Date(max_date), by = "day"),
    preds_lm = exp(predict(fit_lm, data.frame(time = c(1:n)))),
    preds_nls = predict(fit_nls, data.frame(time = c(1:n)))
  )
mad_d

fit_2<-ggplot(melt(mad_d,id=2),aes(x=day,y=value, color=variable)) + geom_line() + geom_point()
fit_2

#ggsave(fit_2,filename = "img/doble_fit.png",width = 13,height = 8,dpi = 320)


mad_d <-
  data.table(
    death_nls = c(mad_value, predict(fit_nls, data.frame(time = c(n+next_days)))),
    death_lm = c(mad_value, exp(predict(fit_lm, data.frame(time =c(n+next_days))))),
    day = seq(min_date, max_date+max(next_days), by = "day"),
    type = c(rep("real", n), rep("pred", max(next_days)))
  )

#

hospital_names <- list(
  'death_nls'="Modelo no lineal",
  'death_lm'="Modelo exponencial"
)
hospital_labeller <- function(variable,value){
  return(hospital_names[value])
}

pred_2_mod<-ggplot(melt(mad_d,id=c (3,4)),aes(x=day,y=value, color=type)) + geom_line() + geom_point() +  
  scale_x_date(
    breaks = "day",
    expand = c(0, 0.3)
  ) +
  theme_economist_white() +
  scale_y_continuous(
    #limits = c(0, NA),
    #breaks = seq(0, NA, by = 2500),
    labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = "#E69F00") +
  geom_hline(yintercept = 0) +
  geom_label_repel(
    aes(label = round(value, 0)),
    box.padding   = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  labs(
    title = "Predicción sobre el número de contagiados en la CCAA de Madrid",
    subtitle = "Utilización de dos modelos matemáticos para predecir los próximos 7 días",
    x = "Fecha",
    y = "Número de contagiados",
    caption = "Data Source: Datadista. Elaboración: Carlos Bort "
  ) +
  facet_grid(variable~., scales="free", labeller=hospital_labeller)

pred_2_mod


#ggsave(pred_2_mod,filename = "img/doble_pred.png",width = 13,height = 8,dpi = 320)


# Casos desplazados -------------------------------------------------------

# Exponential model
fit_lm 

# Non linear potencial model
fit_nls

# Creo una matriz de 14 días antes

dias_previos<- 7


# Total días
dia = seq(min(casos_covid[["dia"]])-dias_previos,max(casos_covid[["dia"]]),by="day")
# Comunidades
ccaa_uniq = unique(casos_covid[["CCAA"]])

# Creamos una tabla con todos los días desde el inicio de contagios 
casos_nm=data.table(CCAA=rep(ccaa_uniq,each=length(dia)), dia=rep(dia,length(ccaa_uniq)))
## Merge para rellenar missings
casos_hist<-merge(casos_nm,casos_covid,by=c("CCAA","dia"),all.x = TRUE)

casos_hist[is.na(casos_hist)] <- 0


mad_14_lag<-casos_hist[CCAA == "Madrid"]

mad_14_lag[,cod_ine:=NULL]

#mad_14_lag[,pred_lm:=exp(predict(fit_lm, data.frame(time = c(1:nrow(mad_14_lag)))))]

mad_14_lag[,pred_nls:=predict(fit_nls, data.frame(time = c(1:nrow(mad_14_lag))))]

max_pred<-max(c(mad_14_lag[["pred_nls"]], mad_14_lag[["pred_lm"]]))

colnames(mad_14_lag)[3]<-"cont_real"

#colnames(mad_14_lag)[4]<-"cont_est"

#melt(mad_14_lag,id.vars = c(1,2))

madrid_nls_graph<-ggplot(melt(mad_14_lag,id.vars = c(1,2)), aes(x = dia, y = value, fill = variable)) +
  geom_col(width = 0.6,position = "dodge") +
  scale_x_date(
    breaks = "day",
    expand = c(0, 0.3)
  ) +
  theme_economist_white() +
  scale_y_continuous(
    limits = c(0, max_pred),
    breaks = seq(0, max_pred, by = 50000),
    labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = c("#E69F00","#008b8b","#75afff"),
                    labels = c("Contagiados Acumulados","Contagiados Estimados", "Contagiados Estimados Potencial"),
                    name= "") +
  geom_hline(yintercept = 0) +
  geom_label_repel(
    aes(label = round(value, 0)),
    box.padding   = 0.35,
    point.padding = 0.5,
    direction = "y"
  ) +
  labs(
    title = "Numero de contagiados acumulados y estimados de la CCAA Madrid",
    subtitle = "Reporte oficial del Ministerio de Sanidad",
    x = "Fecha",
    y = "Número de contagiados",
    caption = "Data Source: Datadista. Elaboración: Carlos Bort "
  )
madrid_nls_graph

#ggsave(madrid_nls_graph,filename = "img/madrid_nls.png",width = 13,height = 8,dpi = 320)


# Test

# Deaths ------------------------------------------------------------------

fallecidos %>%
  melt( id.vars=c("cod_ine",  "CCAA")) -> casos_covid

setnames(casos_covid, 'variable', 'dia')
casos_covid[, dia := as.Date(dia, format='%d/%m/%Y')]
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
    breaks = seq(0, max_value_mad, by = 25),
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
#ggsave(madrid_death_graph,filename = "img/madrid_deaths.png",width = 13,height = 8,dpi = 320)


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


death_to_contan<-function(muertos,fatal_rate=0.01,infection_days=17.3,doubling_time=4){
  muertos = muertos
  fatal_rate = fatal_rate
  cases_caused = muertos/fatal_rate
  infection_days = infection_days
  doubling_time = doubling_time
  times_doubled = infection_days / doubling_time
  total_cases = cases_caused*2^times_doubled
  return(total_cases)
}

mad_death_cases[,cont:=death_to_contan(value)]

mad_death_cases[,cod_ine:=NULL]

madrid_death_est_graph<-ggplot(melt(mad_death_cases,id.vars = c(1,2)), aes(x = dia, y = value, fill = variable)) +
  geom_col(width = 0.6,position = "dodge") +
  scale_x_date(
    breaks = "day",
    expand = c(0, 0)
  ) +
  theme_economist_white() +
  scale_y_continuous(
    #limits = c(0, max_value_mad),
    breaks = seq(0, max_value_mad, by = 10000),
    labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = c("#FF6666","#6666FF","#75afff"),
                    labels = c("Contagiados Acumulados","Contagiados Estimados")) +
  geom_hline(yintercept = 0) +
  geom_label_repel(
    aes(label = round(value, 0)),
    #box.padding   = 0.35,
    #point.padding = 0.5,
    direction = "y"
  ) +
  labs(
    title = "Numero de contagiados acumulados y estimados de la CCAA Madrid",
    subtitle = "Reporte oficial del Ministerio de Sanidad",
    x = "Fecha",
    y = "Número de contagiados",
    caption = "Data Source: Datadista. Elaboración: Carlos Bort "
  )
madrid_death_est_graph


#ggsave(madrid_death_est_graph,filename = "img/madrid_deaths_est.png",width = 13,height = 8,dpi = 320)


# SIR ---------------------------------------------------------------------

library(deSolve)

## Thank Ignacio for this part of code!
N = 6662000
#N = 100

# Contact and recovery times
R0 = 6.49
Tr = 14
Tc = Tr/R0

# Contact reasoning rate, beta, and mean recovery rate, gamma, (in 1/days).
beta = 1/Tc 
gamma = 1./Tr


sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S / N
    dI <-  beta * I * S / N - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}  


parameters_values <- c(
  beta  = beta, # infectious contact rate (/person/day)
  gamma = gamma    # recovery rate (/day) 1/14
)  



initial_values <- c(
  S = N,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)

time_values <- seq(0, 100) # days

sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)

sir_values_1<-as.data.frame(sir_values_1)

setDT(sir_values_1) 

#sir_values_1[max(I)]

date_start = "2020/02/15"

sir_values_1[,time_date:=seq(as.Date(date_start),(as.Date(date_start)+max(time_values)),by="day")]

sir_values_1_melt = melt(sir_values_1,id = c(1,5))

max_value_sir = max(sir_values_1_melt[["value"]])



# Graph

SIR_example<-ggplot(data = sir_values_1_melt, aes(x=time_date, y= value, colour= variable)) + 
  geom_line() + geom_point() +
  theme_economist_white() +
  scale_y_continuous(
    limits = c(0, max_value_sir),
    breaks = seq(0, max_value_sir, by = 500000),
    labels = number_format(big.mark = ".")
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #scale_fill_manual(values = c("#FF6666","#6666FF","#75afff"),
  #                  labels = c("Contagiados Acumulados","Contagiados Estimados")) +
  scale_colour_manual(values = c("#619CFF","#F8766D","#00BA38"),
                      labels = c("Susceptible","Infeccioso","Recuperado")) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Simulación de casos de contagio sobre la población de la CCAA de Madrid",
    subtitle = "Modelo SIR en la propagacions del virus COVID-19",
    x = "Fecha",
    y = "Número de contagiados",
    caption = "Data Source: flattenthecurve.herokuapp.com - Elaboración: Carlos Bort "
  )
SIR_example
#ggsave(SIR_example,filename = "img/SIR_model.png",width = 13,height = 8,dpi = 320)


SIR_example

SIR_infec<-ggplot(data = sir_values_1_melt[variable=="I"][1:39], aes(x=time_date, y= value, colour= variable)) + 
  geom_line() + geom_point() +
  theme_economist_white() +
  scale_y_continuous(
   # limits = c(0, max_value_sir),
  #  breaks = seq(0, max_value_sir, by = 500000),
    labels = number_format(big.mark = ".")
  ) +
  scale_x_date(
    breaks = "day",
    expand = c(0, 0.3)
  )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_colour_manual(values = c("#F8766D"),
                      labels = c("Infeccioso")) +
  geom_hline(yintercept = 0) +
   geom_label_repel(
     aes(label = round(value, 0)),
  box.padding   = 0.35,
  point.padding = 0.5,
     direction = "y"
   ) +
  labs(
    title = "Simulación de casos de contagio sobre la población de la CCAA de Madrid",
    subtitle = "Modelo SIR en la propagacions del virus COVID-19",
    x = "Fecha",
    y = "Número de contagiados",
    caption = "Data Source: flattenthecurve.herokuapp.com - Elaboración: Carlos Bort "
  )

SIR_infec

#ggsave(SIR_infec,filename = "img/SIR_infec_model.png",width = 13,height = 8,dpi = 320)






