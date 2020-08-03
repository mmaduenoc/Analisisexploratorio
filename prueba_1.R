base<-read.xlsx("prueba1.xlsx")

summary(base)

plot_correlation(base,type="continuous")

cor(base)
summary(base)
rcorr(as.matrix(base))
sd(base$casos_confirmados)
sd(base$`%_cuenta_con_agua`)
sd(base$`%_tipo_vivienda_adecuado`)
sd(base$`%_servicioHigenico_bueno`)
