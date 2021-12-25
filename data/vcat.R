
# VARIABLES CATEGÓRICAS - TIPO FACTOR

load("data/data_clean.RData")
attach(data_clean5)

par(mfrow=c(3,2))
plot(data_clean5$Status, main = "Status",
       xlab = "Status", ylab = "Frecuencia", col = c("royalblue", "seagreen", "purple", "grey"))
plot(data_clean5$Gender, main = "Gender",
       xlab = "Gender", ylab = "Frecuencia", col = c("royalblue", "pink", "purple","grey"))
plot(data_clean5$`Funded Bys`, main = "Funded Bys",
       xlab = "Funded Bys", ylab = "Frecuencia", col = c("royalblue", "red"))
plot(data_clean5$`Study Type`, main = "Study Type",
       xlab = "Study Type", ylab = "Frecuencia", col = c("royalblue","brown"))
plot(data_clean5$Phases, main = "Phases",
       xlab = "Phases", ylab = "Frecuencia", col = c("royalblue","red","seagreen","grey"))
plot(data_clean5$`Study Results`, main = "Study Results",
       xlab = "Study Results", ylab = "Frecuencia", col = c("purple","pink"))

