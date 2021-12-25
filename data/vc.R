# VISUALIZACIÓN DEL CONJUNTO DE DATOS

# VARIABLES CUANTITATIVAS

load("data/data_clean.RData")
attach(data_clean5)
layout(matrix(c(1,2,3,4,5,6,7,8), 2, 2, byrow = TRUE))
hist(Enrollment, col = "purple")
hist(Age, col = "pink")
