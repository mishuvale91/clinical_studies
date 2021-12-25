# ==================================PREDICTION================================================

# ===========================================================================================

# CREACIÓN DE LA VARIABLE ANIO A PARTIR DE LA VARIABLE "Start Date"

# anios <- data.frame(table(data_clean5$`Start Date`))

data_prueba <- data_clean5

data_prueba$`Start Date`<- gsub(" ",",",data_prueba$`Start Date`)

t_2 <- data.frame(data_prueba,str_split_fixed(data_prueba$`Start Date`,",", 4))

# t_2 <- as.data.frame(t_2)

t_2 <- t_2 %>% 
  mutate(anio=X4)

t_2 <- t_2 %>%
  mutate(anio=ifelse(anio=="",X2,X4)) %>% 
  select(Title,
         anio,
         Age,
         Gender,
         Enrollment) %>% 
  filter(anio!="" & Enrollment!="") %>% 
  group_by(Title,
           anio,
           Age,
           Gender,
           Enrollment) %>% 
  dplyr::summarise(total=n())

t_2$Gender <- as.factor(t_2$Gender)

# write.table(t_2, "prediction.csv" , sep = ";", row.names = F,col.names = T)

save(t_2, file="t_2.RData")

# =========================================================================
