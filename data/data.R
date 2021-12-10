# ======================= LECTURA DE PAQUETES ======================================

library(foreign)
library(haven)
library(dplyr)
library(reshape2)
library(readxl)
library(data.table)
library(xlsx)
library(RPostgreSQL)
library(foreign)
library (dplyr)
library(stringr)

# ======================= LECTURA DE BASE DE DATOS ==================================

setwd("Base/")
data <- read.csv2("SearchResults.csv", sep = ",", stringsAsFactors = T)

# data<-as.data.frame(lapply(data,function(x) if(is.character(x))
#   iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=F)


# ====================ESTANDARIZACIÓN Y HOMOLOGACIÓN BDD======================================================


dt1<-function(x){
  a1<- chartr("áàãéèíóöúûüñçýõòìîžêëäšℓτχy","aaaeeioouuuncyooiizeeasltxy",x)
  return(a1)
}


dt2<- function(x){
  a_1<- tolower(x)
  return(a_1)
}

t_1<-data %>% mutate_all(dt2) %>% mutate_all(dt1)


# ====================== ESTADÍSTICA DESCRIPTIVA Y VISUALIZACIÓN =====================

# VERIFICAR LA ESTRUCTURA DE LA DATA

str(t_1)

# ESTADÍSTICAS BÁSICAS

summary(t_1)

# ESTADÍSTICAS DE VALORES VACÍOS
colSums(is.na(t_1))

colSums(t_1=="")


# SE TOMA EL VALOR DE "Desconocido" PARA LOS VALORES VACÍOS DE LA VARIABLE "Locations", VALOR DE "Sin Registro" PARA LAS VARIABLES DE "Interventions" y "Phases"
data_clean <- t_1 %>% 
              mutate(Locations=trimws(Locations)) %>% 
              mutate(Locations=ifelse(Locations=="","Desconocido",Locations),
              Interventions=trimws(Interventions),
              Interventions=ifelse(Interventions=="","Sin Registro",Interventions),
              Phases=trimws(Phases),
              Phases=ifelse(Phases=="","Sin Registro",Phases),
              Gender=ifelse(Gender=="","Sin Registro",Gender))
  
data_clean <- data_clean %>%
               mutate(Age_1=ifelse(grepl("18 months",Age)==T,"1.5",
                             ifelse(grepl("21 months",Age)==T,"1.75",
                             ifelse(grepl("1 month",Age)==T,"0.083",
                             ifelse(grepl("6 months",Age)==T,"0.5",
                             ifelse(grepl("1 year",Age)==T,"1",
                             ifelse(grepl("up to 18",Age)==T,"18",
                             ifelse(grepl("up to 35",Age)==T,"35",
                             ifelse(grepl("up to 40",Age)==T,"40",
                             ifelse(grepl("up to 44",Age)==T,"44",
                             ifelse(grepl("up to 45",Age)==T,"45",
                             ifelse(grepl("up to 49",Age)==T,"49",
                             ifelse(grepl("up to 50",Age)==T,"50",
                             ifelse(grepl("up to 55",Age)==T,"55",
                             ifelse(grepl("up to 59",Age)==T,"59",
                             ifelse(grepl("up to 60",Age)==T,"60",
                             ifelse(grepl("up to 64",Age)==T,"64",
                             ifelse(grepl("up to 65",Age)==T,"65",
                             ifelse(grepl("up to 69",Age)==T,"69",
                             ifelse(grepl("up to 70",Age)==T,"70",
                             ifelse(grepl("up to 71",Age)==T,"71",
                             ifelse(grepl("up to 75",Age)==T,"75",
                             ifelse(grepl("up to 80",Age)==T,"80",
                             ifelse(grepl("up to 85",Age)==T,"85",
                             ifelse(grepl("up to 100",Age)==T,"100",
                             ifelse(grepl("up to 120",Age)==T,"120",NA)))))))))))))))))))))))))) %>% 
               mutate(Age_2=ifelse(grepl("and older â",Age)==T,"120",
                             ifelse(grepl("1 year to 120",Age)==T,"120",
                             ifelse(grepl("1 year to 70",Age)==T,"70",
                             ifelse(grepl("1 year to 74",Age)==T,"74",
                             ifelse(grepl("up to 18",Age)==T,"64",
                             ifelse(grepl("up to 35",Age)==T,"64",
                             ifelse(grepl("up to 40",Age)==T,"64",
                             ifelse(grepl("up to 44",Age)==T,"64",
                             ifelse(grepl("up to 45",Age)==T,"64",
                             ifelse(grepl("up to 49",Age)==T,"64",
                             ifelse(grepl("up to 50",Age)==T,"64",
                             ifelse(grepl("up to 55",Age)==T,"64",
                             ifelse(grepl("up to 59",Age)==T,"64",
                             ifelse(grepl("up to 60",Age)==T,"64",    
                             ifelse(grepl("up to 64",Age)==T,"64",
                             ifelse(grepl("up to 65",Age)==T,"120",
                             ifelse(grepl("up to 69",Age)==T,"120",
                             ifelse(grepl("up to 70",Age)==T,"120",
                             ifelse(grepl("up to 71",Age)==T,"120",
                             ifelse(grepl("up to 75",Age)==T,"120",
                             ifelse(grepl("up to 80",Age)==T,"120",
                             ifelse(grepl("up to 85",Age)==T,"120",
                             ifelse(grepl("up to 100",Age)==T,"120",
                             ifelse(grepl("up to 120",Age)==T,"120",NA)))))))))))))))))))))))))
  
  
data_clean1 <- data_clean %>% 
                 mutate(Age_1_2=substr(Age,1,2),
                       Age_2_1=substr(Age,13,14))
  
data_clean1 <- data_clean1 %>% 
                   mutate(Age_1=ifelse(Age_1_2=="ch","0.083",Age_1),
                          Age_2=ifelse(Age_1_2=="ch","120",Age_2))


data_clean1 <- data_clean1 %>% 
               mutate(Age_1=ifelse(is.na(Age_1),Age_1_2,Age_1),
                      Age_2=ifelse(is.na(Age_2),Age_2_1,Age_2))

# =======================================================================================
# CAMBIAR DE FORMATO LAS VARIABLES "Enrollment", "Age_1", "Age_2"  DE CHARACTER A NUMERIC

data_clean1$Enrollment<-as.numeric(data_clean1$Enrollment)
data_clean1$Age_1<-as.numeric(data_clean1$Age_1)
data_clean1$Age_2<-as.numeric(data_clean1$Age_2)

# ==================PROMEDIO DE LA EDAD=====================================
data_clean1 <- data_clean1 %>% 
               mutate(promedio_edad=round((Age_1+Age_2)/2,0)) 
  
# =================================================================================================

# VISUALIZACIÓN DEL CONJUNTO DE DATOS

# VARIABLES CUANTITATIVAS

attach(data_clean1)
layout(matrix(c(1,2,3,4,5,6,7,8), 2, 2, byrow = TRUE))
hist(Enrollment)
hist(Age_1)
hist(Age_2)

# VARIABLES CATEGÓRICAS - TIPO FACTOR

# CAMBIAR DE FORMATO LAS VARIABLES "Enrollment", "Age_1", "Age_2"  DE CHARACTER A FACTOR

data_clean1$Status<-as.factor(data_clean1$Status)
data_clean1$Gender<-as.factor(data_clean1$Gender)
data_clean1$Funded.Bys<-as.factor(data_clean1$Funded.Bys)
data_clean1$Study.Type<-as.factor(data_clean1$Study.Type)
data_clean1$Study.Results<-as.factor(data_clean1$Study.Results)
data_clean1$Phases<-as.factor(data_clean1$Phases)

par(mfrow=c(2,2))
plot(data_clean1$Status, main = "Estado",
     xlab = "Estado", ylab = "Frecuencia")
plot(data_clean1$Gender, main = "Género",
     xlab = "Género", ylab = "Frecuencia")
plot(data_clean1$Funded.Bys, main = "Tipo de financiador",
     xlab = "Financiador", ylab = "Frecuencia")
plot(data_clean1$Study.Type, main = "Tipo de estudio",
     xlab = "Tipo de estudio", ylab = "Frecuencia")

par(mfrow=c(2,2))
plot(data_clean1$Phases, main = "Fases",
     xlab = "Fases", ylab = "Frecuencia")
plot(data_clean1$Study.Results, main = "Resultados",
     xlab = "Resultados", ylab = "Frecuencia")


# =====================================================

# Creación de nuevas variables 

# ==========================================================================

# t_1$Locations1 <- gsub(",","-",t_1$Locations)
# t_1$Locations1 <- gsub("/","-",t_1$Locations1)
# t_1$Locations1 <- gsub("|","-",t_1$Locations1)
# t_2 <- str_split_fixed(t_1$Locations1,"|", 10 )
# t_2 <- as.data.frame(t_2)

#  CONTIENTE Y PAÍS
# ============================AMERICA===========================================

data_clean2 <- data_clean1 %>% 
               mutate(Locations=trimws(Locations)) %>% 
               mutate(north_america=ifelse(grepl("canada",Locations)==T | grepl("alberta",Locations)==T | grepl("british columbia",Locations)==T |
                                           grepl("manitoba",Locations)==T | grepl("new brunswick",Locations)==T | grepl("newfoundland and labrador",Locations)==T |
                                           grepl("nova scotia",Locations)==T | grepl("nunavut",Locations)==T | grepl("ontario",Locations)==T |
                                           grepl("prince edward island",Locations)==T | grepl("quebec",Locations)==T | grepl("saskatchewan",Locations)==T |
                                           grepl("yukon territory",Locations)==T | grepl("greeland",Locations)==T | grepl("mexico",Locations)==T |
                                           grepl("united states",Locations)==T | grepl("alabama",Locations)==T | grepl("alaska",Locations)==T |
                                           grepl("arizona",Locations)==T | grepl("arkansas",Locations)==T | grepl("california",Locations)==T |
                                           grepl("colorado",Locations)==T | grepl("connecticut",Locations)==T | grepl("delaware",Locations)==T |
                                           grepl("district of columbia",Locations)==T | grepl("florida",Locations)==T | grepl("georgia",Locations)==T |
                                           grepl("hawaii",Locations)==T | grepl("idaho",Locations)==T | grepl("illinois",Locations)==T |
                                           grepl("indiana",Locations)==T | grepl("iowa",Locations)==T | grepl("kansas",Locations)==T |
                                           grepl("kentucky",Locations)==T | grepl("louisiana",Locations)==T | grepl("maine",Locations)==T |
                                           grepl("maryland",Locations)==T | grepl("massachusetts",Locations)==T | grepl("michigan",Locations)==T |
                                           grepl("minnesota",Locations)==T | grepl("mississippi",Locations)==T | grepl("missouri",Locations)==T |
                                           grepl("montana",Locations)==T | grepl("nebraska",Locations)==T | grepl("nevada",Locations)==T |
                                           grepl("new hampshire",Locations)==T | grepl("new jersey",Locations)==T |grepl("new mexico",Locations)==T |
                                           grepl("new york",Locations)==T | grepl("north carolina",Locations)==T | grepl("north dakota",Locations)==T |
                                           grepl("ohio",Locations)==T | grepl("oklahoma",Locations)==T | grepl("oregon",Locations)==T |
                                           grepl("pennsylvania",Locations)==T | grepl("rhode island",Locations)==T | grepl("south carolina",Locations)==T |
                                           grepl("south dakota",Locations)==T | grepl("tennessee",Locations)==T | grepl("texas",Locations)==T |
                                           grepl("utah",Locations)==T | grepl("vermont",Locations)==T | grepl("virginia",Locations)==T |
                                           grepl("washington",Locations)==T | grepl("west virginia",Locations)==T | grepl("wisconsin",Locations)==T |
                                           grepl("wyoming",Locations)==T,"North America",NA))


data_clean2 <- data_clean2 %>% 
               mutate(pais_norta=ifelse(grepl("canada",Locations)==T,"Canada",
                                 ifelse(grepl("new brunswick",Locations)==T,"New Brunswick",
                                 ifelse(grepl("ontario",Locations)==T,"Ontario",
                                 ifelse(grepl("mexico",Locations)==T,"Mexico",
                                 ifelse(grepl("united states",Locations)==T,"United States",
                                 ifelse(grepl("georgia",Locations)==T,"Georgia",
                                 ifelse(grepl("maine",Locations)==T,"Maine",
                                 ifelse(grepl("michigan",Locations)==T,"Michigan",
                                 ifelse(grepl("minnesota",Locations)==T,"Minnesota",
                                 ifelse(grepl("mississippi",Locations)==T,"Mississippi",
                                 ifelse(grepl("missouri",Locations)==T,"Missouri",
                                 ifelse(grepl("montana",Locations)==T,"Montana",
                                 ifelse(grepl("nebraska",Locations)==T,"Nebraska",
                                 ifelse(grepl("nevada",Locations)==T,"Nevada",
                                 ifelse(grepl("new hampshire",Locations)==T,"New Hampshire",
                                 ifelse(grepl("new jersey",Locations)==T,"New Jersey",
                                 ifelse(grepl("new mexico",Locations)==T,"New Mexico",
                                 ifelse(grepl("new york",Locations)==T,"New York",
                                 ifelse(grepl("north carolina",Locations)==T,"North Carolina",
                                 ifelse(grepl("north dakota",Locations)==T,"North Dakota",
                                 ifelse(grepl("ohio",Locations)==T,"Ohio",
                                 ifelse(grepl("oklahoma",Locations)==T,"Oklahoma",
                                 ifelse(grepl("oregon",Locations)==T,"Oregon",
                                 ifelse(grepl("pennsylvania",Locations)==T,"Pennsylvania",
                                 ifelse(grepl("rhode island",Locations)==T,"Rhode Island",
                                 ifelse(grepl("south carolina",Locations)==T,"South Carolina",
                                 ifelse(grepl("south dakota",Locations)==T,"South Dakota",
                                 ifelse(grepl("tennessee",Locations)==T,"Tennessee",
                                 ifelse(grepl("texas",Locations)==T,"Texas",
                                 ifelse(grepl("utah",Locations)==T,"Utah",
                                 ifelse(grepl("vermont",Locations)==T,"Vermont",
                                 ifelse(grepl("virginia",Locations)==T,"Virginia",
                                 ifelse(grepl("washington",Locations)==T,"Washington",
                                 ifelse(grepl("wisconsin",Locations)==T,"Wisconsin",NA)))))))))))))))))))))))))))))))))))
                                                                                                                            

data_clean2 <- data_clean2 %>% 
               mutate(south_america=ifelse(grepl("argentina",Locations)==T | grepl("bolivia",Locations)==T | grepl("brazil",Locations)==T |
                                           grepl("chile",Locations)==T | grepl("colombia",Locations)==T | grepl("ecuador",Locations)==T |
                                           grepl("french guiana",Locations)==T | grepl("guyana",Locations)==T | grepl("paraguay",Locations)==T |
                                           grepl("peru",Locations)==T | grepl("suriname",Locations)==T | grepl("uruguay",Locations)==T |
                                           grepl("venezuela",Locations)==T,"South America",NA),
                      pais_southa=ifelse(grepl("argentina",Locations)==T,"Argentina",
                                  ifelse(grepl("bolivia",Locations)==T,"Bolivia",
                                  ifelse(grepl("brazil",Locations)==T,"Brazil",
                                  ifelse(grepl("chile",Locations)==T,"Chile",
                                  ifelse(grepl("colombia",Locations)==T,"Colombia",
                                  ifelse(grepl("ecuador",Locations)==T,"Ecuador",
                                  ifelse(grepl("french guiana",Locations)==T,"French Guiana",
                                  ifelse(grepl("guyana",Locations)==T,"Guyana",
                                  ifelse(grepl("paraguay",Locations)==T,"Paraguay",
                                  ifelse(grepl("peru",Locations)==T,"Peru",
                                  ifelse(grepl("suriname",Locations)==T,"Suriname",
                                  ifelse(grepl("uruguay",Locations)==T,"Uruguay",
                                  ifelse(grepl("venezuela",Locations)==T,"Venezuela",NA))))))))))))))


data_clean2 <- data_clean2 %>%
               mutate(central_america=ifelse(grepl("bahamas",Locations)==T | grepl("belize",Locations)==T | grepl("costa rica",Locations)==T |
                                             grepl("cuba",Locations)==T | grepl("dominican republic",Locations)==T | grepl("el salvador",Locations)==T |
                                             grepl("guatemala",Locations)==T | grepl("haiti",Locations)==T | grepl("honduras",Locations)==T |
                                             grepl("jamaica",Locations)==T | grepl("nicaragua",Locations)==T | grepl("panama",Locations)==T |
                                             grepl("puerto rico",Locations)==T | grepl("trinidad and tobago",Locations)==T, "Central america",NA),

               pais_centrala=ifelse(grepl("bahamas",Locations)==T,"Bahamas",
                             ifelse(grepl("belize",Locations)==T,"Belize",
                             ifelse(grepl("costa rica",Locations)==T,"Costa Rica",
                             ifelse(grepl("cuba",Locations)==T,"Cuba",
                             ifelse(grepl("dominican republic",Locations)==T,"Dominican Republic",
                             ifelse(grepl("el salvador",Locations)==T,"El Salvador",
                             ifelse(grepl("guatemala",Locations)==T,"Guatemala",
                             ifelse(grepl("haiti",Locations)==T,"Haiti",
                             ifelse(grepl("honduras",Locations)==T,"Honduras",
                             ifelse(grepl("jamaica",Locations)==T,"Jamaica",
                             ifelse(grepl("nicaragua",Locations)==T,"Nicaragua",
                             ifelse(grepl("panama",Locations)==T,"Panama",
                             ifelse(grepl("puerto rico",Locations)==T,"Puerto Rico",
                             ifelse(grepl("trinidad and tobago",Locations)==T,"Trinidad and Tobago",NA)))))))))))))))

# ================================= AFRICA=====================================

data_clean2 <- data_clean2 %>%
               mutate(africa=ifelse(grepl("algeria",Locations)==T | grepl("angola",Locations)==T | grepl("benin",Locations)==T |
                                    grepl("botswana",Locations)==T | grepl("burkina faso",Locations)==T | grepl("burundi",Locations)==T |
                                    grepl("cameroon",Locations)==T | grepl("central african republic",Locations)==T | grepl("chad",Locations)==T |
                                    grepl("congo",Locations)==T | grepl("congo, the democratic republic of the",Locations)==T | grepl("côte d'ivoire",Locations)==T |
                                    grepl("djibouti",Locations)==T | grepl("egypt",Locations)==T | grepl("equatorial guinea",Locations)==T |
                                    grepl("eritrea",Locations)==T | grepl("ethiopia",Locations)==T | grepl("gabon",Locations)==T |
                                    grepl("gambia",Locations)==T | grepl("ghana",Locations)==T | grepl("guinea",Locations)==T |
                                    grepl("guinea-bissau",Locations)==T | grepl("kenya",Locations)==T | grepl("lesotho",Locations)==T |
                                    grepl("liberia",Locations)==T | grepl("libyan arab jamahiriya",Locations)==T | grepl("madagascar",Locations)==T |
                                    grepl("malawi",Locations)==T | grepl("mali",Locations)==T | grepl("mauritania",Locations)==T |
                                    grepl("morocco",Locations)==T | grepl("mozambique",Locations)==T | grepl("namibia",Locations)==T |
                                    grepl("niger",Locations)==T | grepl("nigeria",Locations)==T | grepl("rwanda",Locations)==T |
                                    grepl("senegal",Locations)==T | grepl("sierra leone",Locations)==T | grepl("somalia",Locations)==T |
                                    grepl("south africa",Locations)==T | grepl("sudan",Locations)==T | grepl("swaziland",Locations)==T |
                                    grepl("tanzania",Locations)==T | grepl("togo",Locations)==T | grepl("tunisia",Locations)==T |
                                    grepl("uganda",Locations)==T | grepl("zambia",Locations)==T |
                                    grepl("zimbabwe",Locations)==T , "Africa",NA),
                pais_africa=ifelse(grepl("algeria",Locations)==T,"Algeria",
                            ifelse(grepl("botswana",Locations)==T,"Botswana",
                            ifelse(grepl("chad",Locations)==T,"Chad",
                            ifelse(grepl("egypt",Locations)==T,"Egypt",
                            ifelse(grepl("ghana",Locations)==T,"Ghana",
                            ifelse(grepl("kenya",Locations)==T,"Kenya",
                            ifelse(grepl("mali",Locations)==T,"Mali",
                            ifelse(grepl("morocco",Locations)==T,"Morocco",
                            ifelse(grepl("niger",Locations)==T,"Niger",
                            ifelse(grepl("south africa",Locations)==T,"South Africa",
                            ifelse(grepl("tunisia",Locations)==T,"Tunisia",
                            ifelse(grepl("uganda",Locations)==T,"Uganda",NA)))))))))))))


# ==============================ASIA============================================

data_clean2 <- data_clean2 %>% 
                mutate(east_asia=ifelse(grepl("china",Locations)==T | grepl("hong kong",Locations)==T | grepl("korea, democratic people's republic of",Locations)==T |
                                        grepl("korea, republic of",Locations)==T | grepl("mongolia",Locations)==T | grepl("taiwan",Locations)==T |
                                        grepl("japan",Locations)==T,"East Asia",NA),
                        pais_easta=ifelse(grepl("china",Locations)==T,"China",
                                   ifelse(grepl("hong kong",Locations)==T,"Hong Kong",
                                   ifelse(grepl("korea, democratic people's republic of",Locations)==T,"Democratic People's Republic of Korea",
                                   ifelse(grepl("korea, republic of",Locations)==T,"Republic of Korea",
                                   ifelse(grepl("mongolia",Locations)==T,"Mongolia",
                                   ifelse(grepl("taiwan",Locations)==T,"Taiwan",
                                   ifelse(grepl("japan",Locations)==T,"Japan",NA))))))))
                                   
                                   
                                   
data_clean2 <- data_clean2 %>% 
               mutate(nort_asia=ifelse(grepl("armenia",Locations)==T | grepl("azerbaijan",Locations)==T | grepl("belarus",Locations)==T |
                                        grepl("georgia",Locations)==T | grepl("kazakhstan",Locations)==T | grepl("kyrgyzstan",Locations)==T |
                                        grepl("moldova, republic of",Locations)==T | grepl("russian federation",Locations)==T | grepl("tajikistan",Locations)==T |
                                        grepl("ukraine",Locations)==T | grepl("uzbekistan",Locations)==T,"North Asia",NA),
                       pais_nortasia=ifelse(grepl("armenia",Locations)==T,"Armenia",
                                     ifelse(grepl("azerbaijan",Locations)==T,"Azerbaijan",
                                     ifelse(grepl("belarus",Locations)==T,"Belarus",
                                     ifelse(grepl("georgia",Locations)==T,"Georgia",
                                     ifelse(grepl("kazakhstan",Locations)==T,"Kazakhstan",
                                     ifelse(grepl("kyrgyzstan",Locations)==T,"Kyrgyzstan",
                                     ifelse(grepl("moldova, republic of",Locations)==T,"Republic of Moldova",
                                     ifelse(grepl("russian federation",Locations)==T,"Russian Federation",
                                     ifelse(grepl("tajikistan",Locations)==T,"Tajikistan",
                                     ifelse(grepl("ukraine",Locations)==T,"Ukraine",
                                     ifelse(grepl("uzbekistan",Locations)==T,"Uzbekistan",NA))))))))))))
                                     
data_clean2 <- data_clean2 %>%                                      
               mutate(south_asia=ifelse(grepl("afghanistan",Locations)==T | grepl("bangladesh",Locations)==T | grepl("bhutan",Locations)==T |
                                         grepl("india",Locations)==T | grepl("nepal",Locations)==T | grepl("pakistan",Locations)==T |
                                         grepl("sri lanka",Locations)==T,"South Asia",NA),
                      pais_southasia=ifelse(grepl("afghanistan",Locations)==T,"Afghanistan",
                                      ifelse(grepl("bangladesh",Locations)==T,"Bangladesh",
                                      ifelse(grepl("bhutan",Locations)==T,"Bhutan",
                                      ifelse(grepl("india",Locations)==T,"India",
                                      ifelse(grepl("nepal",Locations)==T,"Nepal",
                                      ifelse(grepl("pakistan",Locations)==T,"Pakistan",
                                      ifelse(grepl("sri lanka",Locations)==T,"Sri Lanka",NA))))))))
                                             
                                             
data_clean2 <- data_clean2 %>%                   
               mutate(southeast_asia=ifelse(grepl("brunei darussalam",Locations)==T | grepl("cambodia",Locations)==T | grepl("indonesia",Locations)==T |
                                            grepl("lao people's democratic republic",Locations)==T | grepl("malaysia",Locations)==T | grepl("myanmar",Locations)==T |
                                            grepl("philippines",Locations)==T | grepl("singapore",Locations)==T | grepl("thailand",Locations)==T |
                                            grepl("vietnam",Locations)==T,"Southeast Asia",NA),
                       pais_sotheastasia=ifelse(grepl("brunei darussalam",Locations)==T,"Brunei Darussalam",
                                         ifelse(grepl("cambodia",Locations)==T,"Cambodia",
                                         ifelse(grepl("indonesia",Locations)==T,"Indonesia",
                                         ifelse(grepl("lao people's democratic republic",Locations)==T,"Lao People's Democratic Republic",
                                         ifelse(grepl("malaysia",Locations)==T,"Malaysia",
                                         ifelse(grepl("myanmar",Locations)==T,"Myanmar",
                                         ifelse(grepl("philippines",Locations)==T,"Philippines",
                                         ifelse(grepl("singapore",Locations)==T,"Singapore",
                                         ifelse(grepl("thailand",Locations)==T,"Thailand",
                                         ifelse(grepl("vietnam",Locations)==T,"Vietnam",NA)))))))))))


data_clean2 <- data_clean2 %>% 
                mutate(middle_east=ifelse(grepl("cyprus",Locations)==T | grepl("iran, islamic republic of",Locations)==T | grepl("iraq",Locations)==T |
                                          grepl("israel",Locations)==T | grepl("jordan",Locations)==T | grepl("kuwait",Locations)==T |
                                          grepl("lebanon",Locations)==T | grepl("oman",Locations)==T | grepl("qatar",Locations)==T |
                                          grepl("saudi arabia",Locations)==T | grepl("syrian arab republic",Locations)==T | grepl("turkey",Locations)==T |
                                          grepl("united arab emirates",Locations)==T | grepl("yemen",Locations)==T,"Middle East",NA),
                        pais_middle=ifelse(grepl("cyprus",Locations)==T,"Cyprus",
                                    ifelse(grepl("iran, islamic republic of",Locations)==T,"Iran, Islamic Republic of",
                                    ifelse(grepl("iraq",Locations)==T,"Iraq",
                                    ifelse(grepl("israel",Locations)==T,"Israel",
                                    ifelse(grepl("jordan",Locations)==T,"Jordan",
                                    ifelse(grepl("kuwait",Locations)==T,"Kuwait",
                                    ifelse(grepl("lebanon",Locations)==T,"Lebanon",
                                    ifelse(grepl("oman",Locations)==T,"Oman",
                                    ifelse(grepl("qatar",Locations)==T,"Qatar",
                                    ifelse(grepl("saudi arabia",Locations)==T,"Saudi Arabia",
                                    ifelse(grepl("syrian arab republic",Locations)==T,"Syrian Arab Republic",
                                    ifelse(grepl("turkey",Locations)==T,"Turkey",
                                    ifelse(grepl("united arab emirates",Locations)==T,"United Arab Emirates",
                                    ifelse(grepl("yemen",Locations)==T,"Yemen",NA)))))))))))))))
                                                                                                                               
                                                                                                               
# ================================= EUROPA ===================================================

data_clean2 <- data_clean2 %>% 
               mutate(europe=ifelse(grepl("albania",Locations)==T | grepl("austria",Locations)==T | grepl("belgium",Locations)==T |
                                     grepl("bosnia and herzegovina",Locations)==T | grepl("bulgaria",Locations)==T | grepl("croatia",Locations)==T |
                                     grepl("czech republic",Locations)==T | grepl("denmark",Locations)==T | grepl("estonia",Locations)==T |
                                     grepl("finland",Locations)==T | grepl("france",Locations)==T | grepl("germany",Locations)==T |
                                     grepl("greece",Locations)==T | grepl("hungary",Locations)==T | grepl("iceland",Locations)==T |
                                     grepl("ireland",Locations)==T | grepl("italy",Locations)==T | grepl("latvia",Locations)==T |
                                     grepl("lithuania",Locations)==T | grepl("luxembourg",Locations)==T | grepl("macedonia",Locations)==T |
                                     grepl("montenegro",Locations)==T | grepl("netherlands",Locations)==T | grepl("norway",Locations)==T |
                                     grepl("poland",Locations)==T | grepl("portugal",Locations)==T | grepl("romania",Locations)==T |
                                     grepl("serbia",Locations)==T | grepl("slovakia",Locations)==T | grepl("slovenia",Locations)==T |
                                     grepl("spain",Locations)==T | grepl("sweden",Locations)==T | grepl("switzerland",Locations)==T |
                                     grepl("united kingdom",Locations)==T | grepl("czechia",Locations)==T,"Europe",NA),
                      pais_europe=ifelse(grepl("albania",Locations)==T,"Albania",
                                  ifelse(grepl("austria",Locations)==T,"Austria",
                                  ifelse(grepl("belgium",Locations)==T,"Belgium",
                                  ifelse(grepl("bosnia and herzegovina",Locations)==T,"Bosnia and Herzegovina",
                                  ifelse(grepl("bulgaria",Locations)==T,"Bulgaria",
                                  ifelse(grepl("croatia",Locations)==T,"Croatia",
                                  ifelse(grepl("czech republic",Locations)==T,"Czech Republic",
                                  ifelse(grepl("denmark",Locations)==T,"Denmark",
                                  ifelse(grepl("estonia",Locations)==T,"Estonia",
                                  ifelse(grepl("finland",Locations)==T,"Finland",
                                  ifelse(grepl("france",Locations)==T,"France",
                                  ifelse(grepl("germany",Locations)==T,"Germany",
                                  ifelse(grepl("greece",Locations)==T,"Greece",
                                  ifelse(grepl("hungary",Locations)==T,"Hungary",
                                  ifelse(grepl("iceland",Locations)==T,"Iceland",
                                  ifelse(grepl("ireland",Locations)==T,"Ireland",
                                  ifelse(grepl("italy",Locations)==T,"Italy",
                                  ifelse(grepl("latvia",Locations)==T,"Latvia",
                                  ifelse(grepl("lithuania",Locations)==T,"Lithuania",
                                  ifelse(grepl("luxembourg",Locations)==T,"Luxembourg",
                                  ifelse(grepl("macedonia",Locations)==T,"Macedonia",
                                  ifelse(grepl("montenegro",Locations)==T,"Montenegro",
                                  ifelse(grepl("netherlands",Locations)==T,"Netherlands",
                                  ifelse(grepl("norway",Locations)==T,"Norway",
                                  ifelse(grepl("poland",Locations)==T,"Poland",
                                  ifelse(grepl("portugal",Locations)==T,"Portugal",
                                  ifelse(grepl("romania",Locations)==T,"Romania",
                                  ifelse(grepl("serbia",Locations)==T,"Serbia",
                                  ifelse(grepl("slovakia",Locations)==T,"Slovakia",
                                  ifelse(grepl("slovenia",Locations)==T,"Slovenia",
                                  ifelse(grepl("spain",Locations)==T,"Spain",
                                  ifelse(grepl("sweden",Locations)==T,"Sweden",
                                  ifelse(grepl("switzerland",Locations)==T,"Switzerland",
                                  ifelse(grepl("united kingdom",Locations)==T,"United Kingdom",
                                  ifelse(grepl("czechia", Locations)==T,"Czechia",NA))))))))))))))))))))))))))))))))))))
                                                                                                                                                                                                                                                                         

# ============================= PACIFICA ===================================

data_clean2 <- data_clean2 %>% 
                mutate(pacifica=ifelse(grepl("australia",Locations)==T | grepl("australian capital territory",Locations)==T | grepl("new south wales",Locations)==T |
                                       grepl("northern territory",Locations)==T | grepl("queensland",Locations)==T | grepl("south australia",Locations)==T |
                                       grepl("tasmania",Locations)==T | grepl("victoria",Locations)==T | grepl("western australia",Locations)==T |
                                       grepl("fiji",Locations)==T | grepl("new caledonia",Locations)==T | grepl("new zealand",Locations)==T |
                                       grepl("papua new guinea",Locations)==T | grepl("solomon islands",Locations)==T | grepl("vanuatu",Locations)==T, "Pacifica",NA),
                       pais_pacifica=ifelse(grepl("australia",Locations)==T,"Australia",
                                     ifelse(grepl("victoria",Locations)==T,"Victoria",
                                     ifelse(grepl("new zealand",Locations)==T,"New Zealand",NA))))
                                                                                                                                              
              
data_clean3 <- data_clean2 %>% 
               mutate(continente = north_america,
                      pais = pais_norta)

data_clean3 <- data_clean3 %>% 
               mutate(continente = ifelse(is.na(continente) & !is.na(africa),africa,
                                   ifelse(is.na(continente) & !is.na(europe), europe,
                                   ifelse(is.na(continente) & !is.na(east_asia),east_asia,
                                   ifelse(is.na(continente) & !is.na(nort_asia),nort_asia,
                                   ifelse(is.na(continente) & !is.na(south_asia),south_asia,
                                   ifelse(is.na(continente) & !is.na(southeast_asia),southeast_asia,
                                   ifelse(is.na(continente) & !is.na(pacifica),pacifica,continente))))))),
                      pais = ifelse(is.na(pais) & !is.na(pais_africa),pais_africa,
                             ifelse(is.na(pais) & !is.na(pais_europe), pais_europe,
                             ifelse(is.na(pais) & !is.na(pais_easta),pais_easta,
                             ifelse(is.na(pais) & !is.na(pais_nortasia),pais_nortasia,
                             ifelse(is.na(pais) & !is.na(pais_southasia),pais_southasia,
                             ifelse(is.na(pais) & !is.na(pais_sotheastasia),pais_sotheastasia,
                             ifelse(is.na(pais) & !is.na(pais_pacifica),pais_pacifica,pais))))))))


data_clean4 <- data_clean3 %>% 
               mutate(continente1 = south_america,
                      pais1 = pais_southa)


data_clean4 <- data_clean4 %>% 
               mutate(continente1=ifelse(is.na(continente) & !is.na(middle_east),middle_east, continente1),
                      pais1=ifelse(is.na(pais) & !is.na(pais_middle), pais_middle,pais1))


data_clean4 <- data_clean4 %>% 
               mutate(continente=ifelse(is.na(continente),continente1,continente),
                      pais=ifelse(is.na(pais),pais1,pais))


data_clean4 <- data_clean4 %>% 
                mutate(continente=ifelse(NCT.Number %in% c("nct02982564","nct03576521","nct04617236"),central_america,continente),
                       pais=ifelse(NCT.Number %in% c("nct02982564","nct03576521","nct04617236"),pais_centrala,pais))


# =============== SELECCIÓN DE VARIABLES =======================

# EXCLUIR LOS VALORES VACÍOS DE LA VARIABLE "Locations"

data_clean4 <- data_clean4 %>% 
               filter(Locations!="Desconocido")

data_clean5 <- data_clean4 %>% 
               select(NCT.Number,
                     Title,
                     Status,
                     Study.Results,
                     Conditions,
                     Interventions,
                     Sponsor.Collaborators,
                     Gender,
                     promedio_edad,
                     Phases,
                     Enrollment,
                     Funded.Bys,
                     Study.Type,
                     Start.Date,
                     Primary.Completion.Date,
                     Completion.Date,
                     Last.Update.Posted,
                     continente,
                     pais,
                     URL) %>% 
                plyr::rename(c("NCT.Number"="NCT",
                              "Study.Results"="Study Results",
                              "Sponsor.Collaborators"="Sponsor/Collaborators",
                              "Funded.Bys"="Funded Bys",
                              "Study.Type"="Study Type",
                              "Start.Date"="Start Date",
                              "Primary.Completion.Date"="Primary Completion Date",
                              "Completion.Date"="Completion Date",
                              "Last.Update.Posted"="Last Update Posted",
                              "promedio_edad"="Age",
                              "continente"="Continent",
                              "pais"="Country"))


save(data_clean5, file="data_clean.RData")

