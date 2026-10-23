
# ============================================================
# LIBRARIES
# ============================================================

# Path management (load first)
library(here)

# Data manipulation
library(data.table)
library(dplyr)
library(janitor)
library(reshape2)
library(rio)
library(tidyverse)

# Spatial analysis
library(classInt)
#library(rgdal)      # requires sp; uses proj.4 if installed
#library(rgeos)
library(sp)

# Visualization
library(esquisse)
library(ggplot2)
library(kableExtra)
library(magick)
library(RColorBrewer)
library(wesanderson)

# I/O
library(foreign)
library(openxlsx)
library(sf)

# Statistics (MASS loaded before dplyr to avoid select() conflict)
library(MASS)
library(tigerstats)

# Utilities
library(comprehenr)





# ============================================================
# GOAL
# ============================================================
# This script processes 5 years of INEGI vital statistics to 
# produce municipal and state femicide rates.
# ============================================================

# NOTE: run this script from the folder that contains the
# geostatistics-clustering-mexico repository.

# ============================================================
# STEPS
# ============================================================
# 1. Standard Rate calculation functions
# 2. Homicide data loading
# 3. Core variable construction (from vital statistics - variable defintion)
# 4. Femicide / homicide classification
# 5. State and municipal rate calculation
# 5.1 State Rates (SR)
# 5.1.1 Projections - SR
# 5.1.2 Rates Computation - SR
# 5.1.3 Exploratory Data Analysis — State Rates 
# 5.1.3.1 Crude Rates (Mi) — Aggregated View 
# 5.1.3.2 Distribution Diagnostics on Crude Rates (Mi) 
# 5.1.3.3 Standardized State Rates — Aggregated View 
# 5.1.3.4 Standardized Rates by State — Detailed View
# 5.2 Municipal Rates (MR)
# 5.2.1 Projections (MR)
# 5.2.2 Rates Computation (MR)
# 5.2.3 Exploratory Data Analysis (MR)
# 6. Map: Standardized Femicide Rates by Municipality













# ============================================================
# 1. STANDARD RATE CALCULATION FUNCTIONS ----
# ============================================================

# Function that returns the variables of interest:
# F1: receives the homicide dataset from vital statistics.
#     at this point, homicides are expected to be classified into femicides, female homicides and male homicides
#     ******Adds 5-year age groups to the homicide records
#     PURPOSE of F1:
#     1. Clean the dataset from homicides that lack state or municipality code registration
#     2. *** ADD to the vital statistics records the 5-YEAR AGE GROUP to which the victim belongs.
#     PARAMETERS:
#     BD = INEGI vital statistics, filtered by homicides and classified into femicides, female and male homicides
#     base = "Homicidios", "Proyecciones"  ***** Whether the dataset is homicides or projections
#     unidad = "Estados", "Municipios"     *****record breakdown: state-level or municipal-level


F1 = function(BD, base, unidad){ #Receives dataset: Hom_
  require(dplyr)
  if(base == "Homicidios" & unidad == "Estados"){
    data_ = BD
    data_ = (data_ %>% 
               dplyr::select(EDAD_AGRU, ENT_OCURR, MUN_OCURR, SEXO,VIO_FAMI, arma, lugar, COD_arm, COD_lug, Nombre.de.A,ESCOLARIDA, EDO_CIVIL, EMBARAZO, LENGUA, OCUPACION,FEMI) %>% mutate(ENTIDAD = Nombre.de.A))
    data_ = (data_ %>% dplyr::select(EDAD_AGRU, ENTIDAD,ENT_OCURR, MUN_OCURR, SEXO, VIO_FAMI, arma, lugar, COD_arm, COD_lug, Nombre.de.A,ESCOLARIDA, EDO_CIVIL, EMBARAZO, LENGUA, OCUPACION,FEMI))
    # Remove homicides that do not have the municipality of occurrence registered
    a = as.factor(as.character(99))
    data_ = (data_ %>% dplyr::filter(ENT_OCURR != a))
    
    data_$EDAD_AGRU = as.numeric(as.character(data_$EDAD_AGRU))
    data_ = data_ %>%
      mutate( QUINQUENIO = case_when(
        EDAD_AGRU < 6 ~ "0-4", 
        EDAD_AGRU  == 6  ~ "5-9",
        EDAD_AGRU == 7 ~ "10-14",
        EDAD_AGRU == 8 ~ "15-19",
        EDAD_AGRU == 9 ~ "20-24",
        EDAD_AGRU == 10 ~ "25-29",
        EDAD_AGRU == 11 ~ "30-34",
        EDAD_AGRU == 12 ~ "35-39",
        EDAD_AGRU == 13 ~ "40-44",
        EDAD_AGRU == 14 ~ "45-49",
        EDAD_AGRU == 15 ~ "50-54",
        EDAD_AGRU == 16 ~ "55-59",
        EDAD_AGRU == 17 ~ "60-64",
        EDAD_AGRU == 18 ~ "65-69",
        EDAD_AGRU == 19 ~ "70-74",
        EDAD_AGRU == 20 ~ "75-79",
        EDAD_AGRU == 21 ~ "80-84",
        EDAD_AGRU == 22 ~ "85-89",
        EDAD_AGRU == 23 ~ "90-94",
        EDAD_AGRU == 24  ~ "95-99",
        EDAD_AGRU > 24  ~ "100-mas"
      )
      )
    return(data_)
  }else if(base == "Proyecciones" & unidad == "Estados"){
    data_ = BD
    data_ = (data_ %>% 
               dplyr::select(ENTIDAD,CVE_GEO, EDAD, SEXO, POBLACION) %>%
               mutate( QUINQUENIO = case_when(
                 EDAD < 5 ~ "0-4",
                 EDAD > 4 & EDAD < 10  ~ "5-9",
                 EDAD > 9 & EDAD < 15 ~ "10-14",
                 EDAD > 14 & EDAD < 20 ~ "15-19",
                 EDAD > 19 & EDAD < 25 ~ "20-24",
                 EDAD > 24 & EDAD < 30 ~ "25-29",
                 EDAD > 29 & EDAD < 35 ~ "30-34",
                 EDAD > 34 & EDAD < 40 ~ "35-39",
                 EDAD > 39 & EDAD < 45 ~ "40-44",
                 EDAD > 44 & EDAD < 50 ~ "45-49",
                 EDAD > 49 & EDAD < 55 ~ "50-54",
                 EDAD > 54 & EDAD < 60 ~ "55-59",
                 EDAD > 59 & EDAD < 65 ~ "60-64",
                 EDAD > 64 & EDAD < 70 ~ "65-69",
                 EDAD > 69 & EDAD < 75 ~ "70-74",
                 EDAD > 74 & EDAD < 80 ~ "75-79",
                 EDAD > 79 & EDAD < 85 ~ "80-84",
                 EDAD > 84 & EDAD < 90 ~ "85-89",
                 EDAD > 89 & EDAD < 95 ~ "90-94",
                 EDAD > 94 & EDAD < 100 ~ "95-99",
                 EDAD > 99  ~ "100-mas"
               )))
    return(data_)
  } else if(base == "Proyecciones" & unidad == "Municipios"){
    data_ = BD
    
    data_ = (data_ %>% 
               dplyr::select(ENTIDAD,CLAVE, MUN, EDAD_QUIN, SEXO, POBLACION) %>%
               mutate( QUINQUENIO = case_when(
                 EDAD_QUIN == "pobm_00_04" ~ "0-4",
                 EDAD_QUIN == "pobm_05_09"  ~ "5-9",
                 EDAD_QUIN == "pobm_10_14" ~ "10-14",
                 EDAD_QUIN == "pobm_15_19" ~ "15-19",
                 EDAD_QUIN == "pobm_20_24" ~ "20-24",
                 EDAD_QUIN == "pobm_25_29" ~ "25-29",
                 EDAD_QUIN == "pobm_30_34" ~ "30-34",
                 EDAD_QUIN == "pobm_35_39" ~ "35-39",
                 EDAD_QUIN == "pobm_40_44" ~ "40-44",
                 EDAD_QUIN == "pobm_45_49" ~ "45-49",
                 EDAD_QUIN == "pobm_50_54" ~ "50-54",
                 EDAD_QUIN == "pobm_55_59" ~ "55-59",
                 EDAD_QUIN == "pobm_60_64" ~ "60-64",
                 EDAD_QUIN == "pobm_65_mm" ~ "65-mas")))
    
    return(data_)
  }  else if(base == "Homicidios" & unidad == "Municipios"){
    data_ = BD
    data_ = (data_ %>% 
               dplyr::select(EDAD_AGRU, ENT_OCURR, MUN_OCURR, CLAVE,SEXO,VIO_FAMI, arma, lugar, COD_arm, COD_lug, Nombre.de.A, Nombre.de.A,ESCOLARIDA, EDO_CIVIL, EMBARAZO, LENGUA, OCUPACION,FEMI)%>%
               mutate(ENTIDAD = Nombre.de.A, MUN=MUN_OCURR))
    data_ = (data_ %>% dplyr::select(EDAD_AGRU, ENTIDAD, MUN, CLAVE,SEXO,VIO_FAMI, arma, lugar, COD_arm, COD_lug, Nombre.de.A,ESCOLARIDA, EDO_CIVIL, EMBARAZO, LENGUA, OCUPACION,FEMI))
    
    # HOMICIDES WITHOUT A REGISTERED MUNICIPALITY OF OCCURRENCE ARE EXCLUDED (MUN = MUN_OCURR)
    a = as.factor(as.character(999))
    data_ = (data_ %>% filter(MUN != 999))
    
    data_$EDAD_AGRU = as.numeric(as.character(data_$EDAD_AGRU))
    data_ = data_ %>%
      mutate( QUINQUENIO = case_when(
        EDAD_AGRU < 6 ~ "0-4", 
        EDAD_AGRU  == 6  ~ "5-9",
        EDAD_AGRU == 7 ~ "10-14",
        EDAD_AGRU == 8 ~ "15-19",
        EDAD_AGRU == 9 ~ "20-24",
        EDAD_AGRU == 10 ~ "25-29",
        EDAD_AGRU == 11 ~ "30-34",
        EDAD_AGRU == 12 ~ "35-39",
        EDAD_AGRU == 13 ~ "40-44",
        EDAD_AGRU == 14 ~ "45-49",
        EDAD_AGRU == 15 ~ "50-54",
        EDAD_AGRU == 16 ~ "55-59",
        EDAD_AGRU == 17 ~ "60-64",
        EDAD_AGRU > 17  ~ "65-mas"))
    return(data_)
  }
}











# F2: receives the homicide dataset from vital statistics with the 5-year age groups from function F1
#     receives the population projections dataset at state and municipal level
#     ******Adds 5-year age groups to the homicide records
#     PURPOSE of F2:
#     1. COMPUTE THE CRUDE MORTALITY RATE Mi = ni/
#     2. ni = count(#records by their i)    i (5-year age group). Calculate/count the records of femicides / female homicides / male homicides
#                                                              by their 5-year age group
#     3. Pob.Qui = population projected for year 2016 by 5-year age group i
#     PARAMETERS:
#     BD = INEGI vital statistics from F1 (already includes 5-year age groups)
#     unidad = "Homicidios", "Proyecciones"  ***** Whether the data is at state or municipal level
#     Proyecciones = population projections dataset (year 2016 for this study)
#     STEPS:
#       1. Count of Municipalities or States (as indicated), an intermediate step to obtain the rates
#       2. Append with Population Projections (Proy_Estados)
#       3. Compute Mi = Nxhomicides / Nxprojection



F2 = function(BD, unidad, Proyecciones){
  require(dplyr)
  data_ = BD
  Pro = Proyecciones
  require(dplyr)
  
  data_ = (data_ %>% 
             dplyr::mutate(FEMI = case_when(
               FEMI == 3 ~ "Homicidio Mascu",
               FEMI == 1  ~ "Feminicidio",
               FEMI == 0 ~ "Homicidio Fem"
                          )))
  
  if (unidad == "Municipio"){  # HOMICIDES BY MUNICIPALITY
    
    # Count of population falling within the 5-year age group and whether it belongs to 5-year groups and femicides / female hom. / male hom.
    data_Qui = (data_ %>% 
                  dplyr::select(ENTIDAD, MUN, CLAVE,QUINQUENIO, SEXO,FEMI) %>% 
                  group_by(ENTIDAD, MUN, CLAVE, QUINQUENIO, SEXO,FEMI) %>% 
                  dplyr::count())
    
    # Relabel SEXO for Homicidios_
    data_Qui$SEXO[data_Qui$SEXO == "F"] = "Mujeres"
    data_Qui$SEXO[data_Qui$SEXO == "H"] = "Hombres"
    
    
    # Sum Population from Projections:
    # BY 5-YEAR AGE GROUP
    Pro = (Pro %>% 
             dplyr::select(ENTIDAD, MUN, CLAVE, QUINQUENIO, SEXO, POBLACION) %>% 
             group_by(ENTIDAD, MUN, CLAVE,QUINQUENIO, SEXO) %>% 
             dplyr::summarise(Pob.Qui = sum(POBLACION)))
    Pro = (Pro %>% dplyr::mutate(FEMI = case_when(
      SEXO == "Mujeres" ~ "Homicidio Fem",
      SEXO == "Hombres" ~ "Homicidio Mascu")))
    # Duplicate female population
    A = (Pro %>% dplyr::filter(SEXO == "Mujeres") %>% 
           dplyr::select(ENTIDAD, MUN, CLAVE, QUINQUENIO, SEXO, Pob.Qui) %>%
           dplyr::mutate(FEMI = "Feminicidio"))
    Pro = rbind(Pro,A)
    #
    Pro$ENTIDAD = toupper(Pro$ENTIDAD)
    Pro = (Pro %>% dplyr::rename(NAME.MUN = MUN))
    
    # LEFT JOIN
    # Homicides already summarized by 5-year age group and femicides / female hom. / male hom. are appended to the population projections
    Quinquenios = Pro %>% dplyr::left_join(data_Qui, by= c("ENTIDAD"="ENTIDAD", "SEXO" = "SEXO", "CLAVE"="CLAVE", 
                                                           "FEMI"="FEMI", "QUINQUENIO"="QUINQUENIO"))
    
    # NAs correspond to municipalities WITHOUT homicides
    Quinquenios$n[is.na(Quinquenios$n)] = 0
    
    
    
    
    # Compute Mi, per 5-year age group
    Quinquenios = (Quinquenios %>%
                     mutate(Mi = n/(Pob.Qui*5)))
    
    
    # NAs correspond to municipalities WITHOUT homicides
    Quinquenios$Mi[is.na(Quinquenios$Mi)] = 0
    
    
    return(Quinquenios)
    
  }else{ #States
    Pro = (Pro %>% 
             dplyr::select(ENTIDAD,QUINQUENIO,SEXO,POBLACION) %>%
             group_by(ENTIDAD,QUINQUENIO,SEXO) %>% 
             dplyr::summarise(Pob.Qui = sum(POBLACION)))
    
    Pro = (Pro %>% dplyr::mutate(FEMI = case_when(
      SEXO == "Mujeres" ~ "Homicidio Fem",
      SEXO == "Hombres" ~ "Homicidio Mascu")))
    # Duplicate female population
    A = (Pro %>% dplyr::filter(SEXO == "Mujeres") %>% 
           dplyr::select(ENTIDAD, QUINQUENIO, SEXO, Pob.Qui) %>%
           dplyr::mutate(FEMI = "Feminicidio"))
    Pro = rbind(Pro,A) 
    #
    data_Qui = (data_ %>% 
                  dplyr::select(ENTIDAD, QUINQUENIO, SEXO,FEMI) %>% 
                  group_by(ENTIDAD, QUINQUENIO, SEXO,FEMI) %>% 
                  dplyr::count())
    data_Qui = (data_Qui %>%
                  dplyr::select(ENTIDAD, QUINQUENIO, SEXO, FEMI,n))
    
    # Relabel SEXO for Homicidios_
    data_Qui$SEXO[data_Qui$SEXO == "F"] = "Mujeres"
    data_Qui$SEXO[data_Qui$SEXO == "H"] = "Hombres"
    
    # LEFT JOIN
    Quinquenios = left_join(Pro, data_Qui, by= c("ENTIDAD"="ENTIDAD",  "SEXO" = "SEXO","QUINQUENIO"="QUINQUENIO", "FEMI"="FEMI"))
    
    # NAs correspond to municipalities WITHOUT homicides
    Quinquenios$n[is.na(Quinquenios$n)] = 0
    
    # Compute Mi
    Quinquenios = (Quinquenios %>%
                     mutate(Mi = n/(Pob.Qui*5)))
    
    
    # NAs correspond to municipalities WITHOUT homicides
    Quinquenios$Mi[is.na(Quinquenios$Mi)] = 0
    return(Quinquenios)
  }
}





# F3: receives the standardized rate computation from function F2
#     PURPOSE of F3:
#     1. Compute the 5-year age group weights assigned to each group at state or municipal level
#     PARAMETERS:
#     BD = 5-year age-specific rates coming from F2

F3 = function(BD){
  N = BD %>% summarise(sum(POBLACION))
  N = N$`sum(POBLACION)` 
  
  ponderadores = (BD %>% 
                    dplyr::select(QUINQUENIO, POBLACION) %>%
                    dplyr::group_by(QUINQUENIO) %>%
                    dplyr::summarise(POB.QUI = sum(POBLACION)))
  
  ponderadores = (ponderadores %>%
                    dplyr::mutate(Ci = POB.QUI/N))
  
  # DIVIDE BY a
  return(ponderadores)
}



# F4: receives the 5-year age-specific rates and their weights at state or municipal level
#     PURPOSE of F4:
#     STANDARDIZED HOMICIDE RATE BY MUNICIPALITY
#     1. Compute the standardized rates (accounting for the age composition of the population by 5-year age groups)
#     PARAMETERS:
#     BD = 5-year age-specific rates coming from F2
#     ponderadores = generated by function F3
#     unidad = "Estados", "Municipios"     *****record breakdown: state-level or municipal-level


F4 = function(BD, ponderadores, unidad){
  data_ = BD
  data_ = left_join(data_, ponderadores, by = c("QUINQUENIO" = "QUINQUENIO")) #Pego su ponderador correspondiente
  data_ = (data_ %>% 
             dplyr::mutate(PRODUCTO = Mi*Ci)) #Se multiplica Mi de cada sexo por sU Ci correspondiente seg??n el quenquenio
  # ESTANDARIZACI??N:
  if(unidad == "Municipios"){   
    TASA = (data_ %>%
              dplyr::group_by(ENTIDAD, CLAVE,SEXO,FEMI) %>%
              dplyr::summarise(Tasa.Homicidios = sum(PRODUCTO)))
    return(TASA)
  }else{  
    TASA = (data_ %>%
              dplyr::group_by(ENTIDAD, SEXO,FEMI) %>%
              dplyr::summarise(Tasa.Homicidios = sum(PRODUCTO)))
    return(TASA)
  }
}
#####################################################################################################





# ============================================================
# 2. HOMICIDE DATA LOADING
# ============================================================

### Registered Death Statistics of INEGI from period 2014-2018

def14 =rio::import("geostatistics-clustering-mexico/data/raw/Defunciones/DEFUN14.dbf")
def15 =rio::import("geostatistics-clustering-mexico/data/raw/Defunciones/DEFUN15.DBF")
def16 =rio::import("geostatistics-clustering-mexico/data/raw/Defunciones/DEFUN16.dbf")
def17 =rio::import("geostatistics-clustering-mexico/data/raw/Defunciones/DEFUN17.dbf")
def18= rio::import("geostatistics-clustering-mexico/data/raw/Defunciones/DEFUN18.DBF")



### STACK DATA BASES OF Death statistics 

# Homogenize Columns
def14 = (def14 %>% dplyr::select(-COND_ACT, -ENT_OCULES, -MUN_OCULES, -LOC_OCULES, -RAZON_M))
def15 = (def15 %>% dplyr::select(-COND_ACT, -ENT_OCULES, -MUN_OCULES, -LOC_OCULES, -RAZON_M))
def16 = (def16 %>% dplyr::select(-COND_ACT, -ENT_OCULES, -MUN_OCULES, -LOC_OCULES, -RAZON_M))
def17 = (def17 %>% dplyr::select(-COND_ACT, -ENT_OCULES, -MUN_OCULES, -LOC_OCULES, -RAZON_M))
def18 = (def18 %>% dplyr::select(-COND_ACT, -ENT_OCULES, -MUN_OCULES, -LOC_OCULES, -RAZON_M))


# Stack Tables
Defunciones = rbind(def14,def15,def16,def17,def18)


# Select variables to be used
D = (Defunciones %>% dplyr::select(CAUSA_DEF,SEXO,ANIO_OCUR,ANIO_REGIS,NACIONALID,EDAD,EDAD_AGRU,ENT_OCURR,MUN_OCURR,PAR_AGRE,VIO_FAMI, ENT_RESID, ESCOLARIDA, EDO_CIVIL, EMBARAZO,LENGUA,OCUPACION, PRESUNTO))
# Filter deaths that ocurred between 2014-2018
D = (D %>% dplyr::filter(ANIO_OCUR > 2013 , ANIO_OCUR != 9999)) # 2014 2015 2016 2017 2018 








# ============================================================
# 3. CORE VARIABLE CONSTRUCTION (from vital statistics - variables definition)
# ============================================================
## Cause of Death Classification
## Deaths are classified using ICD-10 codes into 15+ categories including 
## homicide, suicide, accidents, and disease groups. The classification 
## was originally provided by my thesis advisor and refactored here using 
## dplyr's `case_when()` for readability and maintainability.



# ---- 3.1 Extract components from ICD-10 cause code ----
DEF <- D %>%
  mutate(
    letra1 = substr(CAUSA_DEF, 1, 1),
    num1   = substr(CAUSA_DEF, 2, 3),
    num2   = substr(CAUSA_DEF, 4, 4),
    causa  = paste0(letra1, num1),
    num    = as.numeric(num1)
  )


# ---- 3.2 Classify cause of death (causa2) ----
# Based on ICD-10 categories
DEF <- DEF %>%
  mutate(
    causa2 = case_when(
      # 1. Homicide (X85-Y09)
      (letra1 == "X" & num >= 85) | (letra1 == "Y" & num <= 9)         ~ 1,
      # 2. Suicide (X60-X84)
      letra1 == "X" & num >= 60 & num <= 84                            ~ 2,
      # 3. Accidental causes (X0-X59, W, V)
      (letra1 == "X" & num <= 59) | (letra1 == "W" & num <= 99) |
        (letra1 == "V" & num <= 99)                                    ~ 3,
      # 4. Infectious intestinal diseases (A, B0-B99)
      letra1 == "A" | (letra1 == "B" & num <= 99)                      ~ 4,
      # 5. Neoplasms (C, D0-D48)
      letra1 == "C" | (letra1 == "D" & num <= 48)                      ~ 5,
      # 6. Circulatory system diseases (I)
      letra1 == "I" & num <= 99                                        ~ 6,
      # 7. Mental disorders (F)
      letra1 == "F" & num <= 99                                        ~ 7,
      # 8. Endocrine, nutritional and metabolic disorders (E0-E90)
      letra1 == "E" & num <= 90                                        ~ 8,
      # 9. Digestive system diseases (K0-K93)
      letra1 == "K" & num <= 93                                        ~ 9,
      # 10. Perinatal conditions (P0-P96)
      letra1 == "P" & num <= 96                                        ~ 10,
      # 11. Other external causes (S, T, Y10+)
      letra1 == "S" | letra1 == "T" | (letra1 == "Y" & num >= 10)      ~ 11,
      # 12. Other causes (D50-D89, G, H, J, L, M, N, O, Q, R, Z)
      (letra1 == "D" & num >= 50 & num <= 89) |
        letra1 %in% c("G", "H", "J", "L", "M", "N", "O", "Q", "R", "Z") ~ 12,
      # Default: unclassified
      TRUE                                                             ~ 0
    )
  )


# ---- 3.3 Classify weapon / method (arma) — only for external causes ----
DEF <- DEF %>%
  mutate(
    arma = case_when(
      letra1 == "X" & num >= 93 & num <= 97                            ~ 1,  # Firearms, explosives, fire
      (letra1 == "X" & num == 99) | (letra1 == "Y" & num == 0)         ~ 2,  # Sharp or blunt weapons
      letra1 == "X" & ((num >= 85 & num <= 90) | num == 98)            ~ 3,  # Chemicals, gases, drugs
      (letra1 == "X" & num >= 91 & num <= 92) |
        (letra1 == "Y" & ((num >= 1 & num <= 4) | num == 8))           ~ 4,  # Hanging, drowning, force
      letra1 == "Y" & num == 5                                         ~ 5,  # Sexual aggression
      letra1 == "Y" & (num == 6 | num == 7)                            ~ 6,  # Negligence, abandonment
      letra1 == "Y" & num == 9                                         ~ 9,  # Unspecified means
      TRUE                                                             ~ NA_real_
    )
  )


# ---- 3.4 Classify location (lugar) — only for homicides ----
DEF <- DEF %>%
  mutate(
    lugar = case_when(
      causa2 == 1 & num2 == "0"                                          ~ 1,  # Home
      causa2 == 1 & num2 == "4"                                          ~ 2,  # Streets / highways
      causa2 == 1 & num2 %in% c("1", "2", "3", "5", "6", "7", "8")       ~ 3,  # Other place
      causa2 == 1 & arma == 6                                            ~ 9,  # Unspecified (negligence)
      causa2 == 1 & arma == "9"                                            ~ 9,  # Unspecified (negligence)
      causa2 == 1 & num2 == "9"                                          ~ 9,  # Unspecified
      TRUE                                                               ~ NA_real_
    )
  )



# ---- 3.5 Recode age groups (INEGI codes) to age brackets ----
DEF <- DEF %>%
  mutate(
    Age2 = as.numeric(EDAD_AGRU),
    Age = case_when(
      Age2 == 1               ~ 0,
      Age2 %in% c(2, 3, 4, 5) ~ 1,
      Age2 == 6               ~ 5,
      Age2 == 7               ~ 10,
      Age2 == 8               ~ 15,
      Age2 == 9               ~ 20,
      Age2 == 10              ~ 25,
      Age2 == 11              ~ 30,
      Age2 == 12              ~ 35,
      Age2 == 13              ~ 40,
      Age2 == 14              ~ 45,
      Age2 == 15              ~ 50,
      Age2 == 16              ~ 55,
      Age2 == 17              ~ 60,
      Age2 == 18              ~ 65,
      Age2 == 19              ~ 70,
      Age2 == 20              ~ 75,
      Age2 == 21              ~ 80,
      Age2 == 22              ~ 85,
      Age2 == 23              ~ 90,
      Age2 == 24              ~ 95,
      Age2 %in% 25:29         ~ 100,
      TRUE                    ~ NA_real_
    )
  )



# ---- 3.6 Assign human-readable labels ----
DEF <- DEF %>%
  mutate(
    COD = case_when(
      causa2 == 1  ~ "Homicide",
      causa2 == 2  ~ "Suicide",
      causa2 == 3  ~ "Accidents",
      causa2 == 4  ~ "Infectious diseases",
      causa2 == 5  ~ "Neoplasm",
      causa2 == 6  ~ "Circulatory system",
      causa2 == 7  ~ "Mental disorders",
      causa2 == 8  ~ "Endocrine/metabolic",
      causa2 == 9  ~ "Digestive system",
      causa2 == 10 ~ "Perinatal conditions",
      causa2 == 11 ~ "Other external causes",
      causa2 == 12 ~ "Other causes",
      TRUE         ~ NA_character_
    ),
    COD_arm = case_when(
      arma == 1 ~ "Firearms, explosives or fire",
      arma == 2 ~ "Sharp or blunt weapons",
      arma == 3 ~ "Chemicals, gases, drugs, hot objects",
      arma == 4 ~ "Hanging, drowning, force or other means",
      arma == 5 ~ "Sexual aggression",
      arma == 6 ~ "Negligence, abandonment or mistreatment",
      arma == 9 ~ "Unspecified means",
      TRUE      ~ NA_character_
    ),
    COD_lug = case_when(
      lugar == 1 ~ "Home",
      lugar == 2 ~ "Streets or highways",
      lugar == 3 ~ "Other place",
      lugar == 9 ~ "Unspecified",
      TRUE       ~ NA_character_
    ),
    # Flag for violent deaths (PRESUNTO codes 1, 2, 3, 5)
    violenta = if_else(PRESUNTO %in% c(1, 2, 3, 5), 1L, 0L)
  )



### Filter only homicides
DEFUNCIONES = DEF %>% filter(violenta == 1)
DEFUNCIONES = DEFUNCIONES %>% filter(COD == "Homicide")


### Re-coding sex variable
DEFUNCIONES[DEFUNCIONES$SEXO == 1, "SEXO"] = "H"
DEFUNCIONES[DEFUNCIONES$SEXO == 2, "SEXO"] = "F"
DEFUNCIONES[DEFUNCIONES$SEXO == 9, "SEXO"] = "Desconocido"



### ADD Mexican states names  ---------------------------------------------------

# Read catalog with Mexican states names
Estados = rio::import("geostatistics-clustering-mexico/data/raw/Mapa/ESTATAL.dbf")

# Prepare key join for table DEFUNCIONES
DEFUNCIONES = (DEFUNCIONES %>% mutate(ENT_OCURR = case_when(
  nchar(ENT_OCURR) == 1 ~ paste0("0",ENT_OCURR),
  nchar(ENT_OCURR) == 2 ~ as.character(ENT_OCURR))))

Hom_ = left_join(DEFUNCIONES, Estados, by = c("ENT_OCURR" = "Clave.de.AG"))


# Use regular expressions to avoid accents in Mexican states names 
Hom_$Nombre.de.A =str_replace_all(Hom_$Nombre.de.A,"M.XICO", "MEXICO")
Hom_$Nombre.de.A = str_replace_all(Hom_$Nombre.de.A,"MICHOAC.N DE OCAMPO", "MICHOACAN")
Hom_$Nombre.de.A = str_replace_all(Hom_$Nombre.de.A,"LE.N", "LEON")
Hom_$Nombre.de.A = str_replace_all(Hom_$Nombre.de.A,"QUER.TARO", "QUERETARO")
Hom_$Nombre.de.A = str_replace_all(Hom_$Nombre.de.A,"POTOS.", "POTOSI")
Hom_$Nombre.de.A = str_replace_all(Hom_$Nombre.de.A,"VERACRUZ DE IGNACIO DE LA LLAVE", "VERACRUZ")
Hom_$Nombre.de.A = str_replace_all(Hom_$Nombre.de.A,"YUCAT.N", "YUCATAN")
Hom_$Nombre.de.A = str_replace_all(Hom_$Nombre.de.A,"COAHUILA DE ZARAGOZA", "COAHUILA")
Hom_[Hom_$ENT_OCURR == "99", "Nombre.de.A"] = "No Especificado"
Hom_[Hom_$ENT_OCURR == "33", "Nombre.de.A"] = "No Especificado"
Hom_[Hom_$ENT_OCURR == "34", "Nombre.de.A"] = "No Especificado"









# ============================================================
# 4. FEMICIDE / HOMICIDE CLASSIFICATION 
# ============================================================


### DEFINITION OF FEMICIDE IN DATA

# Aggresor
#1 = father ,5 = son, 13 = uncle,  17 = cousin,  11= husband ,  45 = concucubine
#47 = lover,   49 = boyfriend,  51 = ex-husband
agresor = c(1, 5, 13, 17, 11,45, 47, 49, 51)

## Causes 
# Hanging, drowning, force / Chemicals, gases, drugs / Firearms, explosives, fire
# Sharp or blunt weapons /  Push from a high place / Object in movement / motor vehicle
# Physical strength / Sexual aggression / Other forms of abuse / Negligence, abandonment
causas = c("X91", "X92", "X97", "X98", "Y00", "Y01", "Y02", "Y03", "Y04","Y07")


## Filter registers who has sex UNKNOWN, just keep Female and male
Hom_filtro_sexo = (Hom_ %>% dplyr::filter(SEXO != "Desconocido")) 

## Definition of FEMICIDE
# Male Homicide = 3
# Femicide = 1
# Female Homicide = 0
Hom_Mujer = (Hom_filtro_sexo %>% dplyr::mutate(FEMI = case_when(
  SEXO == "H" ~ 3, # Male Homicide
  (substr(CAUSA_DEF,4,4) == "0") ~ 1,  # vivienda
  VIO_FAMI == 1 ~ 1,                       # violencia familiar
  substr(CAUSA_DEF,1,3) == "Y05" ~ 1, # agresion sexual
  (PAR_AGRE %in% agresor)  ~1, # agresor
  (substr(CAUSA_DEF,1,3) %in% causas) ~1 )))


# Female Homicides 
Hom_Mujer$FEMI[is.na(Hom_Mujer$FEMI)] = 0 


# Re-coding femicide variable
Hom_Mujer$FEMI[Hom_Mujer$FEMI == 1] = "Feminicidio"
Hom_Mujer$FEMI[Hom_Mujer$FEMI == 0] = "Homicidio Fem"
Hom_Mujer$FEMI[Hom_Mujer$FEMI == 3] = "Homicidio Mascu"









# ============================================================
# 5. STATE AND MUNICIPAL RATE CALCULATION
# ============================================================


# ============================================================
# 5.1 STATE RATES (SR)
# ============================================================


# ============================================================
# 5.1.1 PROJECTIONS - SR
# ============================================================
# Data base of POPULATION PROJECTIONS OF EACH STATE

Proy_Estados = read.csv("geostatistics-clustering-mexico/data/raw/Proyecciones/PROY_ESTADOS_2016.csv", sep = ",", header=T, encoding = "ISO-8859-1")

Proy_Estados$ENTIDAD =str_replace_all(Proy_Estados$ENTIDAD,"M.xico", "Mexico")
Proy_Estados$ENTIDAD = str_replace_all(Proy_Estados$ENTIDAD,"Michoac.n", "Michoacan")
Proy_Estados$ENTIDAD = str_replace_all(Proy_Estados$ENTIDAD,"Le.n", "Leon")
Proy_Estados$ENTIDAD = str_replace_all(Proy_Estados$ENTIDAD,"Quer.taro", "Queretaro")
Proy_Estados$ENTIDAD = str_replace_all(Proy_Estados$ENTIDAD,"Potos.", "Potosi")
Proy_Estados$ENTIDAD = str_replace_all(Proy_Estados$ENTIDAD,"Yucat.n", "Yucatan")


Proy_Estados$ENTIDAD = toupper(Proy_Estados$ENTIDAD)



# ============================================================
# 5.1.2 RATES COMPUTATION - SR
# ============================================================

##################################      F1    #################################
Homicidios = F1(Hom_Mujer, "Homicidios", "Estados") 
Proyecciones_E = F1(Proy_Estados, "Proyecciones", "Estados")  ####*

##################################      F2    #################################
# Crude Rates
TasasE_1 = F2(Homicidios, "Estado", Proyecciones_E)

##################################      F3    #################################
# Weights 
# BD: is the dataset generated by F1 and N is the total population
# NOTE: BD is the population, NOT homicides
#a = Proy_Estados %>% summarise(sum(POBLACION))

ponderadores_Es = F3(Proyecciones_E)

sum(ponderadores_Es$Ci)

##################################      F4    #################################
####### STANDARDIZED HOMICIDE RATE BY STATE ###################################
# BD: dataset generated by F2, since it contains the Mi values
# unidad: "Estados" or "Municipios"

ESTANDARIZACION_Estados = F4(TasasE_1, ponderadores_Es, "Estados")




# ============================================================
# 5.1.3 EXPLORATORY DATA ANALYSIS — State Rates ----
# ------------------------------------------------------------
# Purpose: visual inspection of computed state-level rates 
# before analytical interpretation. Includes crude vs. 
# standardized rate comparison, distribution analysis 
# (histogram, boxplot), and top-N state rankings by category:
# femicide, female homicide, and male homicide.
# ============================================================

# ---- 5.1.3.1 Crude Rates (Mi) — Aggregated View ---------------
# Bar chart summing crude Mi values across states, grouped by 
# homicide category (FEMI). Useful as a first look before 
# standardization is applied.
ggplot(TasasE_1) + 
  aes(x = FEMI, weight = (Mi)) +
  geom_bar(fill = "#bd3786") +
  theme_minimal() + 
  labs(y = "Tasa Homicidios estandarizada", title = "Tasas estatales Estandarizadas", subtitle = "INEGI, CONAPO 2014-2018", x = "")

# ---- 5.1.3.2 Distribution Diagnostics on Crude Rates (Mi) -----
# Histogram: overall shape and skewness of the crude rate 
# distribution across state–category observations.
hist(TasasE_1$Mi)
# Boxplot: crude rate dispersion by homicide category, 
# revealing relative spread and potential outliers per group.
boxplot(TasasE_1$Mi ~ TasasE_1$FEMI)


# ---- 5.1.3.3 Standardized State Rates — Aggregated View -------
# Same aggregated bar chart, now using age-standardized rates 
# (per 10,000 inhabitants) to enable comparability across 
# states with different age structures.
ggplot(ESTANDARIZACION_Estados) +
  aes(x = FEMI, weight = (Tasa.Homicidios)*10000) +
  geom_bar(fill = "#bd3786") +
  theme_minimal() + 
  labs(y = "Tasa Homicidios estandarizada", title = "Tasas estatales Estandarizadas", subtitle = "INEGI, CONAPO 2014-2018", x = "") 



# ---- 5.1.3.4 Standardized Rates by State — Detailed View ------
# Boxplot showing standardized homicide rates per 10,000 
# inhabitants across all 32 states, disaggregated by category 
# (FEMI). Enables visual comparison of state-level variability 
# within each homicide type.
ggplot(ESTANDARIZACION_Estados) +
  aes(x = ENTIDAD, y = Tasa.Homicidios*10000, fill = FEMI) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal() + 
  theme(axis.text.x=element_text(size=5, 
                                 angle = 90,
                                 vjust=.55),  # X axis text
        axis.text.y=element_text(size=7)) +  # Y axis text  
  labs(y = "Número de Homicidios por cada 10 mil habitantes", title = "Tasa de Homicidios Estandarizada", subtitle = "2018", x = "Sexo") 












# ============================================================
# 5.2 MUNICIPAL RATES (MR)
# ============================================================


# ============================================================
# 5.2.1 PROJECTIONS (MR)
# ============================================================
# Data Source: https://www.gob.mx/conapo/articulos/reconstruccion-y-proyecciones-de-la-poblacion-de-los-municipios-de-mexico


# Mid-year municipal projections were downloaded; they will be adapted 
# to the Proyecciones_Municipios structure previously used
# MID-YEAR MUNICIPAL POPULATION PROJECTIONS
Proyecciones_Mun = read.xlsx("geostatistics-clustering-mexico/data/raw/Proyecciones/1_Grupo_Quinq_00_RM.xlsx", sheet = "Sheet 1")
head(Proyecciones_Mun)


# Change 6th column name
names(Proyecciones_Mun)[6] = "ANIO"
# Use mid-year 2016 population
Proyecciones_Mun = (Proyecciones_Mun %>% filter(ANIO == 2016))

# Drop totals column
Proyecciones_Mun = (Proyecciones_Mun %>% dplyr::select(-POB_TOTAL))

# Reshape so that 5-year age groups become categories (long format)
Proy_Muni = melt(Proyecciones_Mun, id.vars = c("CLAVE", "CLAVE_ENT", "NOM_ENT",  "NOM_MUN", "SEXO", "ANIO"),
variable.name = "Edad_quin")
head(Proy_Muni)                                         
     
# Relabel 5-year age groups to match downstream naming convention
Proy_Muni = (Proy_Muni %>% dplyr::mutate(EDAD_QUIN = case_when(
                                           Edad_quin == "POB_00_04" ~ "pobm_00_04",
                                         Edad_quin == "POB_05_09" ~ "pobm_05_09",
                                         Edad_quin == "POB_10_14" ~ "pobm_10_14",
                                         Edad_quin == "POB_15_19" ~ "pobm_15_19",
                                         Edad_quin == "POB_20_24" ~ "pobm_20_24",
                                         Edad_quin == "POB_25_29" ~ "pobm_25_29",
                                         Edad_quin == "POB_30_34" ~ "pobm_30_34",
                                         Edad_quin == "POB_35_39" ~ "pobm_35_39",
                                         Edad_quin == "POB_40_44" ~ "pobm_40_44",
                                         Edad_quin == "POB_45_49" ~ "pobm_45_49",
                                         Edad_quin == "POB_50_54" ~ "pobm_50_54",
                                         Edad_quin == "POB_55_59" ~ "pobm_55_59",
                                         Edad_quin == "POB_60_64" ~ "pobm_60_64",
                                         Edad_quin == "POB_65_69" ~ "pobm_65_mm",
                                         Edad_quin == "POB_70_74" ~ "pobm_65_mm", 
                                         Edad_quin == "POB_75_79" ~ "pobm_65_mm",
                                         Edad_quin == "POB_80_84" ~ "pobm_65_mm",
                                         Edad_quin == "POB_85_mm" ~ "pobm_65_mm")))

# Drop the intermediate column no longer needed
Proy_Muni = (Proy_Muni %>% dplyr::select(-Edad_quin))
# Rename columns
names(Proy_Muni)[7] = "POBLACION"
names(Proy_Muni)[4] = "MUN"
names(Proy_Muni)[3] = "ENTIDAD"
names(Proy_Muni)[7] 


# Uppercase state names
Proy_Muni$ENTIDAD = toupper(Proy_Muni$ENTIDAD)
head(Proy_Muni)                                         


# Re-label variable SEXO
Proy_Muni$SEXO[Proy_Muni$SEXO == "HOMBRES"] = "Hombres" 
Proy_Muni$SEXO[Proy_Muni$SEXO == "MUJERES"] = "Mujeres" 


# ============================================================
# 5.2.2 RATES COMPUTATION (MR)
# ============================================================

##################################      F1    #################################

# Standardize variable CLAVE : ID of municipals
Hom_Mujer = (Hom_Mujer %>% 
               dplyr::mutate(MUN = case_when(
                 nchar(MUN_OCURR) == 1 ~ paste0("00",MUN_OCURR),
                 nchar(MUN_OCURR) == 2 ~ paste0("0",MUN_OCURR),
                 nchar(MUN_OCURR) == 3 ~ as.character(MUN_OCURR))))

Hom_Mujer = (Hom_Mujer %>% dplyr::mutate(CLAVE = paste0(ENT_OCURR, MUN)))
Hom_Mujer$CLAVE = as.numeric(Hom_Mujer$CLAVE)

# dataset: Hom_Mujer
Homicidos = F1(Hom_Mujer, "Homicidios", "Municipios") 
Proyecciones_M = F1(Proy_Muni, "Proyecciones", "Municipios") 


##################################      F2    #################################
# 1. Count of Municipalities or States (as specified), an intermediate step to obtain the rates
# 2. Append with Population Projections (Proy_Estados)
# 3. Compute Mi = Nxhomicides / Nxprojection
# base: dataset generated by F1 (homicides)
# unidad: "Estado" or "Municipio"
# Proyecciones: dataset generated by F1 (projections)
TasasM_1 = F2(Homicidos, "Municipio", Proyecciones_M)


##################################      F3    #################################
# Weights 
# BD: dataset generated by F1 and N is the total population
# NOTE: BD is the population, NOT homicides

ponderadores_Mu = F3(Proyecciones_M)

##################################      F4    #################################
####### STANDARDIZED HOMICIDE RATE BY MUNICIPALITY ########################################################
# BD: dataset generated by F2, since it contains the Mi values
# unidad: "Estados" or "Municipios"

ESTANDARIZACION_Municipio = F4(TasasM_1, ponderadores_Mu, "Municipios")
head(ESTANDARIZACION_Municipio)

# ============================================================
# ADD GEOGRAPHIC ELEMENT (shapefiles)
# ------------------------------------------------------------
# Source: INEGI, Marco Geoestadístico, September 2019
# ============================================================

# State boundaries (used as reference lines on the map)
estados = st_read("geostatistics-clustering-mexico/data/raw/Mapa/areas_geoestadisticas_estatales.shp",
                  stringsAsFactors = FALSE)

# Municipal boundaries
mun_ = st_read("geostatistics-clustering-mexico/data/raw/Mapa/MUNICIPIOS.shp", stringsAsFactors = FALSE)

head(mun_)

# CLAVE MUNICIPIO a ESTANDARIZACION_Municipio
# Coordenadas
mun_ = (mun_ %>% dplyr::mutate(CLAVE = paste0(CVE_ENT, CVE_MUN)))
c = (mun_ %>% dplyr::mutate(SEXO = "Mujeres", FEMI = "Homicidio Fem"))
d = (mun_ %>% dplyr::mutate(SEXO = "Hombres", FEMI = "Homicidio Mascu"))
e = (mun_ %>% dplyr::mutate(SEXO = "Mujeres", FEMI = "Feminicidio"))
MUN_ = rbind(c, d, e)
MUN_$CLAVE = as.double(MUN_$CLAVE)


# Join
Municpio_Polyg = left_join(MUN_, ESTANDARIZACION_Municipio, by=c("CLAVE" = "CLAVE","SEXO" = "SEXO", "FEMI" = "FEMI")) 



# ============================================================
# 5.2.3 EXPLORATORY DATA ANALYSIS (MR)
# ------------------------------------------------------------
# Purpose: a brief visual inspection of computed municipal-level
# rates before analytical interpretation. Includes only 
# distribution analysis (histogram, boxplot): with and without 
# male homicide.
# ============================================================



# Set margins to a very small size (bottom, left, top, right)
par(mar = c(2, 2, 2, 2))

# Histogram: overall shape and skewness of the standar rate 
# distribution across municipality–category observations.
hist(ESTANDARIZACION_Municipio$Tasa.Homicidios)
# Boxplot: standard rate dispersion by homicide category, 
# revealing relative spread and potential outliers per group.
boxplot(ESTANDARIZACION_Municipio$Tasa.Homicidios ~ ESTANDARIZACION_Municipio$FEMI)

# Histogram & Boxplot WITHOUT Male Homicides
Fem_boxplot = (ESTANDARIZACION_Municipio %>% dplyr::filter(FEMI != "Homicidio Mascu"))
boxplot(Fem_boxplot$Tasa.Homicidios ~ Fem_boxplot$FEMI)



# ============================================================
# 6. MAP: STANDARDIZED FEMICIDE RATES BY MUNICIPALITY
# ------------------------------------------------------------
# Purpose: visualization of municipal femicides 
# ============================================================

# ---- 6.1 Rate transformation ----
# Standardized rates are very small numbers (deaths per inhabitant).
# Multiplying by 10,000 expresses them as femicides per 10,000
# inhabitants, which makes differences between municipalities
# easier to read and compare.
# NOTE: Municpio_Polyg = Yi    ->   Wi = Yi*10000
Mun_Pol_Wi = (Municpio_Polyg %>% dplyr::mutate(Wi = Tasa.Homicidios*10000))


# ---- 6.2 Rate categories ----
# Rates are grouped into 6 categories for the map legend.
# Municipalities with no registered femicides are assigned to "0".
# Colors go from white (0) to dark red (> 15) to highlight
# the municipalities with the highest rates.
Mun_Pol_Wi$Categorias = cut(Mun_Pol_Wi$Wi, breaks=c(0,0.0000001,2,5,8,15,Inf), labels = c("0","> 0 - 2", "2 - 5", "5 - 8","8- 15", "> 15"))
Mun_Pol_Wi$Categorias[is.na(Mun_Pol_Wi$Categorias)] = 0

fill_rank <- c("white", "#fee0d2", "#fc9272", "#fb6a4a", "#ef3b2c", "#99000d") 



# ---- 6.3 Map ----
# Municipal polygons are filled by rate category, with state
# boundaries drawn on top as a reference. The map is exported
# to outputs/maps/
Mun_Pol_Wi %>% filter(FEMI == "Feminicidio") %>%
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = Categorias), colour=alpha("white", 1 / 10)) +
  geom_sf(data=estados, fill=NA, lwd=0.15, color="black") +
  scale_fill_manual(values=fill_rank)+  
  labs(title ="Femicides in Mexico", subtitle="Standard Rates in 2014 - 2018", y = "", 
       x = "", color = "",  fill = "Rate per 10K  inhabitants",
       caption ="Source: INEGI - Mortality Statistics and CONAPO - Mid-year (2016) population projections") +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = "bottom",
        plot.caption = element_text(size = 8))

ggsave("outputs", "maps", "Femicide_standard_rates_2014_2018.png", dpi = 300)



