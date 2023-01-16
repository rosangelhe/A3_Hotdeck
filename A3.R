# Libreria -----------------------------------------------------------------------
library(tidyverse)
library(dplyr)

# DATOS --------------------------------------------------------------------------
personas17 <- read_sav("encovi_personas2017_ds.sav")

#VARIABLE A IMPUTAR: ingreso laboral

# El objetivo es imputar a las personas que deben tener ingresos laborales reportados, 
# pero no los tienen. Es decir, aquellos que declaran estar trabajando de forma remunerada, 
# pero no reportan ingresos laborales o que sus ingresos laborales son iguales o menores a cero
view_df(personas17)

# Para facilitar manipulacion
`%nin%` = Negate(`%in%`)

# Tabla de referencia con valores validos.
ref <- personas17 %>%
  filter(TMHP36 %in% c(1,2),
         TMHP44 %nin% c(98, 99),
         TMHP44BS %nin% c(98, 99),
         TMHP45BS %nin% c(98, 99),
         PMHP60BS %nin% c(98, 99))

# Buscamos los faltantes por data
faltantes <- which(
  personas17$TMHP36 %in% c(1,2) &
  personas17$TMHP44 %in% c(98, 99) &
  personas17$TMHP44BS %in% c(98, 99) &
  personas17$TMHP45BS %in% c(98, 99) &
  personas17$PMHP60BS %in% c(98, 99)
)

# Guardamos una referencia del original
origPersonas <- personas17
ing_laboral_imp <- personas17

# Iteramos por los registros
for (i in faltantes) {
  ref_grupo <- ref %>%
    filter(CMHP19 == personas17$CMHP19[i],
           GRPEDAD == personas17$GRPEDAD[i],
           Tciudad_max == personas17$Tciudad_max[i])
  if (nrow(ref_grupo) == 0) {
    ref_grupo <- ref
  }
  
  # Obtenemos un registro
  registro <- ref_grupo[sample(nrow(ref_grupo), 1),]
  
  # Llenamos los faltantes
  ing_laboral_imp[i, "TMHP44"] <- registro$TMHP44
  ing_laboral_imp[i, "TMHP44BS"] <- registro$TMHP44BS
  ing_laboral_imp[i, "TMHP45BS"] <- registro$TMHP45BS
  ing_laboral_imp[i, "PMHP60BS"] <- registro$PMHP60BS
}
