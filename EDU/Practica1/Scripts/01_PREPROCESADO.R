
# Librerias ---------------------------------------------------------------

# install.packages("dplyr")
# install.packages("data.table")
# install.packages("readx1")
# install.packages("esquisse")
# install.packages("plotly")
library(esquisse)
library(plotly)

library(dplyr)
library(data.table)
library(readxl)

# Carga de datos  ---------------------------------------------------------
datos <- fread(input = "Data/CNA2014_ENCABEZADO_15.csv", sep = ",") %>% 
  select(COD_VEREDA,TIPO_UC,S05_TENENCIA,P_S5PAUTOS,P_S7P82,P_S7P84F,P_S7P85B) %>% 
  filter(TIPO_UC == 1) %>% 
  mutate(S05_TENENCIA = as.character(S05_TENENCIA))
str(datos)
glimpse(datos)

# limpieza  ---------------------------------------------------------
t_homologacion_7 <- readxl::read_excel(
  path = "Data/Tablasdehomologacion.xlsx",
    sheet = "Hoja2") %>% 
  mutate(S05_TENENCIA = as.character(S05_TENENCIA))
str(t_homologacion_7)

datos_dep <- datos %>% 
  left_join(t_homologacion_7, by = c("S05_TENENCIA"="S05_TENENCIA")) %>% 
  select(Predominancia,P_S7P85B) %>% 
  na.omit()  #eliminar datos ausentes.
str(datos_dep)
datos_dep

 # datos_dep <- dplyr::left_join(datos, t_homologacion_7, by...)

# Tablas de distribucion de frecuencias CUALTITATIVA ----------------------

tdf_S05_TENENCIA <- datos_dep %>% 
  group_by(Predominancia) %>% 
  summarise(n_i = n()) %>%  #frecuencia absoluta acumulada 
  arrange(desc(n_i)) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i))


# Grafico -----------------------------------------------------------------

barplot(table(datos_dep$Predominancia))


# Usando esquisser --------------------------------------------------------

# esquisse::esquisser(viewer = "browser")
# grafica de barras
ggplot(datos_dep) +
  aes(x = Predominancia) +
  geom_bar(fill = "#112446") +
  labs(
    x = "predominancia",
    y = "count",
    title = "Distribucion de "
  ) +
  coord_flip() +
  theme_minimal()


# TDF - Variable Cuanti ---------------------------------------------------

# leer paquete DT, datatable 

# Numero de clases

k = round( 1 + 3.3 * log10(nrow(datos_dep)))
k
 # rango

rango = max(datos$P_S7P85B, na.rm = T) -
  min(datos$P_S7P85B, na.rm = T)
rango

# longitud
longitud = rango/k
longitud

cortes <- min(datos$P_S7P85B, na.rm = T) + c(seq(0,k,1))*longitud
cortes
k
 View(datos_dep)
View(tdf_S05_TENENCIA)

# TDF - leche
tdf_P_S7P85B <- datos_dep %>% 
  mutate(P_S7P85B_c = as.factor(cut(P_S7P85B, 
                          breaks = cortes,
                          levels = cortes,
                          include.lowest = T, #limite superior lo inclya como cerrado
                          dig.lab =6
                          ))) %>% 
  group_by(P_S7P85B_c, .drop = F, .add = F) %>% 
  summarise(n_i = n()) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i),
         x_i = cortes[1:k] + longitud/2,
         c_i = abs(cortes[1:k] -cortes[2: (k + 1)]),
         d_i = n_i/c_i)
 # :)

