
source("../Codigos/pre-procesamiento.R")
source("../Codigos/post_seleccion_vars.R")
# indicadores=indicadores |>
#   dplyr::mutate(dplyr::across(GINI:pres_eje_seg_mun, ~tidyr::replace_na(., 0)))
pca_indicadores_post=prcomp(indicadores[,-1], scale = TRUE,retx = T)
summary(pca_indicadores_post)
###Con 9 componentes se tiene para cubrir >70%


library(ggcorrplot)
ggcorrplot::ggcorrplot(corr = round(cor(indicadores[,-1]),2),type = "lower"
                       ,show.diag = F,lab = T,lab_size = 1.4,
                       title = "Matriz de Correlación")


var_explicada <- pca_indicadores_post$sdev^2
# Calculamos el porcentaje de varianza
proporcion_varianza <- var_explicada / sum(var_explicada)

# Creamos el data frame para la visualización
data_varianza <- data.frame(
  PC = 1:length(proporcion_varianza),
  Varianza = proporcion_varianza
)

library(ggplot2)
ggplot(data_varianza, aes(x = PC, y = Varianza)) +
  geom_bar(stat = "identity", fill = "#F5DABC") +
  geom_text(aes(label = paste0(round(Varianza * 100), "%")),
            vjust = -0.5, size = 3) +
  labs(x = "Componente Principal", y = "Proporción de Varianza Explicada") +
  theme_minimal() +
  ggtitle("Varianza Explicada por Componente Principal")











pca_indicadores_post$rotation[pca_indicadores_post$rotation[,1] |> abs() |> order(decreasing = T),1] |> abs() |> plot()
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,1] |> abs() |> order(decreasing = T),1]##Cortamos a 2
####-----------
# ingresos,  viviendas sin internet, marginacion, personas sin educacion basica, 
#ingresos menores a la linea de pobreza, 
#poblacion desocupada, pib estatal, 
#Resumen: pobreza por ingresos. 
##Componente 2: 
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,2] |> abs() |> order(decreasing = T),2] |> abs()|> plot()
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,2] |> abs() |> order(decreasing = T),2]#Cortamos a 8
#slice_top5(3)
# + carencia alimentaria, - poco personal de seguridad, - alumbrado publico, 
# - pob discapacitada,  
#carencia acceso vivienda, baja inversion a carreteras, poblacion inactiva,tiendas liconsa, mucha enf_intestinales.
#Resumen: Medidad de seguridad y carencia alimentaria. 
##Componente 3: 
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,3] |> abs() |> order(decreasing = T),3] |> abs()|> plot()
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,3] |> abs() |> order(decreasing = T),3]
#+ presencia de delitos, + participacion al pib, +inversion en escuelas, + poblacion desocupada
#- poblacion sin derechohabiencia
#, poca poblacion desocupada, pocas personas inactivas,
#Poco personal de seguridad, muchas superficies siniestradas, baja inversion en carreteras
#Resumen. Baja presencia de delitos, más participacion al pib
##Componente 4: 
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,4] |> abs() |> order(decreasing = T),4]|> abs()|> plot()
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,4] |> abs() |> order(decreasing = T),4] #Cortamos a 4
## - kilometros de carreteras, + unidades economicas, - areas verdes , + superficies de cosecha siniestradas

#Resumen. 
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,5] |> abs() |> order(decreasing = T),5]|> abs()|> plot()
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,5] |> abs() |> order(decreasing = T),5] #Cortamos a 4
## - kilometros de carreteras, + unidades economicas, - areas verdes , + superficies de cosecha siniestradas
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,6] |> abs() |> order(decreasing = T),6]|> abs()|> plot()
pca_indicadores_post$rotation[pca_indicadores_post$rotation[,6] |> abs() |> order(decreasing = T),6] #Cortamos a 4
## - kilometros de carreteras, + unidades economicas, - areas verdes , + superficies de cosecha siniestradas

#Resumen. 

factoextra::fviz_pca_var(pca_indicadores_post,
                         axes = 1:2,
                         fill.var = "contrib",
                         alpha.var = "contrib",
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,
                         select.var = list(contrib = 10))
# Gráfico 2: Solo las 10 variables principales
factoextra::fviz_pca_var(pca_indicadores_post,
                         axes = 3:4,
                         fill.var = "contrib",
                         alpha.var = "contrib",
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,
                         select.var = list(contrib = 10))
