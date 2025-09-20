###Hacmos el pca con las variables seleccionadas

pca_indicadores_post=prcomp(indicadores[,-1], scale = TRUE,retx = T)
summary(pca_indicadores_post)
###Con 9 componentes se tiene para cubrir >70%
 
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
slice_top5(3)
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



library(factoextra)
factoextra::fviz_pca_var(pca_indicadores_full,axes = 1:2, alpha.var = "contrib",
                         col.var = "contrib", 
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE)
