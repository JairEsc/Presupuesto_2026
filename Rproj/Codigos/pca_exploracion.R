##PCA

#Supongamos dada la consulta 
##84*(K+1). Datos a nivel municipal de las K variables seleccionadas

library(stats)
library(FactoMineR)
library(factoextra)
library(sf)
source("../Codigos/pre-procesamiento.R")

##Exploramos correlaciones
library(ggcorrplot)
ggcorrplot::ggcorrplot(corr = cor(indicadores[,-1]),type = "lower",show.diag = F,lab = T)
indicadores=indicadores |> dplyr::select(-p_disp_seg,-ind_ingreso,-PORC_POB_IILP,pje_pob_15ymas_edbasinc_2020)
matriz_corr=cor(indicadores[,-1])
matrix(matriz_corr |> sapply(\(z){
  abs(z)>0.8
}),nrow = 34,ncol = 34,byrow = T) |> colSums()-1

##Hacemos una rutina de limpieza como sigue: 
##Calcula el máximo de correlaciones mayores a 0.8 para las variables.
##E.g. max=4, 
  #0 0 4 3 0 0 1 0 0 0 3 1 0 4 0 0 3 0 3 0 0 0 0 0 0 0 0 3 3 0
##De cada una eliminamos la que consideremos cubierta
which(((matrix(matriz_corr |> sapply(\(z){
  abs(z)>0.8
}),nrow = 34,ncol = 34,byrow = T) |> colSums())-1)>0##4 es el máximo
)
#E.g. de la col 3 y 14 que alcanzan el máximo
(indicadores[,-1] |> colnames())[c(3 , 4 , 8, 12, 13, 15, 19, 21, 31)]
#[1] "ind_ingreso" "Pob_des"  
#Vemos de sus correlaciones altas, cuáles ya están incluidas. 
colnames(indicadores[,-1])[(matrix(matriz_corr |> sapply(\(z){
  abs(z)>0.8
}),nrow = 30,ncol = 30,byrow = T)[3,] |> which())]

## ind_ingreso está relacionada con 
caret::findCorrelation(x = matriz_corr, cutoff = 0.8)

indicadores |> openxlsx::write.xlsx("../Datos/municipal_34_indicadores.xlsx")
pca_indicadores=prcomp(indicadores[,-1], scale = TRUE)
summary(pca_indicadores)
pca_indicadores$rotation

####Con la otra librería 
factoextra::fviz_pca_var(pca_indicadores, col.var = "black")


# "../../../Espacios_públicos/Datos/Espacios Publicos/Espacios Publicos/Plazas_Parques_Jardines.shp" |> read_sf() |> st_geometry() |> plot()
# "../../../Espacios_públicos/Datos/Espacios Publicos/Espacios Publicos/Parques_Jardines_Poligonos.shp" |> read_sf() |> st_geometry() |> plot()

