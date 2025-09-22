#"../Datos/input.xlsx" |> openxlsx::read.xlsx()->indicadores


###Iniciamos con 37 variables. De las cuales ya identificamos algunas que son altamente correlacionadas y pertenecen al mismo tema. 
"../../Inputs/base para acp.xlsx" |> openxlsx::read.xlsx()->indicadores
indicadores=indicadores |>
  dplyr::mutate(dplyr::across(GINI:pres_eje_seg_mun, ~tidyr::replace_na(., 0)))
##E.g. #indice de ingresos vs. porc. pob. con ingresos menores a linea de pobreza vs. 
#marginacion y porcentaje de personas mayores a 15 años sin educacion básica.
# porcentaje de viviendas sin accesoa  inernet. 
#Dado que muchas de ellas están involucradas en el cálculo de la marginación, la consideramos una medida resumen y la conservamos. 
indicadores=indicadores |> dplyr::select(-ind_ingreso,-PORC_POB_IILP,-pje_pob_15ymas_edbasinc_2020,-Pje_vivnodinter_2020)
indicadores=indicadores |> dplyr::select(-pib_estatal,-PORC_C_ALIM,-PCDISC_MOT,-p_disp_seg,-DRIOPAP_T,-Enf_int_xhab)


##otro conjunto de correlaciones está en 
#población desocupada. Carpetas puestas a disposición de fiscalía, y pib estatal. 
#Por otro lado, existe correlación alta entre delitos del fuero comun  carpetas ... 
#Eliminamos la de carpetas. 

matrix_corr=cor(indicadores[,-1])
