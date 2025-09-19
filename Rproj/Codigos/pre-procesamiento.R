
"../../Inputs/Catalogo de Variables 2025.xlsx - Tabla Variables Rosas.csv" |> read.csv()->
  indicadores
#indicadores=indicadores |> st_drop_geometry()
indicadores=indicadores |> 
  dplyr::select(c(-CVE_EST,-CVE_MUN,-NOM_EST,-PSINDER))


source("../../../../Reutilizables/Postgres_BUIG/conexion_buig.R")
viviendas_scince_2020municipal=st_read(buig,Lista_BUIG[[313]])
poblacion_scince_2020municipal=st_read(buig,Lista_BUIG[[230]])

poblacion_scince_2020municipal=poblacion_scince_2020municipal |> 
  dplyr::select(cve_mun,nomgeo,pob1) |> st_drop_geometry()
viviendas_scince_2020municipal=viviendas_scince_2020municipal |> 
  dplyr::select(cve_mun,nomgeo,viv2) |> st_drop_geometry()

indicadores=indicadores |> 
  merge(y=poblacion_scince_2020municipal |> 
          dplyr::select(nomgeo,pob1),by.x='NOM_MUN',by.y='nomgeo',all.x=T) |> 
  merge(y=viviendas_scince_2020municipal|> 
          dplyr::select(nomgeo,viv2),by.x='NOM_MUN',by.y='nomgeo')
indicadores=indicadores |> 
  dplyr::mutate(rsu_sin_rec=as.numeric(gsub(",","",rsu_sin_rec))/pob1,
                ec_co2e=1000*as.numeric(gsub(",","",ec_co2e))/pob1,
                pob_ben_beca=1000*as.numeric(gsub(",","",pob_ben_beca))/pob1,
                VPH_NODREN=100*(VPH_NODREN/viv2),
                personal_seguridad=1000*personal_seguridad/pob1,
                TOT_INV_CARR=TOT_INV_CARR/pob1,
                TOT_INV_AGUA=TOT_INV_AGUA/pob1,
                TOT_INV_ESPE=TOT_INV_ESPE/pob1,
                tot_del_fc=1000*tot_del_fc/pob1, #ni idea
                Tiendas.liconsa=1000*Tiendas.liconsa/pob1,
                SPDCD_T=as.numeric(SPDCD_T),
                t_lamp_lumi_alumb_pub=as.numeric(gsub(" ","",t_lamp_lumi_alumb_pub))/pob1,
                p_disp_seg=as.numeric(gsub(" ","",p_disp_seg)),
                pres_eje_seg_mun=as.numeric(gsub(" ","",pres_eje_seg_mun))/pob1
                )
indicadores |> lapply(class)
indicadores[,-1]= indicadores[,-1] |> lapply(\(col){
  col=gsub("%","",col) |> as.numeric()
})
##Para adimensionalizar las que faltan usamos poblacion total y número de viviendas habitadas


indicadores=indicadores |> 
  dplyr::mutate(PCDISC_MOT=round(100*(PCDISC_MOT/pob1),4),
                PE_INAC=round(100*(PE_INAC/pob1),4),
                VPH_NODREN=round(100*(VPH_NODREN/viv2),4)) 

indicadores=indicadores |> 
  dplyr::select(-pob1,-viv2)
indicadores=indicadores |> 
  dplyr::mutate(dplyr::across(GINI:pres_eje_seg_mun, ~tidyr::replace_na(., 0)))



DBI::dbDisconnect(buig)
