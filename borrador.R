fx_metricas =  function(tipo,filtro){

  dia_semana = ifelse(wday(Sys.Date())-1==0,7,wday(Sys.Date())-1)
  semana_pasada = seq(Sys.Date()-dia_semana-6,Sys.Date()-dia_semana,by="days")
  contagiados_clean = contagiados %>%
                      select(FECHA_RESULTADO,DEPARTAMENTO,PROVINCIA,DISTRITO,EDAD,SEXO) %>%
                      mutate(FECHA_RESULTADO=as.Date(FECHA_RESULTADO,format='%d/%m/%Y'),
                             SEXO=ifelse(SEXO=='','POR DETERMINAR',SEXO),
                             PROVINCIA=ifelse(grepl('EN INVESTIGACIÓN',PROVINCIA),NA,PROVINCIA),
                             DISTRITO=ifelse(grepl('EN INVESTIGACIÓN',DISTRITO),NA,DISTRITO),
                             DIA=weekdays(FECHA_RESULTADO))
  fallecidos_clean = fallecidos %>%
                     select(FECHA_FALLECIMIENTO,DEPARTAMENTO,PROVINCIA,DISTRITO,EDAD_DECLARADA,SEXO) %>%
                     mutate(FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,format='%d/%m/%Y'),
                            PROVINCIA=ifelse(PROVINCIA=="",NA,PROVINCIA),
                            DISTRITO=ifelse(DISTRITO=="",NA,toupper(DISTRITO)))
  
  
if(tipo=="Dep"){
  
  # Analizamos contagiados 
  contagiados_filtro = contagiados_clean %>% 
                       select(FECHA_RESULTADO,DEPARTAMENTO,SEXO,EDAD) %>%
                       filter(DEPARTAMENTO %in% filtro)
                  
  resumen = contagiados_filtro %>% group_by(DEPARTAMENTO,SEXO) %>% 
            summarise(Infectados=n()) %>% ungroup() %>%
            cast(DEPARTAMENTO~SEXO,fill=0) %>% mutate(TOTAL=MASCULINO+FEMENINO+`POR DETERMINAR`)
  
  resumen_dia = contagiados_filtro %>% group_by(DEPARTAMENTO,FECHA_RESULTADO) %>%
                summarise(Infectados=n()) %>% top_n(1,Infectados) %>% select(-Infectados) %>%
                dplyr::rename("PICO_INF"="FECHA_RESULTADO")
  
  resumen_dia_semana =  contagiados_filtro %>% group_by(DEPARTAMENTO,DIA) %>%
                        summarise(Infectados=n()) %>% top_n(2,Infectados) %>%
                        mutate(DIAS_MAS_INFECTADOS=paste0(DIA,collapse=',')) %>% ungroup() %>%
                        distinct(DEPARTAMENTO,DIAS_MAS_INFECTADOS) 
  
  resumen_ayer =  contagiados_filtro %>% filter(FECHA_RESULTADO==Sys.Date()-days(1)) %>% 
                  group_by(DEPARTAMENTO) %>%
                  summarise(Infectados=n()) %>% ungroup() %>%
                  dplyr::rename("INFECTADOS_AYER"="Infectados")
  
  resumen_semana_pasada = contagiados_filtro %>% filter(FECHA_RESULTADO %in% semana_pasada) %>% 
                          group_by(DEPARTAMENTO) %>%
                          summarise(Infectados=n()) %>% ungroup() %>%
                          dplyr::rename("INFECTADOS_SEMANA_PASADA"="Infectados")
  
  resumen_cons =  resumen %>% 
                  left_join(resumen_dia) %>% 
                  left_join(resumen_dia_semana) %>%
                  left_join(resumen_ayer) %>%
                  left_join(resumen_semana_pasada)
  
  # Analizamos fallecidos
  fallecidos_filtro =   fallecidos_clean %>% 
                        select(FECHA_FALLECIMIENTO,DEPARTAMENTO,SEXO,EDAD_DECLARADA) %>%
                        filter(DEPARTAMENTO %in% filtro)
  
  resumen_rip = fallecidos_filtro %>% mutate(SEXO = paste0(SEXO,"_F")) %>% group_by(DEPARTAMENTO,SEXO) %>% 
                summarise(Fallecidos=n()) %>% ungroup() %>%
                cast(DEPARTAMENTO~SEXO,fill=0) %>% 
                mutate(TOTAL_F= rowSums(.[grep("_F", names(.))], na.rm = TRUE))
  
  resumen_rip_semana_pasada = fallecidos_filtro %>% filter(FECHA_FALLECIMIENTO %in% semana_pasada) %>% 
                              group_by(DEPARTAMENTO) %>% summarise(Fallecidos=n()) %>% ungroup() %>%
                              dplyr::rename("FALLECIDOS_SEMANA_PASADA"="Fallecidos")
  
  resumen_rip_cons = resumen_rip %>% 
                     left_join(resumen_rip_semana_pasada)
  
  # Consolidamos info.
  metricas = resumen_cons  %>% left_join(resumen_rip_cons)
  return(metricas)
  
}
  
if(tipo=="Prov"){
  filtro="LIMA"
  contagiados_filtro =  contagiados_clean %>% 
                        select(FECHA_RESULTADO,PROVINCIA,SEXO,EDAD) %>%
                        filter(PROVINCIA %in% filtro)
}
  
  
  
}

a = fx_metricas("Dep",c("LIMA"))


bla = data_provincial[data_provincial$NOMBPROV %in% "LIMA",]
lista= list()
for(i in 1:length(bla)){
  lista[[i]]=bla@polygons[[i]]@Polygons[[1]]@coords %>% as.data.frame() 
}

df_lista = do.call(rbind,lista) %>% as.matrix()
vertices = chull(df_lista)
df_final = df_lista[vertices,]
library(sp)
p = Polygon(df_final)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
data = data.frame(f=99.9)
spdf = SpatialPolygonsDataFrame(sps,data)

library(alphahull)

leaflet() %>%
  addTiles()%>%
  addPolygons(data=spdf,stroke=FALSE,
              highlight = highlightOptions(bringToFront = TRUE,
                                           opacity=10,
                                           color="red",
                                           weight=3))

X <- matrix(rnorm(2000), ncol = 2)
plot(X, cex = 0.5)
hpts <- chull(X)
hpts <- c(hpts, hpts[1])
lines(X[hpts, ])
