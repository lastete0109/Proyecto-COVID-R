options(stringsAsFactors = FALSE)
library(dplyr)
library(rgdal)
library(leaflet)
library(tictoc)
library(shiny)
library(reshape)
library(lubridate)
library(purrr)

#setwd(r'(C:\Users\Leo\Desktop\Proyecto COVID - R)')

# Datas GeoJson
data_depart = readOGR('peru_departamental_simple.geojson')
data_provincial = readOGR('peru_distrital_simple.geojson')
data_distrital = readOGR('peru_distrital_simple.geojson')

# Informacion del MINSA
contagiados = read.csv('https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download')
fallecidos = read.csv('https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download')

# Despegables
list_depart = as.character(data_depart$NOMBDEP)
list_prov = as.character(sort(unique(data_provincial$NOMBPROV)))
list_distr = as.character(sort(unique(data_distrital$NOMBDIST)))
lista_analisis = c('Departamental','Provincial','Distrital')

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
      select(FECHA_RESULTADO,DEPARTAMENTO,SEXO,EDAD,DIA) %>%
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
    contagiados_filtro =  contagiados_clean %>% 
      select(FECHA_RESULTADO,PROVINCIA,SEXO,EDAD,DIA) %>%
      filter(PROVINCIA %in% filtro)
    
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
  
  if(tipo=="Dist"){
    contagiados_filtro =  contagiados_clean %>% 
      select(FECHA_RESULTADO,DISTRITO,SEXO,EDAD,DIA) %>%
      filter(DISTRITO %in% filtro)
    
    resumen = contagiados_filtro %>% group_by(DISTRITO,SEXO) %>% 
      summarise(Infectados=n()) %>% ungroup() %>%
      cast(DISTRITO~SEXO,fill=0) %>% mutate(TOTAL=MASCULINO+FEMENINO+`POR DETERMINAR`)
    
    resumen_dia = contagiados_filtro %>% group_by(DISTRITO,FECHA_RESULTADO) %>%
      summarise(Infectados=n()) %>% top_n(1,Infectados) %>% select(-Infectados) %>%
      dplyr::rename("PICO_INF"="FECHA_RESULTADO")
    
    resumen_dia_semana =  contagiados_filtro %>% group_by(DISTRITO,DIA) %>%
      summarise(Infectados=n()) %>% top_n(2,Infectados) %>%
      mutate(DIAS_MAS_INFECTADOS=paste0(DIA,collapse=',')) %>% ungroup() %>%
      distinct(DISTRITO,DIAS_MAS_INFECTADOS) 
    
    resumen_ayer =  contagiados_filtro %>% filter(FECHA_RESULTADO==Sys.Date()-days(1)) %>% 
      group_by(DISTRITO) %>%
      summarise(Infectados=n()) %>% ungroup() %>%
      dplyr::rename("INFECTADOS_AYER"="Infectados")
    
    resumen_semana_pasada = contagiados_filtro %>% filter(FECHA_RESULTADO %in% semana_pasada) %>% 
      group_by(DISTRITO) %>%
      summarise(Infectados=n()) %>% ungroup() %>%
      dplyr::rename("INFECTADOS_SEMANA_PASADA"="Infectados")
    
    resumen_cons =  resumen %>% 
      left_join(resumen_dia) %>% 
      left_join(resumen_dia_semana) %>%
      left_join(resumen_ayer) %>%
      left_join(resumen_semana_pasada)
    
    # Analizamos fallecidos
    fallecidos_filtro =   fallecidos_clean %>% 
      select(FECHA_FALLECIMIENTO,DISTRITO,SEXO,EDAD_DECLARADA) %>%
      filter(DISTRITO %in% filtro)
    
    resumen_rip = fallecidos_filtro %>% mutate(SEXO = paste0(SEXO,"_F")) %>% group_by(DISTRITO,SEXO) %>% 
      summarise(Fallecidos=n()) %>% ungroup() %>%
      cast(DISTRITO~SEXO,fill=0) %>% 
      mutate(TOTAL_F= rowSums(.[grep("_F", names(.))], na.rm = TRUE))
    
    resumen_rip_semana_pasada = fallecidos_filtro %>% filter(FECHA_FALLECIMIENTO %in% semana_pasada) %>% 
      group_by(DISTRITO) %>% summarise(Fallecidos=n()) %>% ungroup() %>%
      dplyr::rename("FALLECIDOS_SEMANA_PASADA"="Fallecidos")
    
    resumen_rip_cons = resumen_rip %>% 
      left_join(resumen_rip_semana_pasada)
    
    # Consolidamos info.
    metricas = resumen_cons  %>% left_join(resumen_rip_cons)
    return(metricas)
    
    
  }
  
}

# Interfaz de usuario
ui <- fluidPage(
    navbarPage("Evolución de Coronavirus", id="naveg",
               tabPanel("Mapa interactivo",
                        div(class='outer',
                            tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                            leafletOutput("mapa",width = "100%", height = "100%"),
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          h4('Filtros de analisis',align='center'),
                                          
                                          selectizeInput('Analisis','Tipo de analisis',lista_analisis,
                                                         options = list( placeholder = 'Seleccione el tipo de analisis',
                                                                        onInitialize = I('function() { this.setValue(""); }'))),
                                          
                                          conditionalPanel("input.Analisis !='' ",
                                                           selectizeInput('Departamento','Departamento',list_depart,multiple=T,
                                                           options = list( placeholder = 'Seleccione el/los departamentos',
                                                                          onInitialize = I('function() { this.setValue(""); }')))),
                                          
                                          conditionalPanel(" input.Departamento!= '' &
                                                           (input.Analisis =='Provincial' | input.Analisis =='Distrital')",
                                                           selectizeInput('Provincia','Provincia',list_prov,multiple=T,
                                                           options = list( placeholder = 'Seleccione la/las provincias',
                                                                           onInitialize = I('function() { this.setValue(""); }')))),
                                          
                                          conditionalPanel(" input.Provincia !='' & input.Analisis=='Distrital'",
                                                           selectizeInput('Distrito','Distrito',list_distr,multiple=T,
                                                           options = list( placeholder = 'Seleccione la/las provincias',
                                                                           onInitialize = I('function() { this.setValue(""); }')))),
                            )
                            ))
               ))


# Codigo de servidor
server <- function(input, output,session){
  
output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles(attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView( lng = -75.648748 , lat= -9.802423 ,zoom=5)
    })
  
observe({
  req(input$Analisis)
  analisis = input$Analisis
  
  if(analisis=='Departamental'){
    req(input$Departamento)
    
    depart_selecc = input$Departamento
    data_selecc = data_depart[data_depart$NOMBDEP %in% depart_selecc,]
    metricas = fx_metricas("Dep",depart_selecc)
    data_selecc@data =  data_selecc@data %>% left_join(metricas,by= c("NOMBDEP"="DEPARTAMENTO"))
    
    labels = sprintf(
      "<strong>%s</strong><br/>
      Infectados totales: %d<br/>
      Mujeres: %d<br/>
      Hombres: %d<br/>
      Pico de infectados: %s<br/>
      Infectados ayer: %d <br/>
      Fallecidos totales: %d <br/>",
      metricas$DEPARTAMENTO,
      metricas$TOTAL,
      metricas$FEMENINO,
      metricas$MASCULINO,
      metricas$PICO,
      metricas$INFECTADOS_AYER,
      metricas$TOTAL_F) %>% lapply(htmltools::HTML)
    
    leafletProxy("mapa") %>%
      clearShapes() %>%
      addPolygons(stroke = TRUE,
                  color='black',
                  weight = 1,
                  data=data_selecc,
                  label = labels,
                  highlight = highlightOptions(bringToFront = TRUE,
                                               color="red",
                                               weight=3),
                  labelOptions = labelOptions(
                    style = list("font-weight"="normal", 
                                 padding = "3px 8px"),
                    textsize = "12px",
                    direction = "auto"))
  }
  
  if(analisis=='Distrital'){
    req(input$Distrito)
    
    dist_selecc = input$Distrito
    data_selecc = data_distrital[data_distrital$NOMBDIST %in% dist_selecc,]
    metricas = fx_metricas("Dist",dist_selecc)
    data_selecc@data =  data_selecc@data %>% 
                        left_join(metricas,by= c("NOMBDIST"="DISTRITO"))  %>%
                        mutate(LABEL=pmap(list(a=NOMBDIST,b=TOTAL,c=FEMENINO,d=MASCULINO,
                                               e=PICO_INF,f=INFECTADOS_AYER,g=TOTAL_F),
                                    function(a,b,c,d,e,f,g){
                                    htmltools::HTML(sprintf("<strong>%s</strong><br/>
                                                             Infectados totales: %d<br/>
                                                             Mujeres: %d<br/>
                                                             Hombres: %d<br/>
                                                             Pico de infectados: %s<br/>
                                                             Infectados ayer: %d <br/>
                                                             Fallecidos totales: %d <br/>",
                                                             a,b,c,d,e,ifelse(is.na(f),0,f),g))}
                                                            ))
  
    leafletProxy("mapa") %>%
      clearShapes() %>%
      addPolygons(stroke = TRUE,
                  color='black',
                  weight = 1,
                  data=data_selecc,
                  label = ~LABEL,
                  highlight = highlightOptions(bringToFront = TRUE,
                                               color="red",
                                               weight=3),
                  labelOptions = labelOptions(
                    style = list("font-weight"="normal", 
                                 padding = "3px 8px"),
                    textsize = "12px",
                    direction = "auto"))
    
  }
  
  })  

observe({
  provincias = data_distrital@data %>% filter(NOMBDEP %in% input$Departamento) %>%
               `$`('NOMBPROV') %>% unique() %>% sort()

  stillSelected<-isolate(input$Provincia[input$Provincia %in% provincias])
  
  updateSelectizeInput(session,"Provincia",choices=provincias,
                       selected=stillSelected,server=T) })

observe({
  distritos = data_distrital@data %>% filter(NOMBPROV %in% input$Provincia) %>%
              `$`('NOMBDIST') %>% unique() %>% sort()
  
  stillSelected<-isolate(input$Distrito[input$Distrito %in% distritos])
  
  updateSelectizeInput(session,"Distrito",choices=distritos,
                       selected=stillSelected,server=T)
})

}

shinyApp(ui = ui, server = server)


