options(stringsAsFactors = FALSE)
library(dplyr)
library(jsonlite)
library(leaflet)
library(tictoc)
library(shiny)
library(spdplyr)
library(reshape)
library(lubridate)

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
    
    # Analizamos contagiados 
    aux = contagiados %>% 
          select(FECHA_RESULTADO,DEPARTAMENTO,SEXO,EDAD) %>%
          mutate(FECHA_RESULTADO=as.Date(FECHA_RESULTADO,format='%d/%m/%Y'),
                 SEXO=ifelse(SEXO=='','POR DETERMINAR',SEXO),
                 DIA=weekdays(FECHA_RESULTADO)) %>% 
                 filter(DEPARTAMENTO %in% depart_selecc)
    
    resumen = aux %>% group_by(DEPARTAMENTO,SEXO) %>% 
              summarise(Infectados=n()) %>% ungroup() %>%
              cast(DEPARTAMENTO~SEXO,fill=0) %>% mutate(TOTAL=MASCULINO+FEMENINO+`POR DETERMINAR`)
    
    resumen_dia = aux %>% group_by(DEPARTAMENTO,FECHA_RESULTADO) %>%
                  summarise(Infectados=n()) %>% top_n(1,Infectados) %>% select(-Infectados) %>%
                  dplyr::rename("PICO"="FECHA_RESULTADO")
    
    resumen_dia_semana =  aux %>% group_by(DEPARTAMENTO,DIA) %>%
                          summarise(Infectados=n()) %>% top_n(2,Infectados) %>%
                          mutate(DIAS_MAS_INFECTADOS=paste0(DIA,collapse=',')) %>% ungroup() %>%
                          distinct(DEPARTAMENTO,DIAS_MAS_INFECTADOS) 
    
    resumen_ayer =  aux %>% filter(FECHA_RESULTADO==Sys.Date()-days(1)) %>% group_by(DEPARTAMENTO) %>%
                    summarise(Infectados=n()) %>% ungroup() %>%
                    dplyr::rename("INFECTADOS_AYER"="Infectados")
    
    dia_semana = ifelse(wday(Sys.Date())-1==0,7,wday(Sys.Date())-1)
    semana_pasada = seq(Sys.Date()-dia_semana-6,Sys.Date()-dia_semana,by="days")
    
    resumen_semana_pasada = aux %>% filter(FECHA_RESULTADO %in% semana_pasada) %>% group_by(DEPARTAMENTO) %>%
                            summarise(Infectados=n()) %>% ungroup() %>%
                            dplyr::rename("INFECTADOS_SEMANA_PASADA"="Infectados")
    
    resumen_cons =  resumen %>% 
                    left_join(resumen_dia) %>% 
                    left_join(resumen_dia_semana) %>%
                    left_join(resumen_ayer) %>%
                    left_join(resumen_semana_pasada)
    
    
    # Analizamos fallecidos
    aux2 =  fallecidos %>% 
            select(FECHA_FALLECIMIENTO,DEPARTAMENTO,SEXO,EDAD_DECLARADA) %>%
            mutate(FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,format='%d/%m/%Y'),
                   DIA=weekdays(FECHA_FALLECIMIENTO)) %>% 
            filter(DEPARTAMENTO %in% depart_selecc)
    
    resumen_rip = aux2 %>% mutate(SEXO = paste0(SEXO,"_F")) %>% group_by(DEPARTAMENTO,SEXO) %>% 
                  summarise(Fallecidos=n()) %>% ungroup() %>%
                  cast(DEPARTAMENTO~SEXO,fill=0) %>% 
                  mutate(TOTAL_F= rowSums(.[grep("_F", names(.))], na.rm = TRUE))
    
    dia_semana = ifelse(wday(Sys.Date())-1==0,7,wday(Sys.Date())-1)
    semana_pasada = seq(Sys.Date()-dia_semana-6,Sys.Date()-dia_semana,by="days")
    
    resumen_rip_semana_pasada = aux2 %>% filter(FECHA_FALLECIMIENTO %in% semana_pasada) %>% group_by(DEPARTAMENTO) %>%
                                summarise(Fallecidos=n()) %>% ungroup() %>%
                                dplyr::rename("FALLECIDOS_SEMANA_PASADA"="Fallecidos")
    
    resumen_rip_cons = resumen_rip %>% left_join(resumen_rip_semana_pasada)
    
    # Consolidamos info.
    
    metricas = resumen_cons  %>% left_join(resumen_rip_cons)
    
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
  
  })  
    
  
  
}

shinyApp(ui = ui, server = server)




pal = colorNumeric("viridis", NULL)

leaflet(data) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, color='black',weight = 0.3,
              smoothFactor = 0.6, fillOpacity = 0.6,
              fillColor = ~pal(log10(AREA_MINAM)),
              label = ~paste0(NOMBDIST, ": ", AREA_MINAM)) %>%
  addLegend(pal = pal, values = ~log10(AREA_MINAM), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))
