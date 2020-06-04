options(stringsAsFactors = FALSE,
        spinner.color="#0275D8",
        spinner.color.background="#ffffff",
        spinner.size=2)
library(dplyr)
library(rgdal)
library(leaflet)
library(tictoc)
library(shiny)
library(reshape)
library(reshape2)
library(lubridate)
library(purrr)
library(ggplot2)
library(gganimate)
library(gifski)
library(shinycssloaders)

setwd("C://Users//Leo//Desktop//Proyecto COVID - R")

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

fx_graficas = function(tipo,filtro){
  if(tipo=="Dep"){

    df_fechas = data.frame(FECHA_RESULTADO=seq(as.Date("16-03-2020","%d-%m-%Y"),Sys.Date()-1,by="days"))
    
    contagiados_plot = contagiados %>%
      select(FECHA_RESULTADO,DEPARTAMENTO,EDAD,SEXO) %>%
      mutate(FECHA_RESULTADO=as.Date(FECHA_RESULTADO,format='%d/%m/%Y'),
             SEXO=ifelse(SEXO=='','POR DETERMINAR',SEXO),
             DIA=weekdays(FECHA_RESULTADO)) %>% 
      filter(DEPARTAMENTO %in% filtro & !is.na(FECHA_RESULTADO)) %>%
      group_by(FECHA_RESULTADO) %>%
      summarise(Infectados=n())
    
    contagiados_final = contagiados_plot %>% merge(df_fechas,by="FECHA_RESULTADO",all=TRUE) %>% 
      mutate(Infectados=ifelse(is.na(Infectados),0,Infectados))%>% 
      select(FECHA_RESULTADO,Infectados)
    
    fallecidos_plot =   fallecidos %>%
      select(FECHA_FALLECIMIENTO,DEPARTAMENTO,EDAD_DECLARADA,SEXO) %>%
      mutate(FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,format='%d/%m/%Y'))%>% 
      filter(DEPARTAMENTO %in% filtro & !is.na(FECHA_FALLECIMIENTO)) %>%
      group_by(FECHA_FALLECIMIENTO) %>%
      summarise(Fallecidos=n()) %>% dplyr::rename("FECHA_RESULTADO"="FECHA_FALLECIMIENTO")
    
    fallecidos_final = fallecidos_plot %>% merge(df_fechas,by="FECHA_RESULTADO",all=TRUE) %>% 
      mutate(Fallecidos=ifelse(is.na(Fallecidos),0,Fallecidos)) %>% 
      select(FECHA_RESULTADO,Fallecidos)
    
    consolidado_plot = contagiados_final %>% left_join(fallecidos_final) %>% 
      mutate(Semana=isoweek(FECHA_RESULTADO)-11) %>% 
      group_by(Semana) %>% 
      summarise(Infectados=sum(Infectados),
                Fallecidos=sum(Fallecidos,na.rm=TRUE)) %>% ungroup() %>%
      mutate(Infectados=cumsum(Infectados),
             Fallecidos=cumsum(Fallecidos)) 
    # melt(id.vars="Semana",
    #      variable.name="Grupo",
    #      value.name="Cantidad de personas")
    
    # Parametros para el segundo eje
    ml = with(consolidado_plot,lm(Fallecidos ~ Infectados))
    b = ml$coefficients[2]
    
    grafica<- ggplot(aes(x=Semana),data=consolidado_plot) +
      geom_line(aes(y=Infectados,colour="Infectados"),size=1)+
      geom_line(aes(y=Fallecidos/b,colour="Fallecidos"),size=1,linetype="dashed")+
      geom_point(aes(y=Infectados,colour="Infectados"),size=2.5)+
      geom_point(aes(y=Fallecidos/b,colour="Fallecidos"),size=2.5)+
      scale_y_continuous(sec.axis = sec_axis(~.*b,name="# de Fallecidos\n"),
                         breaks=scales::pretty_breaks(n = 10)) +
      labs(title=paste0("Evolución del coronavirus en \n",filtro),
           x="\nSemana de\n cuarentena",
           y="# de Infectados \n")+
      scale_x_continuous(labels = as.character(consolidado_plot$Semana), 
                         breaks = consolidado_plot$Semana)+
      theme(legend.text=element_text(size = 12),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5,size=16,face="bold"),
            axis.text=element_text(size = 12),
            axis.title = element_text(size = 14)) +
      transition_reveal(Semana)
    gif <- animate(grafica,fps=1,nframes=length(consolidado_plot$Semana),
                   width=1000,height=500)
    anim_save("outfile.gif",gif)
  }
  
}

# Interfaz de usuario
ui <- fluidPage(
  
    navbarPage("Evolución de Coronavirus", id="naveg",
               tabPanel("Mapa interactivo",
                        div(class='outer',
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mapa",width = "100%", height = "100%"),
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 350, height = "auto",
                                          h2('Filtros de analisis',align='center'),
                                          
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
                                                                           onInitialize = I('function() { this.setValue(""); }'))))
                            )
                            )),
               tabPanel("Estadisticas de selección",
                        withSpinner(imageOutput("grafica"),type=2))
               
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
                    direction = "auto"),
                  layerId = ~NOMBDEP)
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

id_poligono = reactive({
  if(is.null(input$mapa_shape_click)){
    NULL}
  else{
    aux = input$mapa_shape_click$id
    aux}
  })


output$grafica = renderImage({
  if(is.null(id_poligono())){
    return(NULL)
  }
  else{
    analisis = input$Analisis
    
    if(analisis=="Departamental"){
      departamento = id_poligono()
      grafica = fx_graficas("Dep",departamento)
      list(src = "outfile.gif",
           contentType = 'image/gif') }

    }
  
    },deleteFile = TRUE)

}

shinyApp(ui = ui, server = server)

