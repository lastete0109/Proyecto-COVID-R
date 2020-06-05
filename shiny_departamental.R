options(stringsAsFactors = FALSE)

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

setwd("C://Users//Leo//Desktop//Proyecto COVID - R")

# Datas GeoJson
data_depart = readOGR('peru_departamental_simple.geojson')
data_distrital = readOGR('peru_distrital_simple.geojson')

# Informacion del MINSA
contagiados = read.csv('https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download')
fallecidos = read.csv('https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download')

# Despegables
list_depart = as.character(data_depart$NOMBDEP)
list_prov = as.character(sort(unique(data_distrital$NOMBPROV)))
list_distr = as.character(sort(unique(data_distrital$NOMBDIST)))
lista_analisis = c('Departamental','Provincial','Distrital')

fx_metricas =  function(tipo,filtro){
  posibles = c("DEPARTAMENTO","DISTRITO","PROVINCIA")
  
  dia_semana = ifelse(wday(Sys.Date())-1==0,7,wday(Sys.Date())-1)
  
  semana_pasada = seq(Sys.Date()-dia_semana-6,Sys.Date()-dia_semana,by="days")
  
  contagiados_clean = contagiados %>%
    select(FECHA_RESULTADO,DEPARTAMENTO,PROVINCIA,DISTRITO,EDAD,SEXO) %>%
    mutate(FECHA_RESULTADO=as.Date(FECHA_RESULTADO,format='%d/%m/%Y'),
           SEXO=ifelse(SEXO=='','POR DETERMINAR',SEXO),
           PROVINCIA=ifelse(grepl('EN INVESTIGACIÓN',PROVINCIA),NA,PROVINCIA),
           DISTRITO=ifelse(grepl('EN INVESTIGACIÓN',DISTRITO),NA,DISTRITO),
           DIA=weekdays(FECHA_RESULTADO))
  
  fallecidos_clean =  fallecidos %>%
    select(FECHA_FALLECIMIENTO,DEPARTAMENTO,PROVINCIA,DISTRITO,EDAD_DECLARADA,SEXO) %>%
    mutate(FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,format='%d/%m/%Y'),
           PROVINCIA=ifelse(PROVINCIA=="",NA,PROVINCIA),
           DISTRITO=ifelse(DISTRITO=="",NA,toupper(DISTRITO)))
  
  # Nos quedamos solo con las columnas de interes
  contagiados_interes = contagiados_clean %>% 
    select(-posibles[is.na(match(posibles,tipo))]) %>%
    dplyr::rename("INTERES"=tipo)
  
  fallecidos_interes =  fallecidos_clean %>% 
    select(-posibles[is.na(match(posibles,tipo))]) %>%
    dplyr::rename("INTERES"=tipo)
  
  
  # Analizamos contagiados 
  contagiados_filtro =  contagiados_interes %>% 
    select(FECHA_RESULTADO,SEXO,EDAD,DIA,INTERES) %>%
    filter(INTERES %in% filtro)
  
  resumen = contagiados_filtro %>% mutate(SEXO = paste0(SEXO,"_I")) %>%
    group_by(INTERES,SEXO) %>% 
    summarise(Infectados=n()) %>% ungroup() %>%
    cast(INTERES~SEXO,fill=0) %>% 
    mutate(TOTAL_I=rowSums(.[grep("_I", names(.))], na.rm = TRUE))
  
  resumen_dia = contagiados_filtro %>% group_by(INTERES,FECHA_RESULTADO) %>%
    summarise(Infectados=n()) %>% top_n(1,Infectados) %>% select(-Infectados) %>%
    dplyr::rename("PICO_INF"="FECHA_RESULTADO")
  
  resumen_dia_semana =  contagiados_filtro %>% group_by(INTERES,DIA) %>%
    summarise(Infectados=n()) %>% top_n(2,Infectados) %>%
    mutate(DIAS_MAS_INFECTADOS=paste0(DIA,collapse=',')) %>% ungroup() %>%
    distinct(INTERES,DIAS_MAS_INFECTADOS) 
  
  resumen_ayer =  contagiados_filtro %>% filter(FECHA_RESULTADO==Sys.Date()-days(1)) %>% 
    group_by(INTERES) %>%
    summarise(Infectados=n()) %>% ungroup() %>%
    dplyr::rename("INFECTADOS_AYER"="Infectados")
  
  resumen_semana_pasada = contagiados_filtro %>% filter(FECHA_RESULTADO %in% semana_pasada) %>% 
    group_by(INTERES) %>%
    summarise(Infectados=n()) %>% ungroup() %>%
    dplyr::rename("INFECTADOS_SEMANA_PASADA"="Infectados")
  
  resumen_cons =  resumen %>% 
    left_join(resumen_dia) %>% 
    left_join(resumen_dia_semana) %>%
    left_join(resumen_ayer) %>%
    left_join(resumen_semana_pasada)
  
  # Analizamos fallecidos
  fallecidos_filtro =   fallecidos_interes %>% 
    select(FECHA_FALLECIMIENTO,INTERES,SEXO,EDAD_DECLARADA) %>%
    filter(INTERES %in% filtro)
  
  resumen_rip = fallecidos_filtro %>% mutate(SEXO = paste0(SEXO,"_F")) %>% 
    group_by(INTERES,SEXO) %>% 
    summarise(Fallecidos=n()) %>% ungroup() %>%
    cast(INTERES~SEXO,fill=0) %>% 
    mutate(TOTAL_F= rowSums(.[grep("_F", names(.))], na.rm = TRUE))
  
  resumen_rip_semana_pasada = fallecidos_filtro %>% filter(FECHA_FALLECIMIENTO %in% semana_pasada) %>% 
    group_by(INTERES) %>% summarise(Fallecidos=n()) %>% ungroup() %>%
    dplyr::rename("FALLECIDOS_SEMANA_PASADA"="Fallecidos")
  
  resumen_rip_cons =  resumen_rip %>% 
                      left_join(resumen_rip_semana_pasada)
  
  # Consolidamos info.
  metricas = resumen_cons  %>% left_join(resumen_rip_cons) %>% 
             dplyr::rename_with(function(x){return(tipo)},starts_with("INTERES"))
  return(metricas)
  
}

fx_graficas = function(tipo,filtro){
  
  posibles = c("DEPARTAMENTO","DISTRITO","PROVINCIA")
  df_fechas = data.frame(FECHA_RESULTADO=seq(as.Date("16-03-2020","%d-%m-%Y"),Sys.Date()-1,by="days"))
  
  contagiados_clean = contagiados %>%
    select(FECHA_RESULTADO,DEPARTAMENTO,PROVINCIA,DISTRITO,EDAD,SEXO) %>%
    mutate(FECHA_RESULTADO=as.Date(FECHA_RESULTADO,format='%d/%m/%Y'),
           SEXO=ifelse(SEXO=='','POR DETERMINAR',SEXO),
           PROVINCIA=ifelse(grepl('EN INVESTIGACIÓN',PROVINCIA),NA,PROVINCIA),
           DISTRITO=ifelse(grepl('EN INVESTIGACIÓN',DISTRITO),NA,DISTRITO))
  
  fallecidos_clean =  fallecidos %>%
    select(FECHA_FALLECIMIENTO,DEPARTAMENTO,PROVINCIA,DISTRITO,EDAD_DECLARADA,SEXO) %>%
    mutate(FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,format='%d/%m/%Y'),
           PROVINCIA=ifelse(PROVINCIA=="",NA,PROVINCIA),
           DISTRITO=ifelse(DISTRITO=="",NA,toupper(DISTRITO)))
  
  # Nos quedamos solo con las columnas de interes
  contagiados_interes = contagiados_clean %>% 
    select(-posibles[is.na(match(posibles,tipo))]) %>%
    dplyr::rename("INTERES"=tipo)
  
  fallecidos_interes =  fallecidos_clean %>% 
    select(-posibles[is.na(match(posibles,tipo))]) %>%
    dplyr::rename("INTERES"=tipo)
  
  
  # Grafica: Infectados x semana de cuarentena
  
  contagiados_final = contagiados_interes %>%
    filter(INTERES %in% filtro & !is.na(FECHA_RESULTADO)) %>%
    group_by(FECHA_RESULTADO) %>%
    summarise(Infectados=n()) %>% 
    merge(df_fechas,by="FECHA_RESULTADO",all=TRUE) %>% 
    mutate(Infectados=ifelse(is.na(Infectados),0,Infectados))%>% 
    select(FECHA_RESULTADO,Infectados)
  
  fallecidos_final =  fallecidos_interes %>%
    filter(INTERES %in% filtro & !is.na(FECHA_FALLECIMIENTO)) %>%
    group_by(FECHA_FALLECIMIENTO) %>%
    summarise(Fallecidos=n()) %>% dplyr::rename("FECHA_RESULTADO"="FECHA_FALLECIMIENTO") %>%
    merge(df_fechas,by="FECHA_RESULTADO",all=TRUE) %>% 
    mutate(Fallecidos=ifelse(is.na(Fallecidos),0,Fallecidos)) %>% 
    select(FECHA_RESULTADO,Fallecidos)
  
  consolidado_plot = contagiados_final %>% left_join(fallecidos_final) %>% 
    mutate(Semana=isoweek(FECHA_RESULTADO)-11) %>% 
    group_by(Semana) %>% 
    summarise(Infectados=sum(Infectados),
              Fallecidos=sum(Fallecidos,na.rm=TRUE)) %>% ungroup() %>%
    mutate(Infectados=cumsum(Infectados),
           Fallecidos=cumsum(Fallecidos)) 
  
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
                 width=750,height=400)
  anim_save("outfile.gif",gif)
  }

fx_graficas2 = function(tipo,filtro){
  posibles = c("DEPARTAMENTO","DISTRITO","PROVINCIA")
  df_fechas = data.frame(FECHA_RESULTADO=seq(as.Date("16-03-2020","%d-%m-%Y"),Sys.Date()-1,by="days"))
  
  contagiados_clean = contagiados %>%
    select(FECHA_RESULTADO,DEPARTAMENTO,PROVINCIA,DISTRITO,EDAD,SEXO) %>%
    mutate(FECHA_RESULTADO=as.Date(FECHA_RESULTADO,format='%d/%m/%Y'),
           SEXO=ifelse(SEXO=='','POR DETERMINAR',SEXO),
           PROVINCIA=ifelse(grepl('EN INVESTIGACIÓN',PROVINCIA),NA,PROVINCIA),
           DISTRITO=ifelse(grepl('EN INVESTIGACIÓN',DISTRITO),NA,DISTRITO))
  
  fallecidos_clean =  fallecidos %>%
    select(FECHA_FALLECIMIENTO,DEPARTAMENTO,PROVINCIA,DISTRITO,EDAD_DECLARADA,SEXO) %>%
    mutate(FECHA_FALLECIMIENTO=as.Date(FECHA_FALLECIMIENTO,format='%d/%m/%Y'),
           PROVINCIA=ifelse(PROVINCIA=="",NA,PROVINCIA),
           DISTRITO=ifelse(DISTRITO=="",NA,toupper(DISTRITO)))
  
  # Nos quedamos solo con las columnas de interes
  contagiados_interes = contagiados_clean %>% 
    select(-posibles[is.na(match(posibles,tipo))]) %>%
    dplyr::rename("INTERES"=tipo)
  
  fallecidos_interes =  fallecidos_clean %>% 
    select(-posibles[is.na(match(posibles,tipo))]) %>%
    dplyr::rename("INTERES"=tipo)
  
  contagiados_density = contagiados_interes %>% 
    filter(INTERES %in% filtro & SEXO!="POR DETERMINAR" & !is.na(EDAD))  %>%
    select(SEXO,EDAD)
  
  fallecidos_density =  fallecidos_interes %>% 
    filter(INTERES %in% filtro & SEXO!="INDETERMINADO" & !is.na(EDAD_DECLARADA))  %>%
    select(SEXO,EDAD_DECLARADA)
  
  estadisticos = data.frame(Metrica=rep(c("Moda","Media","Mediana"),times=2),
                            Grupo=rep(c("Contagiado","Fallecido"),each=3),
                            Medidas= c(density(contagiados_density$EDAD)$x[which.max(density(contagiados_density$EDAD)$y)], 
                                       mean(contagiados_density$EDAD),
                                       median(contagiados_density$EDAD),
                                       density(fallecidos_density$EDAD_DECLARADA)$x[which.max(density(fallecidos_density$EDAD_DECLARADA)$y)],
                                       mean(fallecidos_density$EDAD_DECLARADA),
                                       median(fallecidos_density$EDAD_DECLARADA)
                            ))
  
  grafica<- ggplot() + 
            geom_density(aes(x=EDAD,fill="Contagiado"),data=contagiados_density,alpha=0.25) + 
            geom_density(aes(x=EDAD_DECLARADA,fill="Fallecido"),data=fallecidos_density,alpha=0.25)+
            geom_vline(aes(xintercept=Medidas,linetype=Metrica,color=Metrica),size=1,data=estadisticos)+
            scale_x_continuous(breaks=scales::pretty_breaks(n=10)) +
            scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
            labs(title=paste0("Edades de contagiados y fallecidos \n por coronavirus en ",filtro),
                 x="\n Edad",
                 y="\n Probabilidad de \n evento",
                 fill="Grupo") +
            theme(legend.text=element_text(size = 12),
                  legend.title = element_blank(),
                  plot.title = element_text(hjust = 0.5,size=14,face="bold"),
                  axis.text=element_text(size = 12),
                  axis.title = element_text(size = 14))
  return(grafica)
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
                        p("Esta pestaña muestra algunas estadísticas de interés del polígono seleccionado.
                          Para hacer uso de eta pestaña, seleccione algún tipo de análisis y seleccione
                          algún polígono graficado en el mapa."),
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"), 
                                      imageOutput("grafica"), 
                                      plotOutput("grafica2")))
                        )
               
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
    metricas = fx_metricas("DEPARTAMENTO",depart_selecc)
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
      metricas$TOTAL_I,
      metricas$FEMENINO_I,
      metricas$MASCULINO_I,
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
    metricas = fx_metricas("DISTRITO",dist_selecc)
    data_selecc@data =  data_selecc@data %>% 
                        left_join(metricas,by= c("NOMBDIST"="DISTRITO"))  %>%
                        mutate(LABEL=pmap(list(a=NOMBDIST,b=TOTAL_I,c=FEMENINO_I,d=MASCULINO_I,
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
                    direction = "auto"),
                  layerId = ~NOMBDIST)
    
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
    list(src="http://www.i2symbol.com/images/abc-123/o/white_smiling_face_u263A_icon_256x256.png",
         filetype = "image/png",
         alt="Error: No ha seleccionado ninguna opcion")
  }
  else{
    analisis = input$Analisis
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = 'Generando gif')
    
    if(analisis=="Departamental"){
      departamento = id_poligono()
      grafica = fx_graficas("DEPARTAMENTO",departamento)
      list(src = "outfile.gif",
           contentType = 'image/gif') }
    
    
    if(analisis=="Distrital"){
      distrito = id_poligono()
      grafica = fx_graficas("DISTRITO",distrito)
      list(src = "outfile.gif",
           contentType = 'image/gif') }

    }
    },deleteFile = TRUE)

output$grafica2 = renderPlot({
  if(is.null(id_poligono())){
    return(NULL)
  }
  else{
    analisis = input$Analisis
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = 'Generando gráfica')
    
    if(analisis=="Departamental"){
      departamento = id_poligono()
      fx_graficas2("DEPARTAMENTO",departamento)}
    
    if(analisis=="Distrital"){
      distrito = id_poligono()
      fx_graficas2("DISTRITO",distrito)}
    
    
    
    }
})

}

shinyApp(ui = ui, server = server)

