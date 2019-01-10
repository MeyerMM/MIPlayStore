library(shiny)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
library(plyr)
library(stringr)
library(plotly)

bcl <- read.csv("googleplaystore.csv", stringsAsFactors = FALSE)
assign("bclg",bcl,envir = .GlobalEnv)

ui <- navbarPage("Google Play App Data",
                 tabPanel("Aplicaciones de Pagas y Gratuitas",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("pYGPopularityInput", "Medida de popularidad",
                                           choices = c("Numero de aplicaciones", "Descargas", "Analisis"),
                                           selected = "Numero de aplicaciones")
                            ),
                            mainPanel(
                              plotlyOutput("pYGCakePlot"),
                              br(), br(),
                              plotlyOutput("pYGBarPlot")
                            )  
                          )
                 ),
                 tabPanel("Analisis de Precio",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("priceInput", "Precio", 0, 450, c(0, 50), pre = "$"),
                              sliderInput("installsInput", "Descargas", 0, 10500000, c(0, 1200000))
                            ),
                            mainPanel(
                              plotlyOutput("pricePlot"),
                              ("Cada punto representa una aplicacion. Son traslucidos para permitir detectar solapamiento."),
                              br(),
                              ("Nota: Diagrama de dispersion implementado con WebGl. No se visualiza en RStudio para Windows")
                            )  
                          )
                 ),
                 tabPanel("Analisis de Evaluaciones",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("evalTypeInput", "Tipo de aplicacion",
                                           choices = c("Pagas y Gratuitas", "Gratuitas", "Pagas"),
                                           selected = "Pagas y Gratuitas"),
                              sliderInput("evalInstallsInput", "Descargas", 0, 1100, c(0, 1100)),
                              radioButtons("evalMultipInput", "Multiplicador de escala",
                                           choices = c("X1", "X100", "X1.000","X10.000","X100.000","X1.000.000"),
                                           selected = "X1.000.000")
                            ),
                            mainPanel(
                              plotlyOutput("evalPlot"),
                              ("Cada punto representa una aplicacion. Son traslucidos para permitir detectar solapamiento."),
                              br(),
                              ("Nota: Diagrama de dispersion implementado con WebGl. No se visualiza en RStudio para Windows")
                            )  
                          )
                 ),
                 tabPanel("Analisis de Categorias",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("catTypeInput", "Tipo de aplicacion",
                                           choices = c("Pagas y Gratuitas", "Gratuitas", "Pagas"),
                                           selected = "Pagas y Gratuitas"),
                              radioButtons("catPopularityInput", "Medida de popularidad",
                                           choices = c("Numero de aplicaciones", "Descargas promedio", "Analisis promedio"),
                                           selected = "Numero de aplicaciones")
                            ),
                            mainPanel(
                              ggiraphOutput("categoryplot")
                            )  
                          )
                 ),
                 tabPanel("Analisis de Accesibilidad",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("accessInput", "Barrera de acceso",
                                           choices = c("Clasificacion de contenido", "Version de Android"),
                                           selected = "Clasificacion de contenido"),
                              radioButtons("accessMeasureInput", "Medida de popularidad",
                                           choices = c("Numero de aplicaciones", "Descargas totales", "Descargas promedio", "Analisis promedio"),
                                           selected = "Numero de aplicaciones")
                            ),
                            mainPanel(
                              plotlyOutput("accessPlot")
                            )  
                          )
                 )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  # Eliminar simbolo + de Installs
  bcl <- data.frame(lapply(bcl, gsub, pattern = "+", replacement = "", fixed = TRUE))
  # Eliminar simbolo , de Installs
  bcl <- data.frame(lapply(bcl, gsub, pattern = ",", replacement = "", fixed = TRUE))
  # Eliminar simbolo $ de Price
  bcl <- data.frame(lapply(bcl, gsub, pattern = "$", replacement = "", fixed = TRUE))
  bcl <- as.data.frame(bcl)
  
  
  output$evalPlot<-renderPlotly({
    if(input$evalMultipInput=="X1"){
      mul = 1
    }
    if(input$evalMultipInput=="X100"){
      mul = 100
    }
    if(input$evalMultipInput=="X1.000"){
      mul = 1000
    }
    if(input$evalMultipInput=="X10.000"){
      mul = 10000
    }
    if(input$evalMultipInput=="X100.000"){
      mul = 100000
    }
    if(input$evalMultipInput=="X1.000.000"){
      mul = 1000000
    }
    if(input$evalTypeInput=="Pagas"){
      data = bcl[bcl$Type == "Paid",]
    }
    if(input$evalTypeInput=="Gratuitas"){
      data = bcl[bcl$Type == "Free",]
    }
    if(input$evalTypeInput=="Pagas y Gratuitas"){
      data = bcl
    }

    #  Definir colores a usar
    pal <- c('#1f77b4', '#ff7f0e')
    
       # Dibujar grafica. Valores del Data Frame deben ser convertidos a numeros. Los puntos se les coloca un alpha de 0.2 para que sean traslucidos y se pueda ver solapamiento.
      data %>%plot_ly(x = ~as.numeric(as.character(Rating)), y = ~as.numeric(as.character(Installs)), color=~Type, colors = pal, alpha = 0.2,  type = "scattergl",
                     hoverinfo = 'text',
                     text= ~paste("X: ",data$Rating,"Estrellas","Y:",data$Installs,"Descargas"))%>% 
     layout(yaxis = list(title = "Descargas", range = c(input$evalInstallsInput[1]*mul,input$evalInstallsInput[2]*mul)),  # Variar rango de eje X por el input
              xaxis = list( title = "Evaluacion"))
    
  })
  
  output$accessPlot<-renderPlotly({
    # Aplicar filtro del input
   
    if(input$accessInput=="Clasificacion de contenido"){
      if(input$accessMeasureInput=="Numero de aplicaciones"){
        data = setNames(count(bcl, 'ContentRating'), c("Type","freq"))
      }
      if(input$accessMeasureInput=="Descargas totales"){
        # Calcular instalaciones totales por version de android soportada
        data = setNames(ddply(bcl,.(ContentRating), summarize,  freq=sum(as.numeric(as.character(Installs)))), c("Type","freq")) 
      }
      if(input$accessMeasureInput=="Descargas promedio"){
        # Calcular instalaciones promedio clasificacion de contenido
        data = setNames(ddply(bcl,.(ContentRating), summarize,  freq=mean(as.numeric(as.character(Installs)))), c("Type","freq"))   
      }
      if(input$accessMeasureInput=="Analisis promedio"){
        # Calcular analisis promedio por clasificacion de contenido
        data = setNames(ddply(bcl,.(ContentRating), summarize,  freq=mean(as.numeric(as.character(Reviews)))), c("Type","freq"))   
      }
      description = "Clasificacion de contenido"
      barcolor = "#99004C"
      xform <- list(title = description,
                    categoryorder = "array",
                    categoryarray = c("Unrated",
                                      "Everyone", 
                                      "Everyone 10", 
                                      "Teen",
                                      "Mature 17",
                                      "Adults only 18"))
      
      
    }
    if(input$accessInput=="Version de Android"){
     
      if(input$accessMeasureInput=="Numero de aplicaciones"){
        data = setNames(count(bcl, 'AndroidVer'), c("Type","freq"))
      }
      if(input$accessMeasureInput=="Descargas totales"){
        # Calcular instalaciones totales por version de android soportada
        data = setNames(ddply(bcl,.(AndroidVer), summarize,  freq=sum(as.numeric(as.character(Installs)))), c("Type","freq")) 
      }
      if(input$accessMeasureInput=="Descargas promedio"){
        # Calcular instalaciones promedio por version de android soportada
        data = setNames(ddply(bcl,.(AndroidVer), summarize,  freq=mean(as.numeric(as.character(Installs)))), c("Type","freq")) 
      }
      if(input$accessMeasureInput=="Analisis promedio"){
        # Calcular analisis promedio por clasificacion de contenido
        data = setNames(ddply(bcl,.(AndroidVer), summarize,  freq=mean(as.numeric(as.character(Reviews)))), c("Type","freq"))   
      }
      description = "Compatibilidad con versiones de Android"
      barcolor = "#990000"
      xform <- list(title = description)
      
    }
    plot_ly(data, x = ~Type, y = ~freq, type = "bar", textposition = 'auto', 
            marker = list(color = barcolor),
            insidetextfont = list(color = '#FFFFFF')
    )%>%
      layout(title = paste("Instalaciones promedio por ", tolower(description)),
             barmode = 'group',
             xaxis = xform,
             yaxis = list(title = "Instalaciones promedio"))
    
  })
  
  output$pricePlot<-renderPlotly({
    #  Definir colores a usar
    pal <- c('#1f77b4', '#ff7f0e')
    # Dibujar grafica. Valores del Data Frame deben ser convertidos a numeros. Los puntos se les coloca un alpha de 0.1 para que sean traslucidos y se pueda ver solapamiento.
    bcl %>%plot_ly(x = ~as.numeric(as.character(Price)), y = ~as.numeric(as.character(Installs)), color=~Type, alpha = 0.2,  type = "scattergl", colors = pal,
                   hoverinfo = 'text',
                   text= ~paste("X: $",bcl$Price,"Y:",bcl$Installs,"Descargas"))%>% 
      layout(yaxis = list(title = "Descargas", range = c(input$installsInput[1],input$installsInput[2])),  # Variar rango de eje X por el input
             xaxis = list( title = "Precio ($)",range = c(input$priceInput[1],input$priceInput[2] )))# Variar rango de eje Y por el input
  })
  
  output$pYGBarPlot<-renderPlotly({
    # Aplicar filtro del input
    if(input$pYGPopularityInput=="Numero de aplicaciones"){
      data = count(bcl, 'Type')  # Contar numero de aplicaciones de cada tipo
      description = "Numero de aplicaciones"
    }
    if(input$pYGPopularityInput=="Descargas"){
      data = ddply(bcl,.(Type), summarize,  freq=mean(as.numeric(as.character(Installs)))) # Calcular instalaciones promedio por tipo
      description = "Promedio de Descargas"
    }
    if(input$pYGPopularityInput=="Analisis"){
      data = ddply(bcl,.(Type), summarize,  freq=mean(as.numeric(as.character(Reviews)))) # Calcular numero promedio de reviews por tipo
      description = "Promedio de Analisis"
    }
    data <- data[order(data$freq),]  # Ordenar grafica para que colores de los valores coincida con la grafica de torta.
    plot_ly(data, x = ~freq, y = ~Type, type = "bar", textposition = 'auto', orientation = 'h',
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(color = c('rgba(255, 127, 14,1)', 'rgba(31, 119, 180,1)')),
            hoverinfo = 'text',
            text = ~paste(description,":",round(data$freq, digits=2))
           )%>%
      layout(title = paste(description,"por tipo"),
             barmode = 'group',
             xaxis = list(title = "Tipo de aplicacion"),
             yaxis = list(title = description))
    
  })
  
  output$pYGCakePlot<-renderPlotly({
    if(input$pYGPopularityInput=="Numero de aplicaciones"){
      data = count(bcl, 'Type')
      description = "Numero de aplicaciones"
    }
    if(input$pYGPopularityInput=="Descargas"){
      data = setNames(aggregate(as.numeric(as.character(bcl$Installs)), by=list(Type=bcl$Type), FUN=sum), c("Type","freq"))
      description = "Descargas"
    }
    if(input$pYGPopularityInput=="Analisis"){
      data =  setNames(aggregate(as.numeric(as.character(bcl$Reviews)), by=list(Type=bcl$Type), FUN=sum), c("Type","freq"))
      description = "Analisis"
    }
    data <- data[order(-data$freq),] # Ordenar entradas para que colores coincidan con el diagrama de torta
    plot_ly(data, labels = ~Type, values = ~freq, type = 'pie',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            insidetextfont = list(color = '#FFFFFF'),
            outsidetextfont = list(color = '#FFFFFF', alpha=0), # Si el texto queda fuera del diagrama, no mostrarlo.
            text = ~paste(description,":", freq),
            pull=0.1,
            sort = FALSE,
            showlegend = TRUE) %>%
      layout(title = paste(description,"por tipo"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$categoryplot <- renderggiraph({
    # Revisar medida de popularidad seleccionada
    if(input$catPopularityInput=="Numero de aplicaciones"){
      # Generar cuenta de aplicaciones por categorias, filtrando por tipo de aplicacion
      if(input$catTypeInput=="Pagas"){
        data = count(bcl[bcl$Type == "Paid",], 'Category')
      }
      if(input$catTypeInput=="Gratuitas"){
        data = count(bcl[bcl$Type == "Free",], 'Category')
      }
      if(input$catTypeInput=="Pagas y Gratuitas"){
        data = count(bcl, 'Category')
      }
      colorPalette = "Greens"
      description = "Numero de aplicaciones"
    }
    if(input$catPopularityInput=="Descargas promedio"){
      # Generar promedio de descargas por categorias, filtrando por tipo de aplicacion
      if(input$catTypeInput=="Pagas"){
        data = ddply(bcl[bcl$Type == "Paid",],.(Category), summarize,  freq=mean(as.numeric(as.character(Installs))))
      }
      if(input$catTypeInput=="Gratuitas"){
        data = ddply(bcl[bcl$Type == "Free",],.(Category), summarize,  freq=mean(as.numeric(as.character(Installs))))
      }
      if(input$catTypeInput=="Pagas y Gratuitas"){
        data = ddply(bcl,.(Category), summarize,  freq=mean(as.numeric(as.character(Installs))))
      }
      
      colorPalette = "Blues"
      description = "Descargas promedio por aplicacion"
    }
    if(input$catPopularityInput=="Analisis promedio"){
      # Generar cuenta de aplicaciones por categorias, filtrando por tipo de aplicacion
      if(input$catTypeInput=="Pagas"){
        data = ddply(bcl[bcl$Type == "Paid",],.(Category), summarize,  freq=mean(as.numeric(as.character(Reviews))))
      }
      if(input$catTypeInput=="Gratuitas"){
        data = ddply(bcl[bcl$Type == "Free",],.(Category), summarize,  freq=mean(as.numeric(as.character(Reviews))))
      }
      if(input$catTypeInput=="Pagas y Gratuitas"){
        data = ddply(bcl,.(Category), summarize,  freq=mean(as.numeric(as.character(Reviews))))
      }
      colorPalette = "Reds"
      description = "Analisis promedio por aplicacion"
    }
    # Ordenar datos
    data <- data[order(-data$freq),]
    # Agregar texto a mostrar de forma interactiva
    data$text=paste("Categoria: ",data$Category, "\n", description,"\\:", round(data$freq, digits=2))
    # Calcular posiciones de circulos
    packing <- circleProgressiveLayout(data$freq, sizetype='area')
    # Espaciar circulos
    packing$radius=0.98*packing$radius
    # Agregar informacion de circulos al data frame
    data = cbind(data, packing)
    # Set the number of points to draw the circles
    dat.gg <- circleLayoutVertices(packing, npoints=50, sizetype = "radius")
    # Hacer la grafica
    p = ggplot() + 
      # Dibujar las burbujas interactivas
      geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=as.numeric(as.factor(id)), tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.6) +
      # Definir el color de las burbujas
      scale_fill_distiller(palette = colorPalette) +
      # Agregar texto en el centro y controlar su tamano
      geom_label(data = data, aes(x, y, size=freq, label = Category), label.padding = unit(0.1, "lines")) +
      scale_size_continuous(range = c(0,5)) +
      # Seleccionar el theme
      theme_void() + 
      theme(legend.position="none", 
            plot.margin=unit(c(0,0,0,0),"cm"),
            plot.title = element_text(color="black") ) +
      coord_equal() +
      ggtitle(paste("Categorias por ",tolower(description), tolower(input$catTypeInput)))
    # Generar objeto interactivo
    girafe(ggobj = p, width_svg = 6, height_svg = 6)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

