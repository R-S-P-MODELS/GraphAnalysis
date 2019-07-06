#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(igraph)
require(plotly)

LayoutMemberShip=function(Communities){ #Communities is the communities vector cluster_louvrain gives us
  Size=length(Communities)
  Numbers=length(unique(Communities))
  xmax=6
  ymax=6
  xmin=-xmax
  ymin=-ymax
  layout=data.frame(1,2)
  Treshx=2*xmax/(Numbers)
  Treshy=2*ymax/(Numbers)
  gap=3
  for(i in 1:Size){
    layout[i,1]=xmin + Treshx*Communities[i] + sample(c(1,-1),1)*runif(1)
    layout[i,2]=ymin + Treshy*Communities[i] + sample(c(1,-1),1)*runif(1)
    
  }
  
  layout[which(layout[,1]<0),1]=(xmin/min(layout[,1]) )*layout[which(layout[,1]<0),1]
  layout[which(layout[,1]>0),1]=(xmax/max(layout[,1]) )*layout[which(layout[,1]>0),1]
  layout[which(layout[,2]<0),2]=(xmin/min(layout[,2]) )*layout[which(layout[,2]<0),2]
  layout[which(layout[,2]>0),2]=(xmax/max(layout[,2]) )*layout[which(layout[,2]>0),2]
  layout[,2]=-layout[,2]
  return(as.matrix(layout))
  
  
  
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
   
   # Application title
   titlePanel("Graph Properties"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "Arquivo",label = "Insira a matriz de adjacencia",accept = c("*.csv","*.txt"),placeholder = "Aguardando a matriz" ),
        conditionalPanel(
         condition = "input.Referencias == 'VizualizaçõesVetoriais'",
          selectInput("Opcoes",label = "Métrica a se observar",choices = c('Degree','Closeness','Betweenness','Alpha_Centrality','Local Transivity'))),
        
        
        conditionalPanel(
          condition = "input.Referencias == 'VizualizaçõesMatriciais'",
          selectInput("OpcoesM",label = "Métrica a se observar",choices = c('Distances','Similarity')))
        
        
        
        
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id = "Referencias",
                   tabPanel("grafico",plotOutput("VizualizarGrafo") ),
                    tabPanel("InformacoesNumericas",tableOutput("InformacoesNumericasTabulares")),
                tabPanel("VizualizaçõesVetoriais", plotlyOutput("Vetoriais")),
                 tabPanel("VizualizaçõesMatriciais",plotlyOutput("Matriciais")),
                tabPanel("Comunidades",plotOutput("Comunidade"))
        ),
        
       
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),

        h3("Aplicativo Desenvolvido por Rafael Silva Pereira"),
        h3("Email de contato r.s.p.models@gmail.com")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000000*1024^2)
  
   output$VizualizarGrafo<-renderPlot({
     if(!is.null(input$Arquivo)){
       require(data.table)
       Matriz=fread(input$Arquivo$datapath)
       Matriz1=as.matrix(Matriz)
       rownames(Matriz1)=colnames(Matriz1)=names(Matriz)
       Graph=graph_from_adjacency_matrix(Matriz1,mode="undirected")
       Graph=simplify(Graph)
       plot(Graph)
     }   
   },height = 800, width = 800)

   output$Comunidade<-renderPlot({
     if(!is.null(input$Arquivo)){
       require(data.table)
       Matriz=fread(input$Arquivo$datapath)
       Matriz1=as.matrix(Matriz)
       rownames(Matriz1)=colnames(Matriz1)=names(Matriz)
       Graph=graph_from_adjacency_matrix(Matriz1,mode="undirected")
       Graph=simplify(Graph)
       clusterlou<-cluster_louvain(Graph)
       layouts=LayoutMemberShip(clusterlou$membership)
       
       plot(Graph, vertex.color=rainbow(length(unique(clusterlou$membership)), alpha=0.6)[clusterlou$membership],layout=as.matrix(layouts))
       
     }   
   },height = 800, width = 800)
   
   output$InformacoesNumericasTabulares <- renderTable({
     if(!is.null(input$Arquivo)){
       require(data.table)
       Matriz=fread(input$Arquivo$datapath)
       Matriz1=as.matrix(Matriz)
       rownames(Matriz1)=colnames(Matriz1)=names(Matriz)
       Graph=graph_from_adjacency_matrix(Matriz1,mode="undirected")
       #Start to build data from Graph
       FinalTable=data.frame(1,2)
       names(FinalTable)=c('Metric','Value')
       FinalTable[1,]=c('Number of edges',ecount(Graph))
       FinalTable[2,]=c('Number of vertices',vcount(Graph))
       FinalTable[3,]=c('Mean Distance',mean_distance(Graph))
       FinalTable[4,]=c('Vertice connectivity',vertex_connectivity(Graph))
       FinalTable[5,]=c('Edge connectivity',edge_connectivity(Graph))
       FinalTable[6,]=c('Reciprocity',reciprocity(Graph))
       FinalTable[7,]=c('Clustering coefficient',transitivity(Graph))
       FinalTable[8,]=c('standart deviation of local clustering',sd(transitivity(Graph,type="local")))  
       FinalTable[9,]=c('Graph Associativity',assortativity(Graph,types1=V(Graph)) ) 
       FinalTable[10,]=c('Normalized mean degree',mean(degree(Graph))/length(V(Graph)) )
       FinalTable[11,]=c('Min Cut',min_cut(Graph) )
       FinalTable[12,]=c('Graph Density', graph.density(Graph)  )
       FinalTable[13,]=c('Modularity', modularity(Graph,membership=V(Graph))   )
       FinalTable[14,]=c('Farthest Distance',farthest.nodes(Graph)$distance)
       Indices=farthest.nodes(Graph)$vertices
       FinalTable[15,]=c('Nodes of Farthest Distance',paste(rownames(Matriz1)[Indices[1]],"with",rownames(Matriz1)[Indices[2]],sep=" "))
       return(FinalTable)
     }
     
   })
   
   output$Vetoriais<-renderPlotly({
     if(!is.null(input$Arquivo)){
       require(data.table)
       Matriz=fread(input$Arquivo$datapath)
       Matriz1=as.matrix(Matriz)
       rownames(Matriz1)=colnames(Matriz1)=names(Matriz)
       Graph=graph_from_adjacency_matrix(Matriz1,mode="undirected")
       if(input$Opcoes=="Degree")
         Objetivo=data.frame(names(Matriz),degree(Graph))
       if(input$Opcoes=="Closeness")
         Objetivo=data.frame(names(Matriz),closeness(Graph))
       if(input$Opcoes=="Betweenness")
         Objetivo=data.frame(names(Matriz),betweenness(Graph))
       if(input$Opcoes=="Alpha_Centrality")
         Objetivo=data.frame(names(Matriz),alpha_centrality(Graph))
       if(input$Opcoes=="Local Transivity")
         Objetivo=data.frame(names(Matriz),transitivity(Graph,type="local"))
       
     # este será uma vizualização gráfica dos elementos que retornam como vetores
     names(Objetivo)=c('objeto','valor')
     ggplotly(ggplot(data=Objetivo,aes(x=objeto,y=valor)) +geom_point()   )
     }
   })
   
   output$Matriciais<-renderPlotly({
     if(!is.null(input$Arquivo)){
       require(reshape2)
       require(data.table)
       Matriz=fread(input$Arquivo$datapath)
       Matriz1=as.matrix(Matriz)
       rownames(Matriz1)=colnames(Matriz1)=names(Matriz)
       Graph=graph_from_adjacency_matrix(Matriz1,mode="undirected")
       if(input$OpcoesM=="Distances")
         Objetivo=melt(distances(Graph))
       if(input$OpcoesM=="Similarity"){
         Objetivo=melt(similarity(Graph))
         Objetivo[,1]=colnames(Matriz1)[Objetivo[,1]]
         Objetivo[,2]=colnames(Matriz1)[Objetivo[,2]]
       }
       
       
       p1=ggplot(data = Objetivo, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + labs(x="",y=""  ) + theme(axis.title.x=element_blank(),
                                                                                                       axis.text.x=element_blank(),
                                                                                                       axis.ticks.x=element_blank() ,axis.title.y=element_blank(),
                                                                                                       axis.text.y=element_blank(),
                                                                                                       axis.ticks.y=element_blank() )    
       p1=p1+scale_fill_gradientn(colours = rainbow(20)) 
       ggplotly(p1)
   }
  

})
   
}

# Run the application 
shinyApp(ui = ui, server = server)

