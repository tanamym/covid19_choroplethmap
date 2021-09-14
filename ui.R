library(shiny)
library(leaflet)
library(shinyWidgets)
source("setShapeStyle.R")

shinyUI(fluidPage(
    leafletjs,
    # Application title
    titlePanel("神奈川県の市区町村ごとの新型コロナウイルス新規感染者数"),
    tags$head(tags$style(type="text/css",
                         "#loadmessage {
                   position: fixed;
                   top: 0px;
                   left: 0px;
                   width: 100%;
                   padding: 5px 0px 5px 0px;
                   text-align: center;
                   font-weight: bold;
                   font-size: 100%;
                   color: #ffffff;
                   background-color: #3399ff;
                   z-index: 105;
                   }
               .th {
                    display: table;
                    width: 100%;
                    }
            .title {
                    display: table-cell;
                    text-align: left;
                   }
             .home {
                    display: table-cell;
                    text-align: right;
                   }"
    )),
    
    sidebarLayout(
        sidebarPanel(
            tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #ffffff;
               background-color: #3399ff;
               z-index: 105;
               
             }
          ")),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading...",id="loadmessage")),
            uiOutput("update"),
            
            uiOutput("date"),
            
            numericInput("y",label = h5("累積日数の設定"),value="7"),
            # radioButtons("y",label = "Set the cumulative number of days",
            #              c("1day"="1",
            #                "7day"="7")
            # ),
            actionButton("button1","Update"),
            br(),
            switchInput("onoff",
                        label="路線図の表示"),
            width=3
            
            
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(type = "tabs",id="tabset",
                              tabPanel("感染者数",value = "tab1",
                                       fluidRow(
                                           tags$style(type = "text/css", "#covid_map {height: calc(70vh - 15px) !important;}",
                                                      "#yoko_map {height: calc(70vh - 15px) !important;}"),
                                           column(8,
                                                  h4(strong("神奈川県全体の状況")),
                                                  leafletOutput("covid_map"),
                                                  numericInput("color1",label="(1日当たりの)凡例の最大値",value=50),
                                           ),
                                           column(4,
                                                  h4(strong("横浜市の状況")),
                                                  leafletOutput("yoko_map"),
                                                  h5("横浜市のみ、区単位の結果を右の図で表示しています。このときの結果は一週間ごとの集計となっています。")
                                           )
                                       )),
                              
                              tabPanel("10万人当たりの感染者数",value = "tab2",
                                       fluidRow(
                                           tags$style(type = "text/css", "#covid_map2 {height: calc(70vh - 15px) !important;}",
                                                      "#yoko_map2 {height: calc(70vh - 15px) !important;}"),
                                           column(8,
                                                  h4(strong("神奈川県全体の状況")),
                                                  leafletOutput("covid_map2"),
                                                  numericInput("color2",label="(1日当たりの)凡例の最大値",value=8),
                                                  p("注意：人口が少ない市町村では10万人当たりの感染者数の色が濃くなることがあります。"),
                                           ),
                                           
                                           column(4,
                                                  h4(strong("横浜市の状況")),
                                                  leafletOutput("yoko_map2"),
                                                  h5("横浜市のみ、区単位の結果を右の図で表示しています。このときの結果は一週間ごとの集計となっています。")
                                           )
                                       )
                              )
        ),
        width=9
        )
    )
    
    
)
)

