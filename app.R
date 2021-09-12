if (!require(shiny)) {
  install.packages("shiny")
}
library(shiny)

if (!require(leaflet)) {
  install.packages("leaflet")
}
library(leaflet)

if (!require(shinythemes)) {
  install.packages("shinythemes")
}
library(shinythemes)

if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

if (!require(sf)) {
  install.packages("sf")
}
library(sf)

if (!require(stringr)) {
  install.packages("stringr")
}
library(stringr)

if(!require(lubridate)){
  install.packages("lubridate")
}
library(lubridate)

if(!require(tidyr)){
  install.packages("tidyr")
}
library(tidyr)

if(!require(htmltools)){
  install.packages("htmltools")
}
library(htmltools)

if(!require(sp)){
  install.packages("sp")
}
library(sp)

if(!require(ggplot2)){
  install.packages("ggplot2")
}
library(ggplot2)

if(!require(data.table)){
  install.packages("data.table")
}
library(data.table)

if(!require(curl)){
  install.packages("curl")
}
library(curl)

if(!require(dygraphs)){
  install.packages("dygraphs")
}
library(dygraphs)

ggColorHue <- function(n, l=50){
  hues <- seq(15, 375, length=n+1)
  hcl(h=hues, l=l, c=100)[1:n]
}


# barplot(1:n,col=ggColorHue(n,50))

# load("files3.RData")
load("files3.RData")

# data7 <-
#   fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/coviddata.csv",encoding="UTF-8") %>%
#   # fread("/srv/shiny-server/choroplethmap_kanagawa/coviddata.csv") %>%
#   mutate(Fixed_Date=as.Date(Fixed_Date)) %>%
#   arrange(desc(Fixed_Date),Hos) %>%
#   filter(!is.na(Fixed_Date))

data2020 <-
  fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2020.csv",encoding="UTF-8")

data202106 <-
  fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data202106.csv",encoding="UTF-8")

data2021 <-
  fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2021.csv",encoding="UTF-8")

data7 <-
  rbind(data2020,data202106,data2021) %>%
  mutate(Fixed_Date=as.Date(Fixed_Date)) %>%
  arrange(desc(Fixed_Date),Hos)

ycd <-
  fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/yoko_covid.csv",encoding="UTF-8") %>%
  mutate(Date=as.Date(Date))
#   group_by(Date,City=gsub("市外.*","その他",City)) %>%
#   summarise(count=sum(count))

date <- 
  data.frame(Date=min(data7$Fixed_Date):max(data7$Fixed_Date)) %>%
  arrange(desc(Date)) %>%
  mutate(Date=as.Date(Date,origin="1970-01-01")) %>%
  filter(Date>="2020-04-20")

yoko<-
  read.csv("https://square.umin.ac.jp/kenkono/csv/ward-new.csv",encoding = "UTF-8",header = F)

yoko2<-
  yoko %>%
  filter(V1!="",V1!="区名") %>%
  tidyr::pivot_longer(-V1, names_to = "V", values_to="count") %>%
  rename(N03_004="V1") %>%
  mutate(count=as.numeric(as.character(count)))

yoko3 <-
  yoko[1:2,] %>%
  t() %>%
  data.frame() %>%
  add_rownames("V") %>%
  mutate(date=str_replace(X2," .+","")) %>%
  mutate(start=str_replace(date,"~.+",""),
         end=str_replace(date,".*~","")) %>%
  mutate(start=as.Date(paste(X1,start),format="%Y年 %m/%d")) %>%
  mutate(end=as.Date(paste(X1,end),format="%Y年 %m/%d"))

data <-
  yoko2 %>%
  filter(!N03_004%in%c("日本","横浜市","調査中","神奈川県")) %>% #,"市外"
  left_join(yoko3 %>% select(V,start,end)) %>%
  select(-V)

CDS <-
  data7 %>%
  count(Date=Fixed_Date,N03_004=Residential_City,Hos) %>%
  full_join(ycd %>% mutate(Hos="横浜")) %>%
  mutate(N03_004=ifelse(!is.na(City),City,N03_004)) %>%
  mutate(n=ifelse(!is.na(City),count,n)) %>%
  distinct() %>%
  select(-City,-count) %>%
  mutate(N03_004=gsub("川崎市","",N03_004)) %>%
  full_join(date) %>%
  rename(nday=n) %>%
  complete(Date,N03_004,fill=list(nday=0)) %>%
  left_join(xy) %>%
  mutate(N03_004=ifelse(is.na(X),"その他",N03_004)) %>%
  arrange(N03_007,N03_004,Date) %>%
  group_by(N03_007,N03_004,Date) %>%
  mutate(nday=sum(nday)) %>%
  distinct(N03_007,N03_004,Date,.keep_all = T) %>%
  group_by(N03_007,N03_004) %>%
  mutate(Nday=cumsum(nday)) %>%
  mutate(n7day=Nday-lag(Nday,7,default = 0)) %>%
  ungroup() %>%
  mutate(ndayj=nday/jinko*100000) %>%
  mutate(n7dayj=n7day/jinko*100000) %>%
  mutate(N03_004=factor(N03_004,levels = unique(N03_004))) %>%
  select(Date,N03_007,N03_004,X,Y,jinko,nday,Nday,n7day,ndayj,n7dayj) %>%
  mutate(JTime=as.POSIXct(Date))
DC <-
  CDS %>%
  arrange(desc(Date)) %>%
  distinct(N03_007,N03_004,.keep_all = T) %>%
  filter(n7dayj>=15) %>%
  filter(n7day>7) %>%
  mutate(Rank=dense_rank(-n7dayj)) %>%
  filter(Rank<=25)

ui <- shinyUI({
  fluidPage(
    #デザインを変えることができるよ####
    theme = shinytheme("lumen"),
    
    tags$title("神奈川県の市区町村ごとの新型コロナウイルス新規感染者数"),
    tags$head(
      tags$script("async src"="https://www.googletagmanager.com/gtag/js?id=G-XGR6Z90C8P"),
      tags$script("window.dataLayer = window.dataLayer || [];
                    function gtag(){dataLayer.push(arguments);}
                    gtag('js', new Date());
                    gtag('config', 'G-XGR6Z90C8P');")),
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
             }
   .dygraph-legend {
                    color: navy;
                    font-weight: bold;
                    left: 100% !important;
                    background-color: transparent !important;
                   }
       .highlight {
                    display: inline;
                    background-color: #B0B0B0;
                    }
  .checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
                   }
.checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
                   }"
    )),
    tags$head(tags$link(rel="alternate", media="only screen and (max-width: 640px)", href="http://covid-map.bmi-tokai.jp/choroplethmap_kanagawa_sp/")),
    
    tags$style(type = "text/css", 
               "#cdline {height: calc(60vh) !important; width: 92% !important;}",
               "#cdplot {height: calc(65vh) !important;}"),
    
    # Application title
    #ホームを右に設置 cssの設定もした####
    tags$div(class="th",
             tags$div(class="title",
                      h3(img(src="tokai.JPG",width="50px"),
                         "神奈川県の市区町村ごとの新型コロナウイルス新規感染者数（コロプレスマップ）")),
             tags$div(class="home",tags$a(href="http://covid-map.bmi-tokai.jp/","ホームへ戻る"),"　　")),
    
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")),
    
    #列で管理####
    fluidRow({
      wellPanel(
        column(1,h4("日付")),
        column(2,dateInput("x",
                           #label = "日付を入力(選択)してください",
                           label = NULL,
                           min = min(date$Date),
                           max = max(date$Date),
                           value = max(date$Date))),
        column(3,
               # h4("単位ごとの変動"),
               actionButton("back",label="前週"),
               actionButton("yesterday",label="前日"),
               actionButton("tomorrow", label = "翌日"),
               actionButton("next1", label = "翌週")),
        h4(column(1,"期間"),
           column(2,
                  radioButtons("y",
                               # label =  h4("累積日数を設定してください"),
                               label =NULL,
                               choices = c("1日"="1", "7日間累積"="7"),
                               inline = T))),
        "　")
    }),
    
    # Show a plot of the generated distribution
    fluidRow({
      mainPanel(
        tabsetPanel(
          type = "tabs",id="tabset",
          tabPanel("新規感染者数",value = "tab1",
                   tags$style(type = "text/css", 
                              "#covid_map {height: calc(59vh) !important;}",
                              "#yoko_map  {height: calc(59vh) !important;}"),
                   column(8,
                          h4(strong("神奈川県全体の状況")),
                          plotOutput("covid_map"),
                          p(paste0(min(date$Date),"記者発表資料から",max(date$Date),"記者発表資料掲載分まで集計しています。")),
                          p("　")),
                   column(4,
                          h4(strong("横浜市の状況")),
                          plotOutput("yoko_map"),
                          p("横浜市の区については金曜日～木曜日の集計結果となっています。"))
          ),
          
          tabPanel("人口10万人あたりの新規感染者数",value = "tab2",
                   tags$style(type = "text/css", 
                              "#covid_map2 {height: calc(59vh) !important;}",
                              "#yoko_map2  {height: calc(59vh) !important;}"),
                   column(8,
                          h4(strong("神奈川県全体の状況")),
                          plotOutput("covid_map2"),
                          p(paste0(min(date$Date),"記者発表資料から",max(date$Date),"記者発表資料掲載分まで集計しています。")),
                          p("注意：清川村、真鶴町、大井町など人口が少ない市町村では10万人あたりの感染者数の色が濃くなることがあります。")), #下から2つの市町村にした
                   column(4,
                          h4(strong("横浜市の状況")),
                          plotOutput("yoko_map2"),
                          p("横浜市の区については金曜日～木曜日の集計結果となっています。")) #変更
          ),
          
          tabPanel("感染者の推移",value = "tab5",
                   checkboxGroupInput("dc",
                                      label = NULL,
                                      choices = unique(CDS$N03_004)[-40],
                                      selected = DC$N03_004,
                                      inline = T),
                   checkboxGroupInput("chiho", 
                                      # label = "地方", 
                                      label=NULL,
                                      choices = c("横浜・川崎","横須賀・三浦","県央","湘南","県西"),
                                      inline = TRUE),
                   radioButtons("cd",label = NULL,
                                choices = c("分割プロット"="plot","重ね合わせプロット"="line"),
                                selected = "plot",
                                inline=T),
                   #追加1####
                   uiOutput("pl3"),
                   ####
                   uiOutput("pl"),
                   uiOutput("pl2")
          ),
          
          tabPanel("参考文献", value = "tab3",
                   h4(strong("参考文献、その他")),
                   
                   p("このサイトは、神奈川県や川崎市、茅ヶ崎市、藤沢市、相模原市、横浜市、横須賀市の行政のサイトで公開されている新型コロナウイルス感染症の感染者の情報を使用しています。", br(),
                     tags$a(href="https://www.pref.kanagawa.jp/prs/list-2021-1-1.html", "記者発表資料 県政記者クラブ2021年度の一覧(神奈川県)"), br(),
                     tags$a(href="https://www.pref.kanagawa.jp/docs/ga4/covid19/occurrence_list.html", "新型コロナウイルスに感染した患者の発生状況一覧(神奈川県)"), br(),
                     tags$a(href="https://www.city.kawasaki.jp/350/page/0000115886.html","【緊急情報】川崎市内の新型コロナウイルスに感染した患者等の発生状況"), br(),
                     tags$a(href="https://www.city.chigasaki.kanagawa.jp/koho/1030702/1038773/index.html","新型コロナウイルス感染症による管内の患者確認について(茅ヶ崎市)"), br(),
                     tags$a(href="https://www.city.fujisawa.kanagawa.jp/hoken-j/corona_doukou_data.html","新型コロナウイルス感染症の感染動向(藤沢市)"), br(),
                     tags$a(href="https://www.city.sagamihara.kanagawa.jp/shisei/koho/1019191.html","新型コロナウイルス感染症に関する相模原市発表資料"), br(),
                     tags$a(href="https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/kenko/","健康福祉局(横浜市)"), br(),
                     tags$a(href="https://www.city.yokosuka.kanagawa.jp/3130/hasseijoukyou.html","横須賀市内の新型コロナウイルス感染症患者の発生状況"),), 
                   
                   p("11月末までのデータはジャックジャパンのデータを使用しています。", br(),
                     tags$a(href="https://gis.jag-japan.com/covid19jp/","新型コロナウイルス感染者数マップ")), 
                   
                   p("本サイトでは、発表日を元に感染者の集計を行っています。",
                     br(),
                     p("データは18時～20時で更新を行っています。"),
                     "各サイトのデータをまとめたデータは以下", br(),
                     tags$a(href="https://github.com/tanamym/covid19_colopressmap_isehara/blob/main/data2020.csv","2020年までのデータ"), br(),
                     tags$a(href="https://github.com/tanamym/covid19_colopressmap_isehara/blob/main/data202106.csv","2021年1月から2021年6月までのデータ"), br(),
                     tags$a(href="https://github.com/tanamym/covid19_colopressmap_isehara/blob/main/data2021.csv","2021年7月以降")),
                   p("横浜市の居住地の内訳はこちら", br(),
                     tags$a(href="https://github.com/tanamym/covid19_colopressmap_isehara/blob/main/yoko_covid.csv","8月25日～")),
                   
                   p("横浜市の区別データは以下のサイトのデータを参照しています。", br(),
                     tags$a(href="https://square.umin.ac.jp/kenkono/","横浜市区別コロナデータ"), br(),
                     "＊1 横浜市が前週の新規感染者数の訂正をしたため、鶴見区と泉区の4月18日から4月27日の新規感染者数がマイナスになっています。", br(),
                     "＊2 横浜市が7月18日から7月24日のデータと7月25日から7月31日のデータをまとめて公表したため、その2週間のデータを半分にしたものを1週間のデータとしています。", br(),
                     "＊3 9月5日から9月11日の区ごとの新規感染者数には、8月29日から9月4日に調査中だった39人が含まれます。（横浜市全体の新規感染者数には含まれません。）", br(),
                     "＊4 横浜市が12月4日に12月10日までのデータを公表したため、11月28日から12月10日は他の週よりも1日短くなっています。", br(),
                     "＊5 横浜市が12月26日から12月31日のデータと1月1日から1月7日のデータをまとめて公表したため、その2週間のデータを半分にしたものを1週間のデータとしています。
                     市外とは、横浜市外に住み、横浜市内で検査して陽性となった人の合計です。"),
                   
                   p("地図表記およびに人口を考慮するために",
                     tags$a(href="https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-v2_4.html#prefecture14","平成31年の国土交通省の国土数値情報の行政区域データ"),"と", br(),
                     "神奈川県の市区町村（行政区も含む）の人口のデータ（2019年10月の人口推計より）",
                     tags$a(href="https://www.pref.kanagawa.jp/docs/x6z/tc30/jinko/kohyosiryo.html","神奈川県人口統計調査（月報）過去の公表資料"),"を使用しています。"),
                   br(),
                   br(),
                   br(),
                   "東海大学大学院理学研究科　棚橋真弓",
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br()
          ),
          
          tabPanel("推奨環境&使い方", value = "tab4",
                   h4(strong("PC推奨環境")),#strong 文字を太くする
                   h5("<OS>"),
                   p("Windows 10"),
                   h5("<ブラウザ>"),
                   p("Microsoft Edge,Google Chrome, Mozilla Firefox"),
                   p("当サイトでは、レイアウトの倍率を100％での利用を推奨しています。また、解像度は1024×768以上を推奨しています。"),
                   
                   h4(strong("使い方")),
                   p("・2021-04-29の感染者数を見たい場合"),
                   p("日付を2021-04-29に設定し、期間を1日に設定する。"),
                   p("・2021-05-01から2021-05-07までの一週間(7日間)の累積感染者数を見たい場合"),
                   p("日付を2021-05-07に設定し、期間を7日累積に設定する。"),
                   p("・10万人あたりの感染者数や10万人あたりの一週間の累積感染者数を見たい場合"),
                   p("新規感染者数の右隣のタブ(人口10万人あたりの新規感染者数)を左クリックしてください。"),
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
                   br()
          )
        ),
        width=12
      )
    }),
    tags$footer(
      a(rel="license",href="http://creativecommons.org/licenses/by/4.0/deed.ja",img(alt="クリエイティブ・コモンズ・ライセンス",style="border-width:0",src="https://i.creativecommons.org/l/by/4.0/88x31.png")),
      br(),
      "この作品は",
      a(rel="license",href="http://creativecommons.org/licenses/by/4.0/deed.ja","クリエイティブ・コモンズ 表示 4.0 国際 ライセンス"),
      "の下に提供されています。"
    )
  )
})

server <- shinyServer(function(input, output, session) {
  output$pl <-
    renderUI({
      switch (input$cd,
              "plot" = plotOutput("cdplot"),
              "line" = dygraphOutput("cdline")
      )
    })
  #追加2####
  output$pl3 <-
    renderUI({
      switch (input$cd,
              "plot" = (radioButtons("period",
                                     label = NULL,
                                     choices = list("1か月" = 1, "3か月" = 3,
                                                    "半年" = 6),#selected = 1,
                                     inline=T)
              ),
              "line" = br()#dygraphOutput("cdline")
      )
    })
  ####
  output$pl2 <-
    renderUI({
      switch (input$cd,
              "plot" = h4("市区町村ごとに直近の1か月分の結果を表示しています。"),
              "line" = h4("グラフの下部にあるバーを調整することで、表示する期間を設定できます。")
      )
    })
  v<-reactiveValues(tomo=0,next1=0,yest=0,back=0,ac=0,count=0 )
  
  observeEvent(input$tabset,{
    if(input$tabset=="tab2"&v$count==0){
      updateRadioButtons(inputId="y",
                         selected = 7)
      v$count=1
    }
  })
  
  observeEvent(input$yesterday|input$tomorrow|input$back|input$next1,{
    x=input$x
    
    if(v$yest<input$yesterday){
      v$yest<-input$yesterday
      x=max(x-1,min(date$Date))
      updateDateInput(inputId = "x",
                      value = x)
    }
    
    if(v$tomo<input$tomorrow){
      v$tomo<-input$tomorrow
      x<-min(x+1,max(date$Date))
      updateDateInput(inputId = "x",
                      value = x)
    }
    if(v$back<input$back){
      v$back<-input$back
      x=max(x-7,min(date$Date))
      updateDateInput(inputId = "x",
                      value = x)
    }
    if(v$next1<input$next1){
      v$next1<-input$next1
      x<-min(x+7,max(date$Date))
      updateDateInput(inputId = "x",
                      value = x)
    }
  })
  
  x=max(date$Date)
  y=7
  y=1
  
  observe({
    x=input$x
    y=as.numeric(input$y)
    z=1
    cn="nday"
    if(y==7){
      z=5
      cn="n7day"
    }
    
    par=colorNumeric(palette = c("#FFFFFF", "#FF0000","#FF0000","#800080","#800080","#4D004D","#4D004D","#4D004D","#000000"),domain = c(0,50*y*8))
    parj=colorNumeric(palette = c("#FFFFFF", "#FF0000","#FF0000","#800080","#800080","#4D004D","#4D004D","#4D004D","#000000"),domain = c(0,10*z*8))
    
    data7.1 <-
      CDS %>%
      rename(count=cn) %>%
      filter(Date %in% x) %>%
      mutate(countj=count/jinko*100000) %>%
      mutate(count_cl=ifelse(count>50*y,50*y,count)) %>%
      mutate(countj_cl=ifelse(countj>8*y,8*y,countj)) 
    
    data7.3 <-
      data7.1 %>%
      # filter(!is.na(X)) %>%
      summarise(Count=sum(count),Jinko=sum(jinko,na.rm = T)) %>%
      mutate(Countj=Count/Jinko*100000)
    
    data7.2 <-
      sp::merge(Shp %>%
                  mutate(N03_004=ifelse(is.na(N03_004),N03_003,N03_004)),
                data7.1, all=F,duplicateGeoms = TRUE)
    
    data7.4 <-
      data7.2 %>%
      filter(N03_004!="横浜市")
    
    output$covid_map <- renderPlot({
      p <-
        ggplot(data7.2) +
        geom_sf(aes(fill=count),color="gainsboro") +
        # geom_sf(data=data7.2 %>% filter(count>50*y*2),color="gainsboro",fill="purple") +
        geom_sf(data=data7.2 %>% filter(N03_004=="横浜市"),color="gray90") +
        # scale_fill_gradient(low = "white",high = "red",
        #                     breaks=seq(0,y*50,ifelse(y==1,10,50)),
        #                     limits=c(0,y*50)) +
        scale_fill_gradientn(colors = par(seq(0,max(data7.4$count),length=nrow(data7.4))),#) +
                             breaks=seq(0,max(data7.4$count),ifelse(y==1,50,100)),
                             limits=c(0,max(data7.4$count))) +
        geom_text(data = data7.1,
                  aes(x = X, y = Y,
                      label = paste0(N03_004,"\n",round(count,2),"人")),size=4) +
        geom_text(data = data7.1 %>% filter(is.na(X)),
                  aes(x = 139.32, y = 35.24,
                      label = paste0(N03_004," ",round(count,2),"人")),size=4) +
        geom_text(data = data7.3,
                  aes(x=139.65,y=35.66,label=paste("神奈川県全体\n",round(Count,2),"人")),size=6) +
        # geom_text(data=data7.3,aes(x=139.65,y=35.7,label=paste("(",y*50*2,"人以上の感染者数の場合、紫色で表示しています。)")),size=4)+
        coord_sf(datum = NA) +
        theme_void() +
        ggtitle(paste0(x-y+1," ~ ",x)) +
        theme(legend.background = element_rect(fill = "gray", size=10, colour="gray"),
              legend.title = element_blank())
      
      if(y==7)
        p <-
          p + ggtitle(paste0(x-y+1," ~ ",x," (7日間累積)"))
      
      if(max(data7.4$count)>50*y*4)
        p <-
          p +
          geom_text(data = data7.1 %>%
                      filter(N03_004!="横浜市") %>% 
                      filter(count>50*y*4),
                    aes(x=X,y=Y,
                        label=paste(N03_004,"\n",count,"人")),size=4,col="white")
      
      plot(p)
    })
    
    output$covid_map2 <- renderPlot({
      p <-
        ggplot(data7.2) +
        geom_sf(aes(fill=countj), color = "gainsboro") +
        # geom_sf(data=data7.2 %>% filter(countj>8*y*2),color="gainsboro",fill="purple") +
        # scale_fill_gradient(low = "white",high = "red",
        #                     breaks=seq(0,y*8,ifelse(y==1,1,10)),
        #                     limits=c(0,y*8)) +
        scale_fill_gradientn(colors = parj(seq(0,max(data7.2$countj),length=nrow(data7.2))),#) +
                             breaks=seq(0,max(data7.2$countj),ifelse(y==1,10,50)),
                             limits=c(0,max(data7.2$countj))) +
        geom_text(data = data7.1,
                  aes(x = X, y = Y,
                      label = paste0(N03_004,"\n",round(countj,2),"人")),size=4) +
        geom_text(data = data7.3,
                  aes(x=139.65,y=35.66,label=paste("神奈川県全体\n",round(Countj,2),"人")),size=6) +
        # geom_text(data=data7.3,aes(x=139.65,y=35.7,label=paste("(",y*8*2,"人以上の感染者数の場合、紫色で表示しています。)")),size=4)+
        coord_sf(datum = NA) +
        theme_void() +
        ggtitle(paste0(x-y+1," ~ ",x)) +
        theme(legend.background = element_rect(fill = "gray", size=10, colour="gray"),
              legend.title = element_blank())
      
      if(y==7)
        p <-
          p + ggtitle(paste0(x-y+1," ~ ",x," (7日間累積)"))
      
      if(max(data7.1$countj,na.rm = T)>10*z*4)
        p <-
          p +
          geom_text(data = data7.1 %>% 
                      filter(countj>10*z*4),
                    aes(x=X,y=Y,
                        label=paste(N03_004,"\n",round(countj,2),"人")),size=4,col="white")
      
      plot(p)
    })
  })
  
  observe({
    x=input$x
    y=7
    z=5
    
    par=colorNumeric(palette = c("#FFFFFF", "#FF0000","#FF0000","#800080","#800080","#4D004D","#4D004D","#4D004D","#000000"),domain = c(0,50*y*8))
    parj=colorNumeric(palette = c("#FFFFFF", "#FF0000","#FF0000","#800080","#800080","#4D004D","#4D004D","#4D004D","#000000"),domain = c(0,10*z*8))
    data1 <-
      data %>%
      filter(start<=x,x<=end)
    if(x>max(data$end))
      data1 <-
      data %>%
      filter(end==max(end))
    
    data2 <-
      data1 %>%
      mutate(N03_003="横浜市") %>%
      left_join(xy) %>%
      mutate(countj=count/jinko*100000) %>%
      mutate(count_cl=ifelse(count>50*7,50*7,count)) %>%
      mutate(countj_cl=ifelse(countj>8*7,8*7,countj)) 
    
    data3 <-
      data2 %>%
      filter(!is.na(X)) %>%
      summarise(start=max(start,na.rm = T),end=max(end,na.rm = T),Count=sum(count,na.rm = T),Jinko=sum(jinko,na.rm = T)) %>%
      mutate(Countj=Count/Jinko*100000)
    
    yoko_shp <-
      sp::merge(Shp, data2,
                all=F,duplicateGeoms = TRUE)
    
    output$yoko_map <- renderPlot({
      p <-
        ggplot(yoko_shp) +
        geom_sf(aes(fill=count),color="gainsboro") +
        # scale_fill_gradient(low = "white",high = "red",
        #                     breaks=seq(0,50*7,50),
        #                     limits=c(0,50*7)) +
        scale_fill_gradientn(colors = par(seq(0,max(yoko_shp$count),length=nrow(yoko_shp))),#) +
                             breaks=seq(0,max(yoko_shp$count),ifelse(y==1,10,50)),
                             limits=c(0,max(yoko_shp$count))) +
        geom_text(data=data2,
                  aes(x=X,y=Y,label=paste0(N03_004,"\n",round(count,2),"人"))) +
        geom_text(data=data2 %>% filter(is.na(X)),
                  aes(x=139.5,y=35.34,label=paste0(N03_004," ",round(count,2),"人"))) +
        geom_text(data=data3,
                  aes(x=139.65,y=35.58,label=paste("横浜市全体\n",round(Count,2),"人")),size=6) +
        coord_sf(datum = NA) +
        theme_void() +
        ggtitle(paste0(data3$start," ~ ",data3$end," (7日間累積)")) +
        theme(legend.background = element_rect(fill = "gray",size=10,colour="gray"),
              legend.title = element_blank())
      
      if(max(data2$count)>50*y*4)
        p <-
          p +
          geom_text(data = data2 %>% 
                      filter(count>50*y*4),
                    aes(x=X,y=Y,
                        label=paste(N03_004,"\n",count,"人")),size=4,col="white")
      
      plot(p)
    })
    
    output$yoko_map2 <- renderPlot({
      p <-
        ggplot(yoko_shp) +
        geom_sf(aes(fill=countj),color="gainsboro") +
        # scale_fill_gradient(low = "white",high = "red",
        #                     breaks=seq(0,8*7,10),
        #                     limits=c(0,8*7)) +
        scale_fill_gradientn(colors = parj(seq(0,max(yoko_shp$countj),length=nrow(yoko_shp))),#) +
                             breaks=seq(0,max(yoko_shp$countj),ifelse(y==1,10,50)),
                             limits=c(0,max(yoko_shp$countj))) +
        geom_text(data=data2,
                  aes(x=X,y=Y,label=paste0(N03_004,"\n",round(countj,2),"人"))) +
        geom_text(data=data3,
                  aes(x=139.65,y=35.58,label=paste("横浜市全体\n",round(Countj,2),"人")),size=6) +
        coord_sf(datum = NA) +
        theme_void() +
        ggtitle(paste0(data3$start," ~ ",data3$end," (7日間累積)")) +
        theme(legend.background = element_rect(fill = "gray",size=10,colour="gray"),
              legend.title = element_blank())
      
      if(max(data2$countj,na.rm = T)>10*z*4)
        p <-
          p +
          geom_text(data = data2 %>% 
                      filter(countj>10*z*4),
                    aes(x=X,y=Y,
                        label=paste(N03_004,"\n",round(countj,2),"人")),size=4,col="white")
      
      plot(p)
    })
  })
  
  vals<-reactiveValues(th=0,kt=0,cb=0,kk=0,cg=0,sk=0,ks=0)
  
  observeEvent(input$chiho, ignoreNULL = F, {
    dc=input$dc
    
    if(sum(input$chiho == "横浜・川崎")){
      vals$th=1
      dc=union(dc,levels(CDS$N03_004)[1:8])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
    if(sum(input$chiho == "横浜・川崎")==0&vals$th==1){
      vals$th=0
      dc=setdiff(dc,levels(CDS$N03_004)[1:8])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
    if(sum(input$chiho == "横須賀・三浦")){
      vals$kt=1
      dc=union(dc,levels(CDS$N03_004)[c(10,12,16,17,26)])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
    if(sum(input$chiho == "横須賀・三浦")==0&vals$kt==1){
      vals$kt=0
      dc=setdiff(dc,levels(CDS$N03_004)[c(10,12,16,17,26)])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
    if(sum(input$chiho == "県央")){
      vals$cb=1
      dc=union(dc,levels(CDS$N03_004)[c(9,19,20,22,23,25,38,39)])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
    if(sum(input$chiho == "県央")==0&vals$cb==1){
      vals$cb=0
      dc=setdiff(dc,levels(CDS$N03_004)[c(9,19,20,22,23,25,38,39)])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
    if(sum(input$chiho == "湘南")){
      vals$kk=1
      dc=union(dc,levels(CDS$N03_004)[c(11,13,15,18,21,27,28,29)])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
    if(sum(input$chiho == "湘南")==0&vals$kk==1){
      vals$kk=0
      dc=setdiff(dc,levels(CDS$N03_004)[c(11,13,15,18,21,27,28,29)])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
    if(sum(input$chiho == "県西")){
      vals$cg=1
      dc=union(dc,levels(CDS$N03_004)[c(14,24,30:37)])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
    if(sum(input$chiho == "県西")==0&vals$cg==1){
      vals$cg=0
      dc=setdiff(dc,levels(CDS$N03_004)[c(14,24,30:37)])
      updateCheckboxGroupInput(inputId = "dc",
                               selected = dc)
    }
  })
  
  observe({
    dc=DC$N03_004
    dc=input$dc
    #追加####
    y="1"
    y=input$y
    ####
    #追加3####
    pe=as.numeric(input$period)
    
    if(is.null(input$period)){
      pe=1
    }
    print(input$period)
    ####
    CDS2 <-
      CDS %>%
      filter(N03_004 %in% dc) %>%
      #mutate(n=ifelse(y=="1",ndayj,n7dayj))%>%
      #変更・追加####
      mutate(n=switch (y,
                       "1" = ndayj,
                       "7" = n7dayj
      ))%>%
      select(JTime,N03_004,n) %>%####
      spread(N03_004,n) %>%
      select(-JTime)
    
    nc=ceiling(sqrt(length(dc)))

    
    date2 <-
      date %>%
      mutate(df=day(Date)<=day(max(Date))) %>%
      mutate(cm=cumsum(df)) %>%
      mutate(f=(day(max(Date))+1)*pe)%>%#追加####
      filter(cm<=f)#変更####
    
    CDS3 <-
      CDS %>%
      filter(N03_004 %in% dc) %>%
      filter(Date %in% date2$Date)%>%
      #追加####
      mutate(n=switch (y,
                       "1" = ndayj,
                       "7" = n7dayj
      ))####
    
    output$cdline <- renderDygraph({
      rownames(CDS2) <- unique(CDS$JTime)
      dygraph(CDS2,main = switch (y,#変更####
                                  "1" = paste0("人口10万人あたりの",y,"日の感染者数(市区町村別)"),
                                  "7" = paste0("人口10万人あたりの",y,"日間累積感染者数(市区町村別)")
      ),group = "dyg") %>%####
        dyOptions(stackedGraph = F, drawPoints = F, pointSize = 10, strokeWidth = 3,fillAlpha = 0.5,
                  colors = ggColorHue(ncol(CDS2)),
                  axisLabelFontSize = 15,axisLabelWidth = 100,titleHeight = 30 ,logscale=F) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 1) %>%
        dyLegend(width = "130", labelsSeparateLines = T) %>%
        dyShading(from = "2020-4-7", to = "2020-5-25", color = "#FFE6E6") %>%
        dyShading(from = "2021-1-8", to = "2021-3-21", color = "#FFE6E6") %>%
        dyShading(from = "2021-4-25", to = "2021-6-20", color = "#FFE6E6") %>%
        dyShading(from = "2021-8-2", to = "2021-9-12", color = "#FFE6E6") %>%
        dyUnzoom() %>%
        dyAxis("y",axisLabelWidth=20) %>%
        # dyCrosshair(direction = "both") %>%
        dyRangeSelector(height = 50,keepMouseZoom = T,dateWindow = c(min(CDS$Date),max(CDS$Date)),retainDateWindow = T)
    })
    
    output$cdplot<-renderPlot({
      ziku=floor(max(CDS3$n,na.rm = T)/40)*10#変更####
      CDS3%>%
        ggplot(aes(x=Date,y=n,color=N03_004)) +#変更####
        geom_line(size=2) +
        geom_text(data=CDS3 %>% filter(Date==max(Date)),
                  aes(y=n+5,label=round(n,2)),size=5) +#変更####
        #変更####
        scale_x_date(date_breaks = switch(input$period,
                                          "1" = "1 week",
                                          "3" = "3 week",
                                          "6" = "5 week"),
                     date_labels = "%m/%d",
                     date_minor_breaks = switch(input$period,
                                                "1" = "1 days",
                                                "3" = "1 week",
                                                "6" = "1 week")) +
        ####
        scale_y_continuous(breaks = seq(0,1000,ziku)) +
        facet_wrap(~N03_004,ncol=nc)+ #,scales = "free_y"
        labs(x="",y="",color="市区町村")+
        #変更####
        ggtitle(switch (y,
                        "1" = paste0("人口10万人あたりの",y,"日の感染者数(市区町村別)"),
                        "7" = paste0("人口10万人あたりの",y,"日間累積感染者数(市区町村別)")
        )) +####
        theme(text = element_text(size=20)) +
        theme(legend.position = "none")
    })
  })
  
})

shinyApp(ui = ui, server = server)
