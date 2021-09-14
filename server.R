library(dplyr)
library(sf)
library(stringr)
library(lubridate)
library(tidyr)
library(htmltools)
library(sp)
library(data.table)
library(curl)

load("Dataset.RData")

shinyServer(function(input, output, session) {
 	data2020 <-
    	fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2020.csv",encoding="UTF-8")
  
  	data202106 <-
    	fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data202106.csv",encoding="UTF-8")
  
 	data2021 <-
    	fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/data2021.csv",encoding="UTF-8")
  	ycd <-
    	fread("https://raw.githubusercontent.com/tanamym/covid19_colopressmap_isehara/main/yoko_covid.csv",encoding="UTF-8") %>%
    	mutate(Fixed_Date=as.Date(Date),
           	Residential_City=City)
  	data7 <-
    	rbind(data2020,data202106,data2021) %>%
    	mutate(Fixed_Date=as.Date(Fixed_Date)) %>%
    	arrange(desc(Fixed_Date),Hos,hos)%>%
    	count(Fixed_Date,Residential_City,hos)%>%
    	full_join(ycd)%>%
    	mutate(n=ifelse(is.na(n),count,n))
 	date <- 
    	data.frame(Date=min(data7$Fixed_Date):max(data7$Fixed_Date)) %>%
    	arrange(desc(Date)) %>%
    	mutate(Date=as.Date(Date,origin="1970-01-01")) %>%
    	filter(Date>="2020-04-20")

    
    yoko<-
        # read.csv("https://square.umin.ac.jp/kenkono/csv/ward-new.csv", encoding = "UTF-8", header = F)
        fread("https://square.umin.ac.jp/kenkono/csv/ward-new.csv", encoding = "UTF-8",header = F)
    yoko2<-
        yoko%>%
        filter(V1!="",V1!="区名")%>%
        tidyr::pivot_longer(-V1,
                            names_to = "V",
                            values_to="count")%>%
        rename("name"="V1")
    yoko3<-
        yoko%>%
        filter(V1=="")%>%
        tidyr::pivot_longer(-V1,
                            names_to="V",
                            values_to="year")%>%
        select(-V1)
    yoko4<-
        yoko%>%
        filter(V1=="区名")%>%
        tidyr::pivot_longer(-V1,
                            names_to="V",
                            values_to="date")%>%
        select(-V1)
    data<-
        left_join(yoko3,yoko4)%>%
        left_join(yoko2)%>%
        filter(!name%in%c("日本","横浜市","市外","調査中","神奈川県"))%>%
        rename("N03_004"="name")%>%
        mutate(count=as.numeric(as.character(count)))%>%
        #filter(date=="4/16~4/22")%>%
        mutate(N03_003="横浜市")%>%
        mutate(start=str_replace(date,"~.+",""),
               end=str_replace(date,".*~",""))%>%
        mutate(end=str_replace(end," .+",""))%>%
        mutate(year2=str_replace(year,"年",""))%>%
        # mutate(start=as.Date(paste0(year2,"/",start))) %>%
        # mutate(end=as.Date(paste0(year2,"/",end))) %>%
        mutate(start=str_replace(start,"/","-"),
               end=str_replace(end,"/","-"),
               start=paste0(year2,"-",start),
               end=paste0(year2,"-",end),
               start=as.Date(start),
               end=as.Date(end))
      # mutate(date=as.character(date)) %>%
      # mutate(count=as.numeric(as.character(count)))
    
    output$update<-
        renderUI({
            h5(paste0("2020-04-21記者発表資料から",date[1,1],"記者発表資料掲載分まで集計しています。"))
        })
    output$date<-
        renderUI({
            dateInput("x",
                      label = h5("日付を選択してください"),
                      min = "2020-04-21",
                      max = date[1,1],
                      value = date[1,1])
        })
    
    output$covid_map <- renderLeaflet({
        leaflet() %>%
            fitBounds(lng1=MXY[[1]], lat1=MXY[[2]], lng2=MXY[[3]], lat2=MXY[[4]]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            #addTiles() %>%
            # addMarkers(139.274823,35.365831, label = "東海大学湘南キャンパス") %>%
            # addMarkers(139.313644,35.407144, label = "東海大学伊勢原キャンパス") %>%
            addPolygons(data=shp, layerId = ~ID, fillOpacity = 1, weight = 1, color = "#666", fillColor = "white")#,
        # highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE))
    })
    
    output$yoko_map<-renderLeaflet({
        leaflet() %>%
            fitBounds(lng1=MXY2[[1]], lat1=MXY2[[2]], lng2=MXY2[[3]], lat2=MXY2[[4]]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(data=shp2, layerId = ~ID, fillOpacity = 1, weight = 1, color = "#666", fillColor = "white")
    })

    output$covid_map2 <- renderLeaflet({
        leaflet() %>%
            fitBounds(lng1=MXY[[1]], lat1=MXY[[2]], lng2=MXY[[3]], lat2=MXY[[4]]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            # addMarkers(139.274823,35.365831, label = "東海大学湘南キャンパス") %>%
            # addMarkers(139.313644,35.407144, label = "東海大学伊勢原キャンパス") %>%
            addPolygons(data=shp, layerId = ~ID, fillOpacity = 1, weight = 1, color = "#666", fillColor = "white")
        # %>%
        #     addControl(tags$div(HTML(paste("更新ボタンを再度押してください")))  , position = "topright")
    })
    
    output$yoko_map2<-renderLeaflet({
        leaflet() %>%
            fitBounds(lng1=MXY2[[1]], lat1=MXY2[[2]], lng2=MXY2[[3]], lat2=MXY2[[4]]) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(data=shp2, layerId = ~ID, fillOpacity = 1, weight = 1, color = "#666", fillColor = "white")
    })

    LD <- eventReactive(input$button1,ignoreNULL = FALSE, ignoreInit = FALSE,{
        x=input$x
        y=input$y
        color1=input$color1
        color2=input$color2
        if(is.null(x)){
            x=date$Date[1]
            y=7
        }
        
        print(x)
        date2=ymd(x)
        date1=date2-y+1
        
        data7.1<-
            data7%>%
            dplyr::filter(Fixed_Date>=date1,
                          Fixed_Date<=date2)%>%
            dplyr::group_by(Residential_City)%>%
            summarise(count1=sum(n))%>%
            ungroup() %>%
            full_join(City)%>%
            mutate(count1=ifelse(is.na(count1),0,count1))%>%
            mutate(count2=ifelse(count1>=color1*y,color1*y,count1))%>%
            #mutate(count7=ifelse(is.na(count7),0,count7))%>%
            dplyr::mutate(col1=pal(count2),
                          #col12=ifelse(count1>input$color2*input$y,"red",col1),
                          #col12=ifelse(Residential_City=="横浜市","gray",col1)
                          )%>%
            # dplyr::mutate(col7=pal2(count7),
            #               col72=ifelse(count7>300*as.numeric(7),"red",col7),
            #               col72=ifelse(Residential_City=="横浜市","gray",col72)) %>%
            left_join(jinko,by=c("Residential_City"="City")) %>%
            mutate(count_j1=count1/jinko*100000)%>%
            mutate(count_j2=ifelse(count_j1>=color2*y,color2*y,count_j1))%>%
            #mutate(count_j7=count7/jinko*100000)%>%
            filter(!is.na(count_j2))%>%
            dplyr::mutate(col_j1=pal3(count_j2),
                          #col_j12=ifelse(count_j1>input$color2*input$y,"red",col_j1),
                          #col_j12=ifelse(Residential_City=="横浜市","gray",col_j1)
                          )%>%
            # dplyr::mutate(col_j7=pal4(count_j7),
            #               col_j72=ifelse(count_j1>8*as.numeric(7),"red",col_j7),
            #               col_j72=ifelse(Residential_City=="横浜市","gray",col_j72)) %>%
            rename(N03_004=Residential_City)
        
        data7.2<-
            sp::merge(shp, data7.1,
                      by="N03_004", all=F,duplicateGeoms = TRUE) %>%
            mutate(date1) %>%
            mutate(date2)
        return(data7.2)
    })
    st <- reactiveValues(counter = 0)
    observeEvent(input$button1|st$counter==0,ignoreNULL = FALSE, ignoreInit = FALSE,{
        y=input$y
        data7.2=LD()
        date1=unique(data7.2$date1)
        date2=unique(data7.2$date2)
        pal  <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(y)*input$color1), reverse=F)
        pal3 <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(y)*input$color2), reverse=F)
        leafletProxy("covid_map",data=data7.2) %>%
                clearControls() %>%
                # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                removeShape(layerId=paste0("P",1:nrow(data7.2))) %>%
                addPolygons(layerId=paste0("P",1:nrow(data7.2)),
                            label = paste0(data7.2$N03_004," ",data7.2$count1,"人"),
                            labelOptions = labelOptions(textsize = "15px"),
                            opacity = 0,
                            fillOpacity = 0) %>%
                setShapeStyle(layerId = ~ID,
                              fillColor = ~pal(count2)) %>%
                addLegend(pal=pal,
                          values = c(0,input$y*input$color1),
                          position="bottomright",#color=~col2,labels=~count,
                          opacity = 1) %>%
                addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")

            leafletProxy("covid_map2",data=data7.2) %>%
                clearControls() %>%
                # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                addPolygons(label = paste0(data7.2$N03_004," ",round(data7.2$count_j1,2),"人"),
                            labelOptions = labelOptions(textsize = "15px"),
                            opacity = 0,
                            fillOpacity = 0) %>%
                setShapeStyle(layerId = ~ID,
                              fillColor = ~pal(count_j2)) %>%
                addLegend(pal=pal3,
                          values = c(0,input$y*input$color2),
                          position="bottomright",#color=~col2,labels=~count,
                          opacity = 1) %>%
                addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")

    })
    
    LD_yoko <- eventReactive(input$button1,ignoreNULL = FALSE, ignoreInit = FALSE,{
        x=input$x
        if(is.null(x)){
            x=date$Date[1]
        }
        #x="2021-07-12"
        date2=ymd(x)
        y=input$y
        #y=14
        date1=date2-y+1
        data0<-data%>%
          mutate(flag=ifelse(date2<=end,1,ifelse(end<date2,2,0)))%>%
          filter(as.numeric(flag)>0)%>%
          arrange(flag)
        
        if(data0[1,10]==1){
          data0<-#data0%>%filter(date2<=end,start<=date2)%>%
            data0%>%filter((date2<=end&start<=date2)|start>=date1)%>%
            #group_by(N03_004,N03_003,start,end)%>%
              # summarise(count=sum(count))%>%
              # ungroup%>%
              group_by(N03_004,N03_003)%>%
              mutate(
                # start2=lag(start),
                #      start2=ifelse(is.na(start2),start,start2),
                #      start2=as.Date(start2,origin="1970-01-01"),
                #      end2=lead(end),
                #      end2=ifelse(is.na(end2),end,end2),
                #      end2=as.Date(end2,origin="1970-01-01")
                start2=min(start),
                end2=max(end)
                     )%>%
              group_by(N03_004,N03_003,start2,end2)%>%
              summarise(count=sum(count))%>%
              left_join(jinko,by=c("N03_004"="City")) %>%
              mutate(count_j=count/jinko*100000)

        }else{
          if(data0[1,10]==2){
          data0<-
            #data0%>%mutate(max=max(end))%>%filter(end==max)%>%
            data0%>%mutate(max=max(end))%>%filter(end==max|start>=max-y)%>%
            # summarise(count=sum(count))%>%
            # ungroup%>%
            # group_by(N03_004,N03_003)%>%
            mutate(
              # start2=lag(start),
              #      start2=ifelse(is.na(start2),start,start2),
              #      start2=as.Date(start2,origin="1970-01-01"),
              #      end2=lead(end),
              #      end2=ifelse(is.na(end2),end,end2),
              #      end2=as.Date(end2,origin="1970-01-01")
              start2=min(start),
              end2=max(end)
            )%>%
            group_by(N03_004,N03_003,start2,end2)%>%
            summarise(count=sum(count))%>%
            left_join(jinko,by=c("N03_004"="City")) %>%
            mutate(count_j=count/jinko*100000)
        }}
        # data0<-data %>%
        #     dplyr::filter(end<=date2,
        #                   end>=date1)%>%
        #     group_by(N03_004,N03_003,start,end)%>%
        #     summarise(count=sum(count))%>%
        #     ungroup%>%
        #     group_by(N03_004,N03_003)%>%
        #     mutate(start2=lag(start),
        #            start2=ifelse(is.na(start2),start,start2),
        #            start2=as.Date(start2,origin="1970-01-01"),
        #            end2=lead(end),
        #            end2=ifelse(is.na(end2),end,end2),
        #            end2=as.Date(end2,origin="1970-01-01")
        #            )%>%
        #     group_by(N03_004,N03_003,start2,end2)%>%
        #     summarise(count=sum(count))%>%
        #     left_join(jinko,by=c("N03_004"="City")) %>%
        #     mutate(count_j=count/jinko*100000)

        data1 <-
            sp::merge(shp2,data0%>%
                        mutate(count2=ifelse(count>=input$color1*y,input$color1*y,count))%>%
                        mutate(count_j2=ifelse(count_j>=input$color2*y,input$color2*y,count_j)),
                      by=c("N03_003","N03_004"), all=F,duplicateGeoms = TRUE) %>%
            mutate(date1) %>%
            mutate(date2)
        return(data1)
    })
    st2 <- reactiveValues(counter = 0)
    observeEvent(input$button1|st2$counter==0,ignoreNULL = FALSE, ignoreInit = FALSE,{
        data1=LD_yoko()
        date1=unique(data1$start2)
        date2=unique(data1$end2)
        pal2 <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(input$y)*input$color1), reverse=F)
        pal4 <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(input$y)*input$color2), reverse=F)
        leafletProxy("yoko_map",data=data1) %>%
            clearControls() %>%
            # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
            # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
            # removeShape(layerId=paste0("P",1:nrow(data7.2))) %>%
            addPolygons(layerId=paste0("P",1:nrow(data1)),
                        label = paste0(data1$N03_004," ",data1$count,"人"),
                        labelOptions = labelOptions(textsize = "15px"),
                        opacity = 0,
                        fillOpacity = 0) %>%
            setShapeStyle(layerId = ~ID,
                          fillColor = ~pal2(count2)) %>%
            addLegend(pal=pal2,
                      values = c(0,input$y*input$color1),
                      position="bottomright",#color=~col2,labels=~count,
                      opacity = 1) %>%
            addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")

        leafletProxy("yoko_map2",data=data1) %>%
            clearControls() %>%
            # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
            # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
            # removeShape(layerId=paste0("P",1:nrow(data7.2))) %>%
            addPolygons(layerId=paste0("P",1:nrow(data1)),
                        label = paste0(data1$N03_004," ",round(data1$count_j,2),"人"),
                        labelOptions = labelOptions(textsize = "15px"),
                        opacity = 0,
                        fillOpacity = 0) %>%
            setShapeStyle(layerId = ~ID,
                          fillColor = ~pal4(count_j)) %>%
            addLegend(pal=pal4,
                      values = c(0,input$y*input$color2),
                      position="bottomright",#color=~col2,labels=~count,
                      opacity = 1) %>%
            addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")
    })

    observe({
        # input$button1
        # input$y
        
        if(input$onoff){
            leafletProxy("covid_map") %>%
                addPolylines(data=rosen,color = "black",weight = 1,
                             layerId=paste0("X",1:nrow(rosen))) %>%
                addPolylines(data=tetudo,
                             color = ~pal5(ln),
                             opacity = 1,
                             label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                             labelOptions = labelOptions(textsize = "15px"),
                             layerId=paste0("Y",1:nrow(tetudo)),
                )
            leafletProxy("covid_map2") %>%
                addPolylines(data=rosen,color = "black",weight = 1,
                             layerId=paste0("X",1:nrow(rosen))) %>%
                addPolylines(data=tetudo,
                             color = ~pal5(ln),
                             opacity = 1,
                             label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                             labelOptions = labelOptions(textsize = "15px"),
                             layerId=paste0("Y",1:nrow(tetudo)),
                )
            leafletProxy("yoko_map") %>%
                addPolylines(data=rosen,color = "black",weight = 1,
                             layerId=paste0("X",1:nrow(rosen))) %>%
                addPolylines(data=tetudo,
                             color = ~pal5(ln),
                             opacity = 1,
                             label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                             labelOptions = labelOptions(textsize = "15px"),
                             layerId=paste0("Y",1:nrow(tetudo)),
                )
            leafletProxy("yoko_map2") %>%
                addPolylines(data=rosen,color = "black",weight = 1,
                             layerId=paste0("X",1:nrow(rosen))) %>%
                addPolylines(data=tetudo,
                             color = ~pal5(ln),
                             opacity = 1,
                             label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                             labelOptions = labelOptions(textsize = "15px"),
                             layerId=paste0("Y",1:nrow(tetudo)),
                )
        }
        else{
            leafletProxy("covid_map") %>%
                removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                removeShape(layerId=paste0("Y",1:nrow(tetudo)))
            leafletProxy("covid_map2") %>%
                removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                removeShape(layerId=paste0("Y",1:nrow(tetudo)))
            leafletProxy("yoko_map") %>%
                removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                removeShape(layerId=paste0("Y",1:nrow(tetudo)))
            leafletProxy("yoko_map2") %>%
                removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                removeShape(layerId=paste0("Y",1:nrow(tetudo)))
        }
    })
    
    vals <- reactiveValues(counter = 0)
    observe({
        pal3 <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(input$y)*input$color2), reverse=F)
        if(isolate(vals$counter)==0&input$tabset=="tab2"){
            vals$counter=1
            y=input$y
            data7.2=LD()
            date1=unique(data7.2$date1)
            date2=unique(data7.2$date2)
                leafletProxy("covid_map2",data=data7.2) %>%
                    clearControls() %>%
                    # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                    # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                    addPolygons(label = paste0(data7.2$N03_004," ",round(data7.2$count_j1,2),"人"),
                                labelOptions = labelOptions(textsize = "15px"),
                                opacity = 0,
                                fillOpacity = 0) %>%
                    setShapeStyle(layerId = ~ID,
                                  fillColor = ~pal(count_j2)) %>%
                    addLegend(pal=pal3,
                              values = c(0,input$y*input$color2),
                              position="bottomright",#color=~col2,labels=~count,
                              opacity = 1) %>%
                    addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")
      
                
            
            data1=LD_yoko()
            date1=unique(data1$start2)
            date2=unique(data1$end2)
            pal4 <- colorNumeric(palette=c("white","red"),domain=c(0,as.numeric(input$y)*input$color2), reverse=F)
            leafletProxy("yoko_map2",data=data1) %>%
                clearControls() %>%
                # removeShape(layerId=paste0("X",1:nrow(rosen))) %>%
                # removeShape(layerId=paste0("Y",1:nrow(tetudo))) %>%
                # removeShape(layerId=paste0("P",1:nrow(data7.2))) %>%
                addPolygons(layerId=paste0("P",1:nrow(data1)),
                            label = paste0(data1$N03_004," ",round(data1$count_j,2),"人"),
                            labelOptions = labelOptions(textsize = "15px"),
                            opacity = 0,
                            fillOpacity = 0) %>%
                setShapeStyle(layerId = ~ID,
                              fillColor = ~pal4(count_j2)) %>%
                addLegend(pal=pal4,
                          values = c(0,input$y*input$color2),
                          position="bottomright",#color=~col2,labels=~count,
                          opacity = 1) %>%
                addControl(tags$div(HTML(paste(date1,date2,sep = "~")))  , position = "topright")
            
            if(input$onoff){
                leafletProxy("covid_map2") %>%
                    addPolylines(data=rosen,color = "black",weight = 1,
                                 layerId=paste0("X",1:nrow(rosen))) %>%
                    addPolylines(data=tetudo,
                                 color = ~pal5(ln),
                                 opacity = 1,
                                 label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                                 labelOptions = labelOptions(textsize = "15px"),
                                 layerId=paste0("Y",1:nrow(tetudo)))
                leafletProxy("yoko_map2") %>%
                    addPolylines(data=rosen,color = "black",weight = 1,
                                 layerId=paste0("X",1:nrow(rosen))) %>%
                    addPolylines(data=tetudo,
                                 color = ~pal5(ln),
                                 opacity = 1,
                                 label = paste(tetudo$N02_004,tetudo$N02_003,tetudo$N02_005),
                                 labelOptions = labelOptions(textsize = "15px"),
                                 layerId=paste0("Y",1:nrow(tetudo)))
            }
        }
    })
})
