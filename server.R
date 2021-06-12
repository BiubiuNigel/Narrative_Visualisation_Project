# R- SHiny narrative visaulization project - server 
# FIT5147
# Author: Yixiao CHen
# Student ID: 31968376
# Date: 2021.6.5

# Import library
library(shiny)
library(DT)
library(tidyverse)
library(dplyr)
library(imputeTS)
library(leaflet)
library(ggplot2)

#Read file
nsw_sec <- read_csv('nsw_sec_schools.csv')
nsw_pri <- read_csv('nsw_pri_schools.csv')
property <- read_csv('aus-property-sales-sep2018-april2020.csv')
property_nsw <- read_csv("aus-property-sales-sep2018-april2020_new_date.csv")

#Remove not necessary column
nsw_pri <- select(nsw_pri,-c("School Name","Language Background Other Than English (%)", "Postcode", "Campus Type", "Full Time Equivalent Teaching Staff","Full Time Equivalent Non-Teaching Staff","Girls Enrolments",
                             "Boys Enrolments","BE Score","Full Time Equivalent Enrolments","Indigenous Enrolments (%)"))
nsw_sec <- select(nsw_sec,-c("School Name", "Postcode","BE Score", "Campus Type", "Full Time Equivalent Teaching Staff","Full Time Equivalent Non-Teaching Staff","Girls Enrolments",
                             "Boys Enrolments","HSC Students","Language Background Other Than English (%)","Exams Sat","Full Time Equivalent Enrolments","Indigenous Enrolments (%)"))

#Select only in NSW
property_nsw <- subset(property_nsw,state == "NSW") 

# Remove the null value in property_nsw 
property_nsw[is.na(property_nsw$lon),] %>% nrow() 
property_nsw <- property_nsw[!is.na(property_nsw$lon),]

# remove the null value in primary school
nsw_pri <- drop_na(nsw_pri)

# remove null value in secondary school and keep DA 

nsw_sec <- nsw_sec[!(is.na(nsw_sec$ICSEA)),]
nsw_sec <- nsw_sec[!is.na(nsw_sec$`Governing Body`),]
nsw_sec <- nsw_sec[!is.na(nsw_sec$`Non-Teaching Staff`),]

#Modify the name of Year
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '10-Dec'] <- '10-12'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '2-Dec'] <- '2-12'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '3-Dec'] <- '3-12'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '3-Jul'] <- '3-7'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '5-Dec'] <- '5-12'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '7-Aug'] <- '7-8'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '7-Dec'] <- '7-12'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '7-Nov'] <- '7-11'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '7-Oct'] <- '7-10'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '11-Dec'] <- '11-12'
nsw_sec$`Year Range`[nsw_sec$`Year Range` == '2-Apr'] <- '2-4'


#Remove outlier
property_nsw <- property_nsw[-65142,]
property_nsw <- select(property_nsw, -c("loc_pid","lga_pid"))

#group by suburb
property_nsw$'suburb' <- as.factor(property_nsw$'suburb')


# Property sale every year for each month
property_month <- property_nsw %>% dplyr::group_by(date_sold_year,date_sold_month) %>% dplyr::summarise(count = n()) %>% as.data.frame()

#group by suburb
property_suburb_price <- property_nsw%>%dplyr::select(`date_sold_year`,
                                               `suburb`,
                                               `price`,
                                               `lat`,
                                               `lon`)
property_sp_data <- group_by(property_suburb_price,suburb) %>% dplyr::summarise(count = n(), 
                                                                         ave_sales = mean(price),
                                                                         sum_sales = sum(price),
                                                                         lat = mean(lat),
                                                                         lon = mean(lon))
property_sp_data <- property_sp_data[property_sp_data$count > 33,]


# Deal with the primary school data
nsw_pri <- drop_na(nsw_pri)

nsw_pri_data <- nsw_pri%>%dplyr::select(`ACARA ID`,`Suburb`,`Latitude`,`Longitude`,
                                 `Governing Body`,`ICSEA`,`Top SEA Quarter (%)`,
                                 `Teaching Staff`,`Total Enrolments`)
names(nsw_pri_data)[names(nsw_pri_data) == "Top SEA Quarter (%)"] <- "Top SEA"


# Deal with the secondary data

nsw_sec_data <- nsw_sec%>%dplyr::select(`ACARA ID`,`Suburb`,`Latitude`,`Longitude`,
                                 `% of exams sat that received DA`,`Governing Body`,
                                 `ICSEA`,`Top SEA Quarter (%)`,`Teaching Staff`,`Total Enrolments`)

names(nsw_sec_data)[names(nsw_sec_data) == "% of exams sat that received DA"] <- "DA(%)"
names(nsw_sec_data)[names(nsw_sec_data) == "Top SEA Quarter (%)"] <- "Top SEA(%)"


# Define server logic required to draw
shinyServer(function(input, output,session) {

    session$allowReconnect("force")
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    # Get current DATE
    output$currentDate <- renderUI({
        invalidateLater(20 * 1000, session)  # invalidateLater causes this output to automatically become invalidated every 20 seconds
        
        input$bus_refresh  # also update when refresh button is clicked
        
        h2(paste0("Current Date: ", format(Sys.Date())))  # Return current time
    })
    
    #Draw circular plot
    output$circular_plot <- renderPlot({
       
        property_av_50 <- property_sp_data[order(property_sp_data$ave_sales,decreasing = T),][1:input$top_property,]
        property_count_50 <- property_sp_data[order(property_sp_data$sum_sales,decreasing = T),][1:input$top_property,] 
        
        #dealing with top 50 average sale (circular boxplot)
        angle_ave_50 <- 90-360*(as.numeric(rownames(property_av_50))-0.5)/nrow(property_av_50)

        #Set the circular degree
        property_av_50$hjust <- ifelse(angle_ave_50 < -90,1,0)
        property_av_50$angle <- ifelse(angle_ave_50 < -90, angle_ave_50+180,angle_ave_50)

        
        # If the user select average sale price ranking
        if(input$pro_ave_sum == 'Average'){
            plot_data = property_av_50
            y_axis = property_av_50$ave_sales
            
            #Draw the circular plot
            ggplot(plot_data,aes(x = as.numeric(rownames(plot_data)), y = y_axis)) +
                geom_bar(stat = "identity",fill = alpha("lightgreen",0.7)) +
                theme_minimal() +
                theme(
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank(),
                    plot.margin = unit(rep(-1,4), "cm")     
                ) +
                ylim(-1797900,3751220) +
                coord_polar() +
                geom_text(data=plot_data, aes(x=as.numeric(rownames(plot_data)), y=round(ave_sales/10000,2), label=suburb, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=nrow(plot_data)*-0.05+5, angle= plot_data$angle, inherit.aes = FALSE )+
                geom_text(data=plot_data, aes(x=as.numeric(rownames(plot_data)), y=ave_sales, label=round(ave_sales/10000,2), hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4.5, angle= plot_data$angle, inherit.aes = FALSE )
        
        # If user select total sale price ranking    
        }else{
            plot_data = property_count_50
            # Use bar plot instead
            # Set to transparent
            ggplot(plot_data,aes(x = suburb, y = round(sum_sales/1000000,2))) +
                geom_bar(stat = "identity",fill = alpha("lightgreen",0.7)) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle=90,hjust=1))+
                geom_text(aes(label = round(sum_sales/1000000,2)),vjust=-0.3,check_overlap = TRUE)+
                labs(x = 'Suburbs',y = 'Total Sale (Million dollors)')
        }
        
       
    },bg="transparent")
    
    # Draw the map
    output$property_map <- renderLeaflet({
        
        # Get the data
        property_av_50 <- property_sp_data[order(property_sp_data$ave_sales,decreasing = T),][1:input$top_property,]
        property_total_50 <- property_sp_data[order(property_sp_data$sum_sales,decreasing = T),][1:input$top_property,]
        
        # Set color bin
        mybins_av <- seq(1.5, 4, by=0.5)
        mybins_total <- seq(320,1130,by = 162)
        
        # Set the palette by the domain
        mypalette_av <- colorBin( palette="Spectral", domain=property_av_50$ave_sales/1000000, na.color="transparent", bins=mybins_av)
        mypalette_total <- colorBin( palette="Spectral", domain=property_total_50$sum_sales/1000000, na.color="transparent", bins=mybins_total)
        
        # Get the label information
        mytext_av <- paste(
            "Suburb Name:",property_av_50$suburb, "<br/>",
            "Number of Property Sale: ",property_av_50$count, "<br/>",
            "Average sale price: ", round(property_av_50$ave_sales/1000000,2)," Million","<br/>",sep="")%>%
            lapply(htmltools::HTML)
        
        # Get the label information
        mytext_total <- paste(
            "Suburb Name:",property_total_50$suburb, "<br/>",
            "Number of Property Sale: ",property_total_50$count, "<br/>",
            "Total sale price: ", round(property_total_50$sum_sales/1000000,2)," Million","<br/>",sep="")%>%
            lapply(htmltools::HTML)
        
        # Draw the average property sale map
        # The number of circular depands on the user input
        # Set the hover
        # Use the addCircleMarkers to draw the circle
        if(input$pro_ave_sum == 'Average'){
            leaflet(property_av_50) %>% 
                addProviderTiles("Esri.WorldStreetMap") %>% 
                addCircleMarkers(~lon, ~lat,radius =~ave_sales/150000,
                                 fillColor = ~mypalette_av(ave_sales/1000000), fillOpacity = 0.7, color="white", stroke=FALSE,
                                 label = mytext_av,
                                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
                                 addLegend( pal=mypalette_av, values=~ave_sales/1000000, opacity=0.9, title = "Average Sale Price(M)", position = "bottomright" )
            
        }
        else{
            leaflet(property_total_50) %>% 
                addProviderTiles("Esri.WorldStreetMap") %>% 
                addCircleMarkers(~lon, ~lat,radius = ~sum_sales/40000000,
                                 fillColor = ~mypalette_total(sum_sales/1000000), fillOpacity = 0.7, color="white", stroke=FALSE,label = mytext_total,
                                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"))%>%
                addLegend( pal=mypalette_total, values=~sum_sales/1000000, opacity=0.9, title = "Total Sale Price(M)", position = "bottomright" )
            
        }
        
    })

    # Draw the line plot
    # User can select the year
    # Set to transparent
    output$line_peoperty <- renderPlot({
        property_month_plot <- subset(property_month,property_month$date_sold_year %in% input$year_check_pro)
        property_month_plot$date_sold_year <- as.character(property_month_plot$date_sold_year)
        ggplot(property_month_plot,aes(x = date_sold_month,y=count,color = date_sold_year,group = date_sold_year))+
            geom_line(linetype = "solid",size = 1.2)+
            geom_point()+
            geom_text(aes(label = count),vjust=-1,check_overlap = TRUE)+
            labs(title = "Property Sale monthly", x = "Month", y = "Numbers of property sale")+
            scale_x_continuous(breaks = seq(1,12,1))+
            theme_minimal() +
            scale_color_discrete(name = "Year")
    },bg="transparent")
    
    # Draw the map title
    output$pro_map_title <- renderUI({
        if(input$pro_ave_sum == 'Average'){
            h2(paste0("Top ",input$top_property," Property Average Sale Suburbs"))
        }
        else{
            h2(paste0("Top ",input$top_property," Property Total Sale Suburbs"))
        }
    })
    
    # Draw the school title
    output$school_title <- renderUI({
        if(input$school_type == 'Primary'){
            h2(paste0("Primary School Top ICSEA Score Suburb"))
        }
        else{
            h2(paste0("Secondary School Top ICSEA Score Suburb"))
        }
    })
    
    # Draw the lollipop chart
    # User can use slider bar to select the ICSEA Score
    # Read the input information 
    output$lol_ranking <- renderPlot({
        if(input$school_type == 'Primary'){
            
            # Prepare the data
            nsw_pri_top_1000 <- nsw_pri_data[nsw_pri_data$ICSEA > input$icsea_level,]
            nsw_pri_top_50 <- nsw_pri_top_1000[order(nsw_pri_top_1000$ICSEA,decreasing = T),]
            nsw_pri_score <- group_by(nsw_pri_top_50,Suburb) %>% dplyr::summarise(total_school = n(), 
                                                                           ave_ICSEA = round(mean(ICSEA),0),
                                                                           ave_top_SEA = round(mean(`Top SEA`),0),
                                                                           lat = mean(Latitude ),
                                                                           lon = mean(Longitude))
            nsw_pri_score <- nsw_pri_score[order(nsw_pri_score$ave_ICSEA,decreasing = T),]
            
            # Draw the lolilpop plot
            # Use theme_minimal to set transparent
            ggplot(nsw_pri_score,aes(x = Suburb, y = ave_ICSEA))+
                geom_segment(aes(x = Suburb, xend = Suburb, y = 0, yend = ave_ICSEA), color = "darkgrey") +
                geom_point(color = "orange",size = 5)+
                theme_minimal()+
                theme(
                    panel.grid.major.x = element_blank(),
                    panel.border = element_blank(),
                    axis.ticks.x = element_blank()
                ) +
                coord_cartesian(ylim = c(1130,1250)) +
                theme(axis.text.x = element_text(angle=45)) +
                geom_text(aes(label = ave_ICSEA,vjust = -2),check_overlap = TRUE)+
                labs(x = "Suburb", y ="Average ICSEA Score", title = "Top 50 Primary School ICSEA Score Suburb")
        }
        else{
            nsw_sec_top_1000 <- nsw_sec_data[nsw_sec_data$ICSEA > input$icsea_level,]
            nsw_sec_to_50 <- nsw_sec_top_1000[order(nsw_sec_top_1000$ICSEA,decreasing = T),]
            nsw_sec_score <- group_by(nsw_sec_to_50,Suburb)%>% dplyr::summarise(total_school = n(),
                                                                         ave_ICSEA = round(mean(ICSEA),0),
                                                                         ave_top_SEA = round(mean(`Top SEA(%)`),0),
                                                                         ave_DA = mean(`DA(%)`),
                                                                         lat = mean(Latitude ),
                                                                         lon = mean(Longitude))
            
            nsw_sec_score <- nsw_sec_score[order(nsw_sec_score$ave_ICSEA,decreasing = T),]
            ggplot(nsw_sec_score,aes(x = Suburb, y = ave_ICSEA))+
                geom_segment(aes(x = Suburb, xend = Suburb, y = 0, yend = ave_ICSEA), color = "darkgrey") +
                geom_point(color = "orange",size = 5)+
                theme_minimal()+
                theme(
                    panel.grid.major.x = element_blank(),
                    panel.border = element_blank(),
                    axis.ticks.x = element_blank()
                ) +
                coord_cartesian(ylim = c(1130,1250)) +
                theme(axis.text.x = element_text(angle=45)) +
                geom_text(aes(label = ave_ICSEA,vjust = -2),check_overlap = TRUE)+
                labs(x = "Suburb", y ="Average ICSEA Score", title = "Top 50 Secondary ICSEA Score Suburb")
            
        }
    }, bg="transparent")
    
    # Draw the school map 
    # Set the bin
    output$school_map <- renderLeaflet({
        
        
        mybins_pri <- seq(1150, 1220, by=14)
        mybins_sec <- seq(1150,1250,by = 20)
           
        # If user choose primary, draw the primary map 
        # with certain color with different level of ICSEA score
        if(input$school_type == 'Primary'){
            nsw_pri_top_1000 <- nsw_pri_data[nsw_pri_data$ICSEA > input$icsea_level,]
            nsw_pri_top_50 <- nsw_pri_top_1000[order(nsw_pri_top_1000$ICSEA,decreasing = T),]
            nsw_pri_score <- group_by(nsw_pri_top_50,Suburb) %>% dplyr::summarise(total_school = n(), 
                                                                           ave_ICSEA = round(mean(ICSEA),0),
                                                                           ave_top_SEA = round(mean(`Top SEA`),0),
                                                                           lat = mean(Latitude ),
                                                                           lon = mean(Longitude))
            nsw_pri_score <- nsw_pri_score[order(nsw_pri_score$ave_ICSEA,decreasing = T),]
            
            # Set the palette color
            mypalette_pri <- colorBin( palette="BrBG", domain=nsw_pri_score$ave_ICSEA, na.color="transparent", bins=mybins_pri)
            
            mytext_pri <- paste(
                "Suburb Name:",nsw_pri_score$Suburb, "<br/>",
                "Number of School: ",nsw_pri_score$total_school, "<br/>",
                "Average ICSEA Score: ", nsw_pri_score$ave_ICSEA,"<br/>",sep="")%>%
                lapply(htmltools::HTML)
            
            #Draw the map using openstreetmap style
            leaflet(nsw_pri_score) %>% 
                addProviderTiles("OpenStreetMap") %>% 
                addCircleMarkers(~lon, ~lat,radius = ~ave_ICSEA/40,
                                 fillColor = ~mypalette_pri(ave_ICSEA), fillOpacity = 0.7, color="white", stroke=FALSE,
                                 label = mytext_pri,
                                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
                                addLegend( pal=mypalette_pri, values=~ave_ICSEA, opacity=0.9, title = "ICSEA Score", position = "bottomright" )
        }
        else{
            
            # Prepare the data 
            nsw_sec_top_1000 <- nsw_sec_data[nsw_sec_data$ICSEA > input$icsea_level,]
            nsw_sec_to_50 <- nsw_sec_top_1000[order(nsw_sec_top_1000$ICSEA,decreasing = T),]
            nsw_sec_score <- group_by(nsw_sec_to_50,Suburb)%>% dplyr::summarise(total_school = n(),
                                                                         ave_ICSEA = round(mean(ICSEA),0),
                                                                         ave_top_SEA = round(mean(`Top SEA(%)`),0),
                                                                         ave_DA = mean(`DA(%)`),
                                                                         lat = mean(Latitude ),
                                                                         lon = mean(Longitude))
            
            nsw_sec_score <- nsw_sec_score[order(nsw_sec_score$ave_ICSEA,decreasing = T),]
            
            # Get the palette sytle
            mypalette_sec <- colorBin( palette="BrBG", domain=nsw_sec_score$ave_ICSEA, na.color="transparent", bins=mybins_sec)
            
            # Set the hover
            mytext_sec <- paste(
                "Suburb Name:",nsw_sec_score$Suburb, "<br/>",
                "Number of School: ",nsw_sec_score$total_school, "<br/>",
                "Average ICSEA Score: ", nsw_sec_score$ave_ICSEA,"<br/>",sep="")%>%
                lapply(htmltools::HTML)
            
            
            leaflet(nsw_sec_score) %>% 
                addProviderTiles("OpenStreetMap") %>% 
                addCircleMarkers(~lon, ~lat,radius = ~ave_ICSEA/40,
                                 fillColor = ~mypalette_sec(ave_ICSEA), fillOpacity = 0.7, color="white", stroke=FALSE,
                                 label = mytext_sec,
                                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
                addLegend( pal=mypalette_sec, values=~ave_ICSEA, opacity=0.9, title = "ICSEA Score", position = "bottomright" )
            
        }
    })
    
    # Draw the table that contains the school details
    output$school_details <- DT::renderDataTable(DT::datatable({
        
        # Show the data for different selection
        if(input$school_type == 'Primary'){
            nsw_pri_top_1000 <- nsw_pri_data[nsw_pri_data$ICSEA > input$icsea_level,]
            nsw_pri_top_50 <- nsw_pri_top_1000[order(nsw_pri_top_1000$ICSEA,decreasing = T),]
            nsw_pri_score <- group_by(nsw_pri_top_50,Suburb) %>% dplyr::summarise(total_school = n(), 
                                                                           ave_ICSEA = round(mean(ICSEA),0),
                                                                           ave_top_SEA = round(mean(`Top SEA`),0),
                                                                           lat = mean(Latitude ),
                                                                           lon = mean(Longitude))
            nsw_pri_score <- nsw_pri_score[order(nsw_pri_score$ave_ICSEA,decreasing = T),]
            nsw_pri_score
        }
        
        # Show the data table of secondary school suburb
        else{
            nsw_sec_top_1000 <- nsw_sec_data[nsw_sec_data$ICSEA > input$icsea_level,]
            nsw_sec_to_50 <- nsw_sec_top_1000[order(nsw_sec_top_1000$ICSEA,decreasing = T),]
            nsw_sec_score <- group_by(nsw_sec_to_50,Suburb)%>% dplyr::summarise(total_school = n(),
                                                                         ave_ICSEA = round(mean(ICSEA),0),
                                                                         ave_top_SEA = round(mean(`Top SEA(%)`),0),
                                                                         ave_DA = mean(`DA(%)`),
                                                                         lat = mean(Latitude ),
                                                                         lon = mean(Longitude))
            
            nsw_sec_score <- nsw_sec_score[order(nsw_sec_score$ave_ICSEA,decreasing = T),]
            nsw_sec_score
        }
        
    }))
    
    # Draw the join map
    output$school_pro_map <- renderLeaflet({
        
        if(input$school_type_third == "Secondary"){
            
            # Prepare the data 
            property_av_50 <- property_sp_data[order(property_sp_data$ave_sales,decreasing = T),][1:input$top_property_third,]
            nsw_sec_top_1000 <- nsw_sec_data[nsw_sec_data$ICSEA > input$icsea_level_third,]
            nsw_sec_to_50 <- nsw_sec_top_1000[order(nsw_sec_top_1000$ICSEA,decreasing = T),]
            nsw_sec_score <- group_by(nsw_sec_to_50,Suburb)%>% dplyr::summarise(total_school = n(),
                                                                                ave_ICSEA = round(mean(ICSEA),0),
                                                                                ave_top_SEA = round(mean(`Top SEA(%)`),0),
                                                                                ave_DA = mean(`DA(%)`),
                                                                                lat = mean(Latitude ),
                                                                                lon = mean(Longitude))
            
            nsw_sec_score <- nsw_sec_score[order(nsw_sec_score$ave_ICSEA,decreasing = T),]
            
            # Set the hover information
            mytext_pro <- paste(
                "Suburb Name:",property_av_50$suburb, "<br/>",
                "Number of Property Sale: ",property_av_50$count, "<br/>",
                "Average sale price: ", round(property_av_50$ave_sales/1000000,2)," Million","<br/>",sep="")%>%
                lapply(htmltools::HTML)
            
            mytext_sec_th <- paste(
                "Suburb Name:",nsw_sec_score$Suburb, "<br/>",
                "Number of School: ",nsw_sec_score$total_school, "<br/>",
                "Average ICSEA Score: ", nsw_sec_score$ave_ICSEA,"<br/>",sep="")%>%
                lapply(htmltools::HTML)

            # Get the length
            len_pro <- nrow(property_av_50)
            len_nsw <- nrow(nsw_sec_score)
            
            # modify the latitude and longitude
            for(i in c(1:len_nsw)){
                for (j in c(1:len_pro)){
                    if(nsw_sec_score$Suburb[i] == property_av_50$suburb[j]){
                        property_av_50$lat[j] <- nsw_sec_score$lat[i]
                        property_av_50$lon[j] <- nsw_sec_score$lon[i]
                        break
                    }
                }  
            }
            
           # Draw the map with both dataset 
            leaflet() %>% 
                addProviderTiles("Esri.WorldStreetMap") %>% 
                addCircleMarkers(data = nsw_sec_score,~lon, ~lat,radius = ~ave_ICSEA /40, 
                                 fillColor ="#5081BB", fillOpacity = 0.7, stroke=FALSE,
                                 label = mytext_sec_th,
                                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
                addCircleMarkers(data = property_av_50,~lon, ~lat,radius = ~ave_sales/150000,
                                 fillColor ="#63C17D", fillOpacity = 0.7, stroke=FALSE,
                                 label = mytext_pro,
                                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"))
            
        }else{
            
            # Prepare the data
            property_av_50_pri <- property_sp_data[order(property_sp_data$ave_sales,decreasing = T),][1:input$top_property_third,]
            nsw_pri_top_1000 <- nsw_pri_data[nsw_pri_data$ICSEA > input$icsea_level_third,]
            nsw_pri_top_50 <- nsw_pri_top_1000[order(nsw_pri_top_1000$ICSEA,decreasing = T),]
            nsw_pri_score <- group_by(nsw_pri_top_50,Suburb) %>% dplyr::summarise(total_school = n(), 
                                                                                  ave_ICSEA = round(mean(ICSEA),0),
                                                                                  ave_top_SEA = round(mean(`Top SEA`),0),
                                                                                  lat = mean(Latitude ),
                                                                                  lon = mean(Longitude))
            nsw_pri_score <- nsw_pri_score[order(nsw_pri_score$ave_ICSEA,decreasing = T),]
            len_pro_pri <- nrow(property_av_50_pri)
            len_nsw_pri <- nrow(nsw_pri_score)
            
            # Set the hover information
            mytext_pro <- paste(
                "Suburb Name:",property_av_50_pri$suburb, "<br/>",
                "Number of Property Sale: ",property_av_50_pri$count, "<br/>",
                "Average sale price: ", round(property_av_50_pri$ave_sales/1000000,2)," Million","<br/>",sep="")%>%
                lapply(htmltools::HTML)
            
            mytext_pri_th <- paste(
                "Suburb Name:",nsw_pri_score$Suburb, "<br/>",
                "Number of School: ",nsw_pri_score$total_school, "<br/>",
                "Average ICSEA Score: ", nsw_pri_score$ave_ICSEA,"<br/>",sep="")%>%
                lapply(htmltools::HTML)
            
            # modify the latitude and longitude
            for(i in c(1:len_nsw_pri)){
                for (j in c(1:len_pro_pri)){
                    if(nsw_pri_score$Suburb[i] == property_av_50_pri$suburb[j]){
                        property_av_50_pri$lat[j] <- nsw_pri_score$lat[i]
                        property_av_50_pri$lon[j] <- nsw_pri_score$lon[i]
                        break
                    }
                }  
            }
            
            # Draw map with two datasets 
            leaflet() %>% 
                addProviderTiles("Esri.WorldStreetMap") %>% 
                addCircleMarkers(data = nsw_pri_score,~lon, ~lat,radius = ~ave_ICSEA /60, 
                                 fillColor ="#5081BB", fillOpacity = 0.7, stroke=FALSE,
                                 label = mytext_pri_th,
                                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
                addCircleMarkers(data = property_av_50_pri,~lon, ~lat,radius = ~sum_sales/20000000,
                                 fillColor ="#63C17D", fillOpacity = 0.7, stroke=FALSE,
                                 label = mytext_pro,
                                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"))
        }
    })
})
