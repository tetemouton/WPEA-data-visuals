library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(sf)
library(maps)
library(grid)
library(gridExtra)
library(ggthemes)
library(magrittr)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

  theme_set(theme_bw())
  
  sf_use_s2(FALSE)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")

  load("./Data/All_a-best_data.RData")
  load("./Data/All_lf-master-wpea_data.RData")
  load("./Data/All_wf-master-wpea_data.RData")

  dat %<>% mutate(lat = as.numeric(str_sub(lat_short, 1, -2)), lon = as.numeric(str_sub(lon_short, 1, -2)),
                  lat_txt = str_sub(lat_short, -1, -1), lon_txt = str_sub(lon_short, -1, -1),
                  lat = ifelse(lat_txt == "S", -lat, lat), lon = ifelse(lon_txt == "W", 360 - lon, lon))


  dat_w <- dat %>% filter(ocean_id %in% c("WN","WS"), flag_id %in% c("ID","PH","VN")) %>% mutate(Fleet = paste(flag_id, fleet_id)) %>%
                   select(Gear = gr_id, flag_id, fleet_id, Fleet, yy, lat, lon, ocean_id, days, sets, hhooks, stdeff, Skipjack = skj_c, Yellowfin = yft_c, Bigeye = bet_c)

  
  dat_lf %<>% mutate(FLEET_ID = ifelse(is.na(FLEET_ID), "", FLEET_ID))
  dat_wf %<>% mutate(FLEET_ID = ifelse(is.na(FLEET_ID), "", FLEET_ID))
  
  
  xlim_yrs <- c(min(dat_w$yy), max(dat_w$yy))
  
  dat_l <- dat_w %>% pivot_longer(c(Skipjack,Yellowfin,Bigeye), names_to = "Species", values_to = "Catch")

  fstyr <- 1950
  lstyr <- 2021
  
  eez <- st_read("./Data/EEZ_Shape_Files/World_EEZ_v10_2018_0_360.shp")
  
  assreg <- read.csv("./Data/reg_borders.csv", header=TRUE)
  assreg5 <- filter(assreg, ID == 5)

  collist <- c("S" = "red2", "L" = "royalblue2", "G" = "darkolivegreen3", "H" = "darkorchid2", "K" = "goldenrod1", "P" = "grey30",  "R" = "lightsalmon1",  "O" = "wheat4",
               "Skipjack" = "black", "Yellowfin" = "brown", "Bigeye" = "green",
               "ID " = "orange", "ID ID" = "darkorchid1", "PH " = "skyblue1", "PH DW" = "yellow", "PH PH" = "forestgreen", "VN " = "pink")

  
#____________________________________________________________________________________________________________
# User interface

ui <- fluidPage(
    
    title = "Fishy fishy",

    tabPanel("Catch consequences",
             
             tags$head(
                 tags$style(HTML("hr {border-top: 1px solid #000000;}"))
             ),

             titlePanel(title = div(img(height = 90, width = 1700, src = "HeaderBar1.png")), windowTitle = "At ease on the high seas"),

             setBackgroundColor(
                 color = c("white", "white"),
                 gradient = "linear",
                 direction = c("bottom","left")
             ),
             
             
             sidebarLayout(
               sidebarPanel(width = 2,
                            
                            checkboxGroupInput("grs", "Fishing gears to display:",
                                               c("Purse Seine (S)" = "S", "Longline (L)" = "L", "Pole and Line (P)" = "P", "Gillnet (G)" = "G", "Handline (H)" = "H",
                                                 "Knet??? (K)" = "K", "Ringnet (R)" = "R", "Other gears (O)" = "O"),
                                               selected = c("S","L")),
                            
                            checkboxGroupInput("species", "Choose your species:",
                                               c("Skipjack" = "Skipjack", "Yellowfin" = "Yellowfin", "Bigeye" = "Bigeye"),
                                               selected = c("Skipjack","Yellowfin","Bigeye")),
                            
                            checkboxGroupInput("cntry", "Choose your country:",
                                               c("Indonesia" = "ID", "Philippines" = "PH", "Vietnam" = "VN"),
                                               selected = c("ID","PH","VN")),
                            
                            radioButtons("map_size", label = h5(tags$b("Choose map size:")),
                                         choices = list("WPEA" = 1, "Wider WCPO" = 2),
                                         selected = 2, inline = TRUE),

                            conditionalPanel(condition = "input.response == 'longL'",
                                              radioButtons("spp_switch_l", label = h5(tags$b("Choose metric:")),
                                                           choices = c("Bigeye catch (mt)" = 1, "Yellowfin catch (mt)" = 2, "Albacore catch (mt)" = 3),
                                                           selected = 1)),
                            
                            radioButtons("all_switch", label = h5(tags$b("Show catches:")),
                                         choices = list("Gears" = 1, "Fleets" = 2, "Species" = 3),
                                         selected = 1, inline = TRUE),
                            

                                            sliderInput("sliderrng", label = h5(tags$b("Choose years to display")),  min = fstyr, max = lstyr, value = c(2000, lstyr), width="250px", ticks=TRUE, sep="")
               ),
               mainPanel(
                 fluidRow(column(8, plotOutput("Catmap")), column(4, plotOutput("Lenplots"))),
                 br(),
                 br(),
                 fluidRow(column(8, plotOutput("Catbar")), column(4, plotOutput("Wgtplots"))),
               )
             )
    )
)


#____________________________________________________________________________________________________________
# The server

server <- function(input, output) {
    
  plot_dat <- reactive({
    
    # Filter to leave only checked gears
    if(is.null(input$grs)){
      
      dat_pl <- dat_l
      
    } else{
      
      dat_pl <- dat_l %>% filter(Gear %in% c(input$grs))
      
    }
    
    # Filter to leave only checked species
    if(is.null(input$species)){
      
      dat_pl <- dat_pl
      
    } else{
      
      dat_pl %<>% filter(Species %in% c(input$species))
      
    }
    
    # Filter to leave only checked countries (flags)
    if(is.null(input$cntry)){
      
      dat_pl <- dat_pl
      
    } else{
      
      dat_pl %<>% filter(flag_id %in% c(input$cntry))
      
    }
    
    if(input$all_switch == "1"){
      
      dat_pl %<>% mutate(grp_var = Gear)
      
    } else{
      
      if(input$all_switch == "2"){
        
        dat_pl %<>% mutate(grp_var = Fleet)
        
      } else{
        
        dat_pl %<>% mutate(grp_var = Species)
        
      }
      
    }
    
    dat_pl
    
  })
  
  
  
  plot_len_dat <- reactive({
    
    dat_len <- dat_lf %<>% filter(between(LEN, 0, 200)) %>% mutate(Species = recode(SP_ID,  SKJ = "Skipjack", BET = "Bigeye", YFT = "Yellowfin"),
                                  Fleet = paste(FLAG_ID, FLEET_ID), Gear = GR)
    
    # Filter to leave only checked gears
    if(is.null(input$grs)){
      
      dat_pl <- dat_len
      
    } else{
      
      dat_pl <- dat_len %>% filter(Gear %in% c(input$grs))
      
    }
    
    # Filter to leave only checked species
    if(is.null(input$species)){
      
      dat_pl <- dat_pl
      
    } else{
      
      dat_pl %<>% filter(Species %in% c(input$species))
      
    }
    
    # Filter to leave only checked countries (flags)
    if(is.null(input$cntry)){
      
      dat_pl <- dat_pl
      
    } else{
      
      dat_pl %<>% filter(FLAG_ID %in% c(input$cntry))
      
    }
    
    if(input$all_switch == "1"){
      
      dat_pl %<>% mutate(grp_var = Gear)
      
    } else{
      
      if(input$all_switch == "2"){
        
        dat_pl %<>% mutate(grp_var = Fleet)
        
      } else{
        
        dat_pl %<>% mutate(grp_var = Species)
        
      }
      
    }
    
    dat_pl
    
  })
  
  
  
  plot_wgt_dat <- reactive({
    
    dat_wgt <- dat_wf %<>% filter(between(WT, 0, 150)) %>% mutate(Species = recode(SP_ID,  SKJ = "Skipjack", BET = "Bigeye", YFT = "Yellowfin"),
                                  Fleet = paste(FLAG_ID, FLEET_ID), Gear = GR)
    
    # Filter to leave only checked gears
    if(is.null(input$grs)){
      
      dat_pl <- dat_wgt
      
    } else{
      
      dat_pl <- dat_wgt %>% filter(Gear %in% c(input$grs))
      
    }
    
    # Filter to leave only checked species
    if(is.null(input$species)){
      
      dat_pl <- dat_pl
      
    } else{
      
      dat_pl %<>% filter(Species %in% c(input$species))
      
    }
    
    # Filter to leave only checked countries (flags)
    if(is.null(input$cntry)){
      
      dat_pl <- dat_pl
      
    } else{
      
      dat_pl %<>% filter(FLAG_ID %in% c(input$cntry))
      
    }
    
    if(input$all_switch == "1"){
      
      dat_pl %<>% mutate(grp_var = Gear)
      
    } else{
      
      if(input$all_switch == "2"){
        
        dat_pl %<>% mutate(grp_var = Fleet)
        
      } else{
        
        dat_pl %<>% mutate(grp_var = Species)
        
      }
      
    }
    
    dat_pl
    
  })
  
  
  output$Catmap <- renderPlot({
    
    dat_pl <- plot_dat()
    
    dat_pl_map <- dat_pl %>% filter(between(yy, input$sliderrng[1], input$sliderrng[2])) %>% group_by(lat, lon, Gear) %>%
                             summarise(Catch = sum(Catch)/(input$sliderrng[2] - input$sliderrng[1] + 1))
    
    
    if(input$map_size == "1"){
      
      map_expnse <- c(150, 20)
      assreg %<>% filter(ID == 5)
      
    } else{
        
      map_expnse <- c(210, 40)
      
    }
    
    
      pl <- ggplot() +
                   #geom_sf(data = eez, color = alpha("black", 0.9), fill = "aliceblue") +
                   geom_sf(data = world, color = "black", fill = "grey80") + ggthemes::theme_few() +
                   scale_colour_manual(values = collist[names(collist) %in% unique(dat_pl_map$Gear)]) +
                   coord_sf(xlim = c(100, map_expnse[1]), ylim = c(-20, map_expnse[2]), expand = FALSE) +
                   geom_polygon(data = assreg, aes(x = lon, y = lat, group = ID, fill = ID), colour = "black", fill = alpha("steelblue", 0.6), alpha = 0.1) +
                   geom_polygon(data = assreg5, aes(x = lon, y = lat, group = ID, fill = ID), colour = "black", fill = alpha("purple", 0.7), alpha = 0.1)
        
      if(length(input$grs) > 1){
        
        pl <- pl + geom_jitter(data = dat_pl_map, aes(x = lon + 2.5, y = lat + 2.5, size = Catch, colour = Gear), alpha = 0.8, width = .5)
        
      } else{
        
        pl <- pl + geom_point(data = dat_pl_map, aes(x = lon + 2.5, y = lat + 2.5, size = Catch, colour = Gear), alpha = 0.8, width = .5)
        
      } 
      
      
      pl <- pl + scale_size_continuous(range = c(1,15)) +
                 theme(panel.border = element_blank(), axis.title = element_blank(),
                       axis.text = element_blank(), axis.ticks = element_blank())
      
      pl
    
  }, height = 400, width = 800)
  

  output$Catbar <- renderPlot({
    
    dat_pl <- plot_dat()
    
    dat_pl_bar <- dat_pl %>% group_by(yy, Group = grp_var) %>%
                             summarise(Catch = sum(Catch))
    
   # %>% filter(between(yy, input$sliderrng[1], input$sliderrng[2])) 
    
    pl <- ggplot() + xlab("") + ylab("Catch (1,000's mt)") +
                     geom_rect(aes(xmin = input$sliderrng[1] - 0.5,
                                   xmax = input$sliderrng[2] + 0.5,
                                   ymin = -Inf, ymax = Inf), fill = alpha("steelblue", 0.3)) + ggtitle("Catch history") +
                     geom_bar(data = dat_pl_bar, aes(x = yy, y = Catch/1000, fill = Group), alpha = 0.6,  stat = "identity", width = 0.8) + theme_minimal() +
                     scale_fill_manual(values = collist[names(collist) %in% unique(dat_pl_bar$Group)]) + xlim(xlim_yrs) +
                     theme(panel.border = element_blank(), axis.text = element_text(size = 14), axis.title = element_text(size = 14),
                           plot.title = element_text(vjust = - 7, size = 18, colour = alpha("grey30", 0.7), face = "bold"))
    
    pl
    
  }, height = 350, width = 800)
  
  
  output$Lenplots <- renderPlot({
    
    dat_pl <- plot_len_dat()
    
    dat_pl_bar <- dat_pl %>% group_by(yy = YR, Group = grp_var) %>%
                             summarise(N = sum(FREQ))
    
    # %>% filter(between(yy, input$sliderrng[1], input$sliderrng[2])) 
    
    pl1 <- ggplot() + xlab("") + ylab("No. lengths (1,000's)") +
      geom_rect(aes(xmin = input$sliderrng[1] - 0.5,
                    xmax = input$sliderrng[2] + 0.5,
                    ymin = -Inf, ymax = Inf), fill = alpha("steelblue", 0.3)) + ggtitle("Length frequencies") +
      geom_bar(data = dat_pl_bar, aes(x = yy, y = N/1000, fill = Group), alpha = 0.6,  stat = "identity", width = 0.8) + theme_minimal() +
      #guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_fill_manual(values = collist[names(collist) %in% unique(dat_pl_bar$Group)]) + xlim(xlim_yrs) +
      theme(panel.border = element_blank(), axis.text = element_text(size = 13), axis.title = element_text(size = 14),
            plot.title = element_text(vjust = - 8, size = 14, colour = alpha("grey30", 0.7), face = "bold"))
    
    
    dat_pl <- plot_len_dat() %>% ungroup()
    
    dat_pl_den <- dat_pl %>% filter(between(YR, input$sliderrng[1], input$sliderrng[2])) %>% group_by(Group = grp_var, LEN) %>%
      summarise(N = sum(FREQ)) %>% mutate(N = N/sum(N))
    
    #dat_pl_den %<>% group_by_all %>% expand(N = 1:N) %>% filter(N != 0) %>% ungroup()
    
    # %>% filter(between(yy, input$sliderrng[1], input$sliderrng[2])) 
    
    pl2 <- ggplot() + xlab("Length (cm)") + ylab("Density") +
      #geom_density(data = dat_pl_den, aes(x = LEN, y = N, fill = Group), alpha = 0.6) + theme_minimal() +
      geom_bar(data = dat_pl_den, aes(x = LEN, y = N, fill = Group), stat = "identity", alpha = 0.4, width = 1) + theme_minimal() +
      #guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_fill_manual(values = collist[names(collist) %in% unique(dat_pl_bar$Group)]) +
      theme(panel.border = element_blank()) #, legend.position = c(0.8, 0.7))
    
    
    grid.arrange(pl1, pl2, ncol = 1, nrow = 2)
    
  }, height = 400, width = 600)  
  
  
  output$Wgtplots <- renderPlot({
    
    dat_pl <- plot_wgt_dat()
    
    dat_pl_bar <- dat_pl %>% group_by(yy = YR, Group = grp_var) %>%
                             summarise(N = sum(FREQ))
    
    
    pl1 <- ggplot() + xlab("") + ylab("No. weights (1,000's)") +
      geom_rect(aes(xmin = input$sliderrng[1] - 0.5,
                    xmax = input$sliderrng[2] + 0.5,
                    ymin = -Inf, ymax = Inf), fill = alpha("steelblue", 0.3)) + ggtitle("Weight frequencies") +
      geom_bar(data = dat_pl_bar, aes(x = yy, y = N/1000, fill = Group), alpha = 0.6,  stat = "identity", width = 0.8) + theme_minimal() +
      #guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_fill_manual(values = collist[names(collist) %in% unique(dat_pl_bar$Group)]) + xlim(xlim_yrs) +
      theme(panel.border = element_blank(), axis.text = element_text(size = 13), axis.title = element_text(size = 14),
            plot.title = element_text(vjust = - 8, size = 14, colour = alpha("grey30", 0.7), face = "bold"))
    
    
    dat_pl <- plot_wgt_dat() %>% ungroup()
    
    dat_pl_den <- dat_pl %>% filter(between(YR, input$sliderrng[1], input$sliderrng[2])) %>% group_by(Group = grp_var, WT) %>%
      summarise(N = sum(FREQ)) %>% mutate(N = N/sum(N))
    
    #dat_pl_den %<>% group_by_all %>% expand(N = 1:N) %>% filter(N != 0) %>% ungroup()
    
    # %>% filter(between(yy, input$sliderrng[1], input$sliderrng[2])) 
    
    pl2 <- ggplot() + xlab("Weight (kg)") + ylab("Density") +
      #geom_density(data = dat_pl_den, aes(x = LEN, fill = Group), alpha = 0.6) + theme_minimal() +
      geom_bar(data = dat_pl_den, aes(x = WT, y = N, fill = Group), stat = "identity", alpha = 0.4, width = 1) + theme_minimal() +
      #guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_fill_manual(values = collist[names(collist) %in% unique(dat_pl_bar$Group)]) +
      theme(panel.border = element_blank()) #, legend.position = c(0.8, 0.7))
    
    
    grid.arrange(pl1, pl2, ncol = 1, nrow = 2)
    
  }, height = 350, width = 600)  
    
    
}


shinyApp(ui = ui, server = server)
