#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Packages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(pacman)
# p_load(char = c("DataExplorer", 
#                 "here",
#                 "gt",
#                 "reactable",
#                 "paletteer",
#                 "ggthemes", 
#                 "highcharter",
#                 "shiny", 
#                 "shinyWidgets",
#                 "styler",
#                 "bs4Dash",
#                 "tools",
#                 "ggallin",
#                 "ggimage",
#                 "tidyverse"
# ))


library(here)
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(ggallin)
library(ggimage)
library(tidyverse)


options(scipen = 999)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Source functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



gg.gauge <- function(pos,breaks=c(0,10,30,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y), size = 5, color = alpha("black", 0.3))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"")))+
    annotate("text",x=0,y=0,label= paste("Risk score", 
                                         format(pos, digits = 2)
                                         
                                         ) ,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ ui ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ui <-
  bs4DashPage(
    old_school = FALSE,
    sidebar_min = FALSE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = FALSE,
    controlbar_overlay = TRUE,
    title = "Map input vars to pictorial chart idea",
    navbar = bs4DashNavbar(
      sidebarIcon = "bars",
      controlbarIcon = "th",
      fixed = FALSE
    ),
    sidebar = bs4DashSidebar(
      bs4SidebarMenu(
        bs4SidebarHeader("Tabs"),
        bs4SidebarMenuItem(
          "Run simulations",
          tabName = "tab1",
          icon = "sliders"
        )
        )
    ),
    controlbar = bs4DashControlbar(
      skin = "light",
      title = " Change parameters",
      sliderInput(
        inputId = "size",
        label = "Room size (cubic metres):",
        min = 25,
        max = 10000,
        value = 50, 
        ticks = FALSE
      ),
      sliderInput(
        inputId = "occupants",
        label = "Occupants:",
        min = 1,
        max = 100,
        value = 5, 
        ticks = TRUE
      ),
      sliderInput(
        inputId = "window_area",
        label = "Windowed region",
        min = 0,
        max = 1,
        value = 0.5, 
        ticks = TRUE
      ),
      column(
        width = 12,
        align = "center",
        radioButtons(
          inputId = "masks_on_or_off",
          label = "Masks?",
          c(
            "Masks on" = "masks_on",
            "Masks off" = "masks_off"
          )
        )
      )
    ),
    footer = bs4DashFooter(),
    body = bs4DashBody(
      bs4TabItems(
        bs4TabItem(
          tabName = "tab1",
      fluidRow(column(width = 9,
                      bs4Card(title = "Simulation",
                              closable = TRUE,
                              width = 12,
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("room_plot")))),
        fluidRow(column(width = 9,
                        bs4Card(title = "Risk",
                                closable = TRUE,
                                width = 5,
                                gradientColor = "success",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput("risk_level")))
                 )
        )
      )
      )
  )


# 
# input <- list(
#   size = 10000,
#   occupants = 50,
#   masks_on_or_off = "masks_off",
#   window_area = 10
# 
# )


server <- function(input, output) {
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Accept inputs into reactives ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  #~~ Bounds ---------------------------
  
  bounds <-
    reactive(
      input$size  %>%
        {./5} %>%  # convert to area assuming height of room is 5m
        {sqrt(.)} %>% # square root to convert to length of a side
        {./2} # so that we can go from -. to +.
    )

# 
# bounds <- function(){input$size  %>%
#   {./5} %>%  # convert to area assuming height of room is 5m
#   {sqrt(.)} %>% # square root to convert to length of a side
#   {./2} } # so that we can go from -. to +.

  #~~ Occupants ---------------------------
  
  occupants <- reactive(input$occupants)
 
  #occupants <- function()input$occupants
  
  #~~ Masks on or off ---------------------------
  
  masks_on_or_off <- reactive(input$masks_on_or_off)
  
  #masks_on_or_off <- function()input$masks_on_or_off
  
  
  #~~ Window area ---------------------------
  
  window_prop <- 
    reactive(
    input$window_area 
      )
  # 
  # window_length <- 
  #   function(){
  #     input$window_area %>% 
  #       {sqrt(.)}}
  # 

  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Make the plot ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  

  make_the_room_plot <- 
    function(){
      room_plot <- ggplot() + 
        geom_rect(aes(xmin = (bounds()) * -1.2, 
                  xmax = bounds() * 1.2, 
                  ymin = (bounds()) * -1.2, 
                  ymax = bounds() * 1.2), 
              fill = alpha("orange", 0.1), size = 3, 
              color = 'dodgerblue4') + 
        
        # top
        geom_segment(aes(x = (bounds()) * -1.2 * window_prop(),
                         xend = bounds() * 1.2 * window_prop(),
                         y = bounds() * 1.2,
                         yend = bounds() * 1.2  ),
                     size = 3,
                     color = "skyblue") +
        # bottom
        geom_segment(aes(x = (bounds()) * -1.2 * window_prop(),
                         xend = bounds() * 1.2 * window_prop(),
                         y = bounds() * -1.2,
                         yend = bounds() * -1.2  ),
                     size = 3,
                     color = "skyblue") +
        # right
        geom_segment(aes(y = (bounds()) * -1.2 * window_prop(),
                         yend = bounds() * 1.2 * window_prop(),
                         x = bounds() * 1.2,
                         xend = bounds() * 1.2  ),
                     size = 3,
                     color = "skyblue") +
        # left
        geom_segment(aes(y = (bounds()) * -1.2 * window_prop(),
                         yend = bounds() * 1.2 * window_prop(),
                         x = bounds() * -1.2,
                         xend = bounds() * -1.2  ),
                     size = 3,
                     color = "skyblue") +
    scale_x_continuous(trans = pseudolog10_trans, limits = c(-27.95085 - 1, 27.95085 + 1 ) ) + 
    scale_y_continuous(trans = pseudolog10_trans, limits = c(-27.95085 - 1 , 27.95085 + 1) ) + coord_fixed() 
      
  icon_positions <- ((round((100*-bounds())) : round((100*bounds())))/100)
  position_weights <- 1 - (abs(icon_positions)/30)
  
  icons_df <- tibble(
    x = sample(x = icon_positions, size = occupants(), prob = position_weights),
    y = sample(x = icon_positions, size = occupants(), prob = position_weights),
    # x = rnorm(n = occupants(), mean = 0, sd = log10(bounds()) ),
    # y = rnorm(n = occupants(), mean = 0, sd = log10(bounds()) ),
    masked_image = sample(x = c(here("pictures/masked_boy.png"), 
                                here("pictures/masked_girl.png")),
                          size = occupants(), replace = T),
    unmasked_image = sample(x = c(here("pictures/unmasked_boy.png"), 
                                here("pictures/unmasked_girl.png")),
                          size = occupants(), replace = T)
  )
    
    
  if(masks_on_or_off() == "masks_on"){
    room_plot <- room_plot + 
      geom_image(data = icons_df, aes(x, y, image = masked_image ), size = 1/20)
    }

  if(masks_on_or_off() == "masks_off"){
    room_plot <- room_plot + 
      geom_image(data = icons_df, aes(x, y, image = unmasked_image ), size = 1/20)
  }
  
  return(room_plot)
  
}
  
  ## calculate risk
  
  calc_risk_level <- function(){
  
  risk_level <-  ((occupants()^(2.8)/ ((bounds()^(1/1.2) ) * ((window_prop() + 0.01)/3) )))^(1/4)
  
  if(masks_on_or_off() == "masks_on"){
    risk_level <- 0.4 * risk_level
  }
  # 
  # # normalize to 100 
  # 
  # risk_level <- 
  #   (risk_level - 0.05) / (2500 - 0.05) %>% 
  #   {. * 100 } %>% 
  #   round(digits = 2) 
  
  return(risk_level)
  
  }
    

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Output the plot ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  output$room_plot <- renderPlot(make_the_room_plot())
    
  output$risk_level <- renderPlot(gg.gauge(pos = calc_risk_level())) 
  
  
}


# Run the application
shinyApp(ui = ui, server = server)



