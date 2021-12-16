#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
library(lme4)
library(broom)
library(broom.mixed)
library(DiagrammeR)
theme_set(theme_bw(base_size = 18))

source("ovb_temp_snail_funs.R")
source("make_ovb_diagram.R")

# Define UI for application that draws a histogram
# Define UI for random distribution app ----
ui <- fluidPage(
    title = "Exploring OVB with Snails",
    withMathJax(),
    tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script >
            ")),
    
    # App title ----
    titlePanel("Exploring Different Methods for Coping with Omitted Variable Bias in the Relationship Between Snails and Temperature"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            actionButton("run", "Click to re-simulate data and analyze"),
            
            br(),br(),
            
            # Site-level Properties ----
            tags$h4("Site-level Properties"),
            
            sliderInput("n_sites", "Number of Sites", value = 10, 
                        min = 4, max = 100),
            numericInput("ocean_temp", 
                         "Slope of relationship between Oceanography and Temperature",
                         value = 2,
                         step = 0.1),
            numericInput("temp_sd", 
                         "SD in Temperature Across Sites",
                         value = 0,
                         min = 0,
                         step = 0.01),
            numericInput("temp_mean", 
                         "Resulting Regional Mean in Temperature",
                         value = 15,
                         min = 0),
            numericInput("ocean_recruitment", 
                         "Slope of relationship between Oceanography and Recruitment",
                         value = -2,
                         step = 0.1),
            numericInput("recruitment_sd", 
                         "SD in Recruitment Across Sites",
                         value = 0,
                         min = 0,
                         step = 0.01),
            numericInput("rec_mean", 
                         "Resulting Regional Mean in Recruitment (#/plot)",
                         value = 15,
                         min = 0),

            # br() element to introduce extra vertical spacing ----
            br(), br(),
            
            # Plot properties ----
            tags$h4("Plot-level Properties"),
            
            sliderInput("n_plots_per_site", 
                        "Number of Plots Per Site/Years Sampled", 
                        value = 10, 
                        min = 4, max = 100),
            numericInput("plot_temp_sd", 
                         "SD in temperature between plots",
                         value = 1,
                         min = 0,
                         step = 0.01),
            numericInput("temp_effect", 
                         "Slope of Relationship Between Temperature and Snails",
                         value = 1,
                         step = 0.1),
            numericInput("recruitment_effect", 
                         "Slope of Relationship Between Recruitment and Snails",
                         value = 1,
                         step = 0.1),
            numericInput("sd_plot", 
                         "SD of Snails",
                         value = 3,
                         step = 0.01),
            br(),br(),
            checkboxInput("use_seed", "Use a seed for repeatable simulation?"),
            
            numericInput("seed", 
                          "seed (for repeatable simulations)",
                          value = 31415),
            

        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        
                        tabPanel("About", 
                                 HTML("This app shows the consqeuences of different model structures
                                      for the analysis of data contaminated by omitted variable bias. <br><br>
                                      
                                      Consider a system (see the system tab) where you are interested in how temperature
                                      affects snail abundance. However, temperature is driven by oceanography at a site 
                                      scale, and oceanography also drives recruitment. You have measured neither oceanography
                                      nor recruitment.<br><br>
                                      
                                      In your work, you have measured sites anually (or assessed plots within
                                      sites in one year - same models - as long as there is variation in temperature between plots!) 
                                      and both temperature and number of snails is recorded. Temperature 
                                      within one replicate is influenced by both site-level average temperature
                                      as well as more immediate varying conditions - i.e., either variation within
                                      a site or variation across years. <br><br> 
                                      
                                      Snail abundance is influenced both by temperature as well as site-level 
                                      recruitment. To remind you, a) we have no measure of site level recruitment and 
                                      b) site level recruitment and temperature are collinear - both are driven by site-level oceanography.
                                      <br><br>
                                      
                                      The models we use to analyze the data are as follows for year (or plot) i in site j:
                                      
                                      <br><br>
                                      <b>Naive model</b>: <br>
                                      <code>lm(y ~ x)</code><br>
                                      $y_{ij} \\sim \\mathcal{N}(\\widehat{y_{ij}}, \\sigma^2)$<br>
                                      $\\widehat{y_{ij}} = \\beta_0 x_{ij} +\ \beta_1$<br>
                                      
                                      <br><br>
                                      <b>RE model</b>: <br>
                                      <code>lmer(y ~ x + (1|site))</code><br>
                                      $y_{ij} \\sim \\mathcal{N}(\\widehat{y_{ij}}, \\sigma^2)$<br>
                                      $\\widehat{y_{ij}} = \\beta_0 x_{ij} + \\beta_1 + \\mu_j$<br>
                                      $\\mu_j \\sim \\mathcal{N}(0, \\sigma_{site}^2)$<br>

                                      <br><br>
                                      <b>FE model</b>: <br>
                                      <code>lm(y ~ x + site)</code><br>
                                      $y_i \\sim \\mathcal{N}(\\widehat{y_i}, \\sigma^2)$<br>
                                      $\\widehat{y_ij} = \\beta_0 x_{1ij} + \\sum_{k=1}^{j} \\beta_k x_{2ij}$<br>
                                      $x_{2ij} =  1  \\text{ if site} = j, 0  \\text{ otherwise}$<br>
                                      
                                      <br><br>
                                      <b>Group Mean Covariate model</b>: <br>
                                      <code>lmer(y ~ x + x_site_mean + (1|site))</code><br>
                                      $y_{ij} \\sim \\mathcal{N}(\\widehat{y_{ij}}, \\sigma^2)$<br>
                                      $\\widehat{y_{ij}} = \\beta_0 x_{ij} + + \\beta_1 \\bar{x_j} + \\beta_2 + \\mu_j$<br>
                                      $\\mu_j \\sim \\mathcal{N}(0, \\sigma_{site}^2)$<br>
                                      
                                      <br><br>
                                      <b>Group Mean Centered model</b>: <br>
                                      <code>lmer(y ~ x_dev_from_site_mean + x_site_mean + (1|site))</code><br>
                                      $y_{ij} \\sim \\mathcal{N}(\\widehat{y_{ij}}, \\sigma^2)$<br>
                                      $\\widehat{y_{ij}} = \\beta_0 (x_{ij} - \\bar{x_j}) + \\beta_1 \\bar{x_j} + \\beta_2 + \\mu_j$<br>
                                      $\\mu_j \\sim \\mathcal{N}(0, \\sigma_{site}^2)$<br>
                                      
                                      <br><br>
                                      <b>Panel model</b> - assumed 1 measurement per site per year: <br>
                                      <code>lmer(delta_y ~ delta_x + (1|site))</code><br>
                                      $\\Delta y_{ij} \\sim \\mathcal{N}(\\widehat{\\Delta y_{ij}}, \\sigma^2)$<br>
                                      $\\widehat{\\Delta y_{ij}} = \\beta_0 \\Delta_x{ij} + \\beta_2 + \\mu_j$<br>
                                      $\\mu_j \\sim \\mathcal{N}(0, \\sigma_{site}^2)$<br>
                                      <br><br>
                                      
                                      Code for app is at <a href=https://github.com/jebyrnes/ovb_yeah_you_know_me/tree/master/app>github</a>")),
                        
                        tabPanel("System Diagram",
                                 fluidRow(align="center",
                                 br(),br(),
                                 grVizOutput("system"))),
                        
                        tabPanel("Temperature-Recruitment", 
                                 plotOutput("temp_rec_plot"),
                                 br(),
                                 tableOutput("temp_rec_tab")),
                        
                        tabPanel("Sampled Data", 
                                 plotOutput("sampled_data")),
                        
                        tabPanel("Temperature Effect on Snails", 
                                 HTML("These are the coefficients for the relevant term
                                      in each model that dives the estimated effect of
                                      temperature on snail abundance. For the GMC model
                                      this is $x-\\bar{x_j}$ and for the panel model, this 
                                      is $\\Delta x$"),
                                 tableOutput("coefs")
                        ),
                        tabPanel("Full Model Results", 
                                 HTML("The coefficients for all models. Compare and
                                      contrast values for similar terms."),
                                 tableOutput("full_models_out")
                        )
            )
            
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    # Reactives ####
    
    # site data frame reactive
    site_dat <- reactive({
        
        #update on...
        input$run
        
        seed <- NULL
        
        isolate({
            if(input$use_seed){seed <- input$seed}
        })
        
        isolate({
        make_environment(input$n_sites,
                         input$ocean_temp,
                         input$temp_sd,
                         input$ocean_recruitment,
                         input$recruitment_sd,
                         input$temp_mean,
                         input$rec_mean,
                         seed)
        })
    })
    
    # plot data frame reactive
    
    plot_dat <- reactive({
        #update on...
        input$run
        seed <- NULL
        
        isolate({
        if(input$use_seed){seed <- input$seed}
        })
        
        isolate({
         make_plots(site_dat(),
                   input$n_plots_per_site,
                   input$plot_temp_sd,
                   input$temp_effect,
                   input$recruitment_effect,
                   input$sd_plot,
                   seed)
        })
    })
    
    # analyzed data reactive
    # plot data frame reactive
    
    analysis_dat <- reactive({
        #update on...
        input$run
        isolate({
            analyze_plots(plot_dat())
        })
        
    })
    
    
    # Outputs ####
    output$system <- renderGrViz({
        #update on...
        input$run
        
        isolate({
         make_snail_graph( 
            or = input$ocean_recruitment,
            ot = input$ocean_temp,
            rs = input$recruitment_effect,
            ts = input$temp_effect,
            rsd = input$recruitment_sd,
            tsd = input$temp_sd,
            esd = 1,
            lsd = input$plot_temp_sd)
        })
        
    })
    
    #information about site-level temperature-recruitment things
    output$temp_rec_plot <- renderPlot({
        ggplot(site_dat(),
               aes(x = site_temp, y = site_recruitment)) +
            geom_point(size = 4) +
            stat_smooth(method = "lm", size = 0.5, alpha = 0.5,
                        fill = "grey", color = "black",
                        formula = y ~ x) +
            labs(x = "mean site temperature",
                 y = "mean site recruitment",
                 title = "Relationship between temperature and recruitment across sites")
    })
    
    output$temp_rec_tab <- renderTable({
        lm(site_recruitment ~ site_temp, data = site_dat()) %>%
            tidy()
    })
    
    #what's happening in the plots!
    output$sampled_data <- renderPlot({
        ggplot(plot_dat(),
               aes(x = plot_temp, y = snails, color = site)) +
            geom_point(size = 4)+
            labs(x = "plot temperature",
                 y = "snails",
                 color = "site",
                 title = "Sampled temperature-snail relationship")
        
    })
    
    #analysis!
    output$coefs <- renderTable({
        analysis_dat() %>%
            select(model_type, temp_effect) %>%
            unnest(temp_effect) 
    })
    
    output$full_models_out <- renderTable({
        analysis_dat() %>%
            select(model_type, coefs) %>%
            unnest(coefs)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
