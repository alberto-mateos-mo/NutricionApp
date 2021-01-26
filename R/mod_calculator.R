#' calculator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_calculator_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyMobile::f7Page(
      title = "",
      shinyMobile::f7TabLayout(
        panels = tagList(
          shinyMobile::f7Panel(
            side = "left", theme = "light", effect = "cover",
            "Peso (Kg)",
            shinyMobile::f7Stepper(ns("peso1"), label = "", min = 0, max = 30, value = 1),
            "Peso (g)",
            shinyMobile::f7Stepper(ns("peso2"), label = "", min = 0, max = 1000, value = 100),
            "Req. Líquido",
            shinyMobile::f7Stepper(ns("liq"), label = "", min = 80, max = 280, value = 140),
            "Na (mEq)",
            shinyMobile::f7Stepper(ns("na"), label = "", min = 2, max = 3, value = 2),
            "K (mEq)",
            shinyMobile::f7Stepper(ns("k"), label = "", min = 1, max = 3, value = 2),
            "Ca (mEq)",
            shinyMobile::f7Stepper(ns("ca"), label = "", min = 50, max = 400, value = 250),
            "Aminoacidos (gr)",
            shinyMobile::f7Stepper(ns("amino"), label = "", min = 1, max = 3, value = 1.5, step = 0.1),
            "Lípidos (gr)",
            shinyMobile::f7Stepper(ns("lipi"), label = "", min = 1, max = 2, value = 1, step = 0.1),
            "Carbohidratos (gr)",
            shinyMobile::f7Stepper(ns("carbo"), label = "", min = 4, max = 15, value = 6, step = 0.1)
          )
        ),
        navbar = shinyMobile::f7Navbar(
          title = "Protocolo de Nutrición Parental Total",
          left_panel = TRUE
        ),
        shinyMobile::f7Tabs(
          animated = TRUE,
          id = "tabs",
          shinyMobile::f7Tab(
            tabName = "Métricas",
            icon = shinyMobile::f7Icon("textformat_123"),
            shinyMobile::f7Card(
              title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: black"), icon("vial", "fa-2x"), "Total."),
              outline = TRUE,
              div(style = ("font-size: 40px; font-weight: bold;"),
                  textOutput(ns("total"))
              )
            ),
            shinyMobile::f7Card(
              title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: black"), icon("vial", "fa-2x"), "Subtotal."),
              outline = TRUE,
              div(style = ("font-size: 40px; font-weight: bold;"),
                  textOutput(ns("subtotal"))
              )
            ),
            shinyMobile::f7Card(
              title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: black"), icon("vial", "fa-2x"), "Líquidos totales."),
              outline = TRUE,
              div(style = ("font-size: 40px; font-weight: bold;"),
                  textOutput(ns("liqtot"))
              )
            ),
            shinyMobile::f7Card(
              title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: black"), icon("vial", "fa-2x"), "Líquidos restantes."),
              outline = TRUE,
              div(style = ("font-size: 40px; font-weight: bold;"),
                  textOutput(ns("liqrest"))
              )
            ),
            shinyMobile::f7Card(
              title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: black"), icon("vial", "fa-2x"), "Mililitros de glucosa (50%)"),
              outline = TRUE,
              div(style = ("font-size: 40px; font-weight: bold;"),
                  textOutput(ns("milgluc"))
              )
            ),
            shinyMobile::f7Card(
              title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: black"), icon("vial", "fa-2x"), "Mililitros de glucosa (10%)"),
              outline = TRUE,
              div(style = ("font-size: 40px; font-weight: bold;"),
                  textOutput(ns("milgluc10"))
              )
            ),
            shinyMobile::f7Card(
              title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: black"), icon("vial", "fa-2x"), "Mililitros de Agua Inyectable"),
              outline = TRUE,
              div(style = ("font-size: 40px; font-weight: bold;"),
                  textOutput(ns("milagua"))
              )
            )
          ),
          shinyMobile::f7Tab(
            tabName = "Kcal Admin",
            icon = shinyMobile::f7Icon("info"),
            shinyMobile::f7Card(
              title = "Kcal por sustrato",
              div(align = "center", tableOutput(ns("kcal")))
            ),
            shinyMobile::f7Card(
              title = "Concentración en la NTP",
              div(align = "center", tableOutput(ns("ntp")))
            ),
            shinyMobile::f7Card(
              title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: black"), icon("vial", "fa-2x"), "Total de Kcal."),
              outline = TRUE,
              div(style = ("font-size: 40px; font-weight: bold;"),
                  textOutput(ns("totkcal"))
              )
            ),
            shinyMobile::f7Card(
              title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: black"), icon("vial", "fa-2x"), "Relación calórica no proteica."),
              outline = TRUE,
              div(style = ("font-size: 40px; font-weight: bold;"),
                  textOutput(ns("rel"))
              )
            )
          ),
          shinyMobile::f7Tab(
            tabName = "Resultados",
            icon = shinyMobile::f7Icon("table"),
            shinyMobile::f7Card(
              title = "Tabla de resultados",
              div(align = "center", DT::DTOutput(ns("tabla")))
            )
          )
        )
      )
    )
  )
}
    
#' calculator Server Function
#'
#' @noRd 
mod_calculator_server <- function(input, output, session){
  ns <- session$ns
  
  ## Métricas
  
  peso <- reactive({
    round(as.numeric((input$peso1 + (input$peso2/1000))),2)
  })

  tmp <- reactive({
    
    ## Cálculo para Na
    tabla$Dosis[1] <- paste0(input$na, "mEq/kg/d")
    tabla$`Total de Nutriente`[1] <- input$na*peso()
    tabla$`Volumen (ml)`[1] <- (tabla$`Total de Nutriente`[1]*100)/15.4
    
    ## Cálculo para K
    tabla$Dosis[2] <- paste0(input$k, "mEq/kg/d")
    tabla$`Total de Nutriente`[2] <- input$k*peso()
    tabla$`Volumen (ml)`[2] <- (tabla$`Total de Nutriente`[2]/2)
    
    ## Cálculo para Ca
    tabla$Dosis[3] <- paste0(input$ca, "mg/kg/d")
    tabla$`Total de Nutriente`[3] <- input$ca*peso()
    tabla$`Volumen (ml)`[3] <- (tabla$`Total de Nutriente`[3]/100)
    
    ## Cálculo para Mg
    tabla$`Total de Nutriente`[4] <- 50*peso()
    tabla$`Volumen (ml)`[4] <- (tabla$`Total de Nutriente`[4]/100)
    
    ## Cálculo para oligoelementos
    tabla$`Total de Nutriente`[5] <- 0.1*peso()
    tabla$`Volumen (ml)`[5] <- tabla$`Total de Nutriente`[5]
    
    ## Cálculo para MVI
    tabla$`Total de Nutriente`[6] <- peso()
    tabla$`Volumen (ml)`[6] <- peso()
    
    ## Cálculo para aminoácidos
    tabla$Dosis[7] <- paste0(input$amino, "gr/kg/d")
    tabla$`Total de Nutriente`[7] <- input$amino*peso()
    tabla$`Volumen (ml)`[7] <- tabla$`Total de Nutriente`[7]*10
    
    ## Cálculo para lípidos
    tabla$Dosis[8] <- paste0(input$lipi, "gr/kg/d")
    tabla$`Total de Nutriente`[8] <- input$lipi*peso()
    tabla$`Volumen (ml)`[8] <- (tabla$`Total de Nutriente`[8]*10)/2
    
    ## Cálculo para carbo
    tabla$Dosis[9] <- paste0(input$carbo, "mg/kg/min")
    tabla$`Total de Nutriente`[9] <- (input$carbo*peso()*1440)/1000
    
    return(tabla)
  })
  
  liq_tot <- reactive({
    input$liq*peso()
  })
  
  liq_rest <- reactive({
    round(liq_tot()-sum(tmp()$`Volumen (ml)`, na.rm = TRUE), 2)
  })
  
  mil_gluc <- reactive({
    gr_g <- (input$carbo*peso()*1440)/1000
    res <- ((gr_g*10)-liq_rest())/4
    ifelse(res>=0, round(res, 2), "Valor inválido")
  })
  
  gluc_10 <- reactive({
    ifelse(mil_gluc() == "Valor inválido", ((input$carbo*peso()*1440)/1000)*10,
           liq_rest()-mil_gluc())
  })
  
  agua_i <- reactive({
    ifelse(mil_gluc() == "Valor inválido", round(liq_rest()-gluc_10(), 2), 
           "No aplica")
  })
  
  subtot <- reactive({
    sum(tmp()$`Volumen (ml)`, na.rm = TRUE)
  })
  
  total <- reactive({
    ifelse(mil_gluc() != "Valor inválido",
           sum(tmp()$`Volumen (ml)`, na.rm = TRUE) + gluc_10() + mil_gluc(),
           gluc_10()+agua_i()+subtot())
  })
  
  output$total <- renderText({
    paste(round(total(), 2), "ml")
  })  
  
  output$subtotal <- renderText({
    paste(round(subtot(), 2), "ml")
  })

  output$liqtot <- renderText({
    paste(round(liq_tot(), 2), "ml")
  })
  
  output$liqrest <- renderText({
    paste(round(liq_rest(), 2), "ml")
  })
  
  output$milgluc <- renderText({
    mil_gluc()
  })
  
  output$milgluc10 <- renderText({
    round(gluc_10(), 2)
  })
  
  output$milagua <- renderText({
    agua_i()
  })
  
  ## Kcal Administradas
  
  kcal <- reactive({
    gluc_kcal <- ((input$carbo*peso()*1440)/1000)*3.4
    lip_kcal <- tmp()$`Total de Nutriente`[8]*9
    amino_kcal <- tmp()$`Total de Nutriente`[7]*4
    
    data.frame(Sustrato = c("Glucosa", "Lípidos", "Aminoacidos"),
               Kcal = c(gluc_kcal, lip_kcal, amino_kcal))
  })
  
  concentracion <- reactive({
    cho <- (((input$carbo*peso()*1440)/1000)*100)/liq_tot()
    aa <- (tmp()$`Total de Nutriente`[7]*100)/liq_tot()
    
    data.frame(Sustrato = c("CHO", "AA"),
               Concentración = c(cho, aa))
  })
  
  rel <- reactive({
    a <- round(sum(kcal()$Kcal[1:2]), 2)
    b <- kcal()$Kcal[3]/6.25
    a/b
  })
  
  output$kcal <- renderTable({
    kcal()
  })
  
  output$ntp <- renderTable({
    concentracion()
  })
  
  output$totkcal <- renderText({
    round(sum(kcal()$Kcal), 2)
  })
 
  output$rel <- renderText({
    round(rel(), 2)
  })
  
  output$tabla <- DT::renderDT({
    DT::datatable(tmp(), rownames = F, 
                  options = list(scrollX = TRUE, fixedColumns = TRUE))
  })
  
}
    
## To be copied in the UI
# mod_calculator_ui("calculator_ui_1")
    
## To be copied in the server
# callModule(mod_calculator_server, "calculator_ui_1")
 
