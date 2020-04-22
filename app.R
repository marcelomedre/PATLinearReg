#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(tibble)


ui <- dashboardPage(
        dashboardHeader(
                title = "Process Analytical Technology - Controle Peso vs Força Compressão",
                titleWidth = 1000),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Camada 1", tabName = "camada1", icon = icon("chart-line")),
                        menuItem("Camada 2", tabName = "camada2", icon = icon("chart-line")),
                        menuItem("Camada 3", tabName = "camada3", icon = icon("chart-line")),
                        menuItem("Parâmetros Calculados", tabName = "parcalc", icon = icon("code")),
                        menuItem("Histórico de Versões", tabName = "versions", icon = icon("file-alt")),
                        br(),
                        br(),
                        tags$div(class="header", align = "center", checked=NA,
                                 list(
                                         tags$p("APP Desenvolvido por"),
                                         tags$p("Marcelo M. Nobrega"),
                                         br(),
                                         tags$p("Para reportar um bug,"),
                                         tags$p("sugerir uma melhoria ou"),
                                         tags$p("fazer um comentário."),
                                         tags$a(href="mailto:marcelo.nobrega@eurofarma.com.br", "Clique Aqui!"),
                                         "Obrigado"
                                 )
                        )
                )
        ),
        dashboardBody(
                tabItems(
                        #Camada 1 tab content
                        tabItem(tabName = "camada1",
                                fluidRow(
                                        box(
                                                title = "Dados da camada 1",
                                                tags$b("Entrada de Dados:"),
                                                tags$b("Entrada de Dados Camada 1:"),
                                                textInput("x1", "Força Compressão camada 1 (KN)", value = "7, 8, 10, 11, 12, 13",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 5.1, 10.2, 11.3"),
                                                textInput("y1", "Peso da camada 1 (mg)", value = "225, 235, 237, 239, 245, 255",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 230.1, 310.2, 411.3"),
                                                textInput("peso1", label = "Peso Desejado da Camada 1 (mg):", value = "240", placeholder = "240"),
                                                textInput("desvio1", label = "Desvio de Peso aceitável na camada 1 + ou - (%):", value = "5", placeholder = "5"),
                                                tags$b("Validação dos Dados da Camada 1:"),
                                                textInput("xval1", "Força Compressão camada 1 (KN)", value = "9.3, 9.5, 10.4, 10.8, 11, 11.3",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 5.1, 10.2, 11.3"),
                                                textInput("yval1", "Peso da camada 1 (mg)", value = "238, 238.5, 239.4, 239, 243, 245",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 230.1, 310.2, 411.3"),
                                                hr(),
                                                hr(),
                                                tags$b("Plot:"),
                                                textInput("xlab", label = "Título do eixo x:", value = "Força de Compressão kN", placeholder = "xlabel"),
                                                textInput("ylab", label = "Título do eixo y:", value = "Peso camada 1 mg", placeholder = "ylabel"),
                                                checkboxInput("se", "Inserir intervalo de confiança na regressão?", FALSE),
                                                hr(),
                                                radioButtons("format", "Download report:", c("HTML", "PDF", "Word"),
                                                             inline = TRUE
                                                ),
                                                #checkboxInput("echo", "Show code in report?", FALSE),
                                                downloadButton("downloadReport"),
                                                hr()
                                        ),
                                        box(
                                                tags$b("Seus Dados:"),
                                                DT::dataTableOutput("tbl1"),
                                                br(),
                                                uiOutput("data1"),
                                                br(),
                                                plotOutput("plot1", height = 500),
                                                br(),
                                                tags$b("Interpretação:"),
                                                uiOutput("interpretation1"),
                                                br(),
                                                tags$b("Faixa de Força de Trabalho para o peso desejado na Camada 1:"),
                                                uiOutput("pesodesejado1")
                                        )
                                )),
                        tabItem(tabName = "camada2",
                                fluidRow(
                                        box(
                                                title = "Dados da camada 2",
                                                textInput("f12", "Força Compressão camada 1 (KN)", value = "11, 11.5, 11.7, 11.9, 12, 12.5",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 5.1, 10.2, 11.3"),
                                                textInput("x2", "Força Compressão camada 2 (KN)", value = "15, 16, 17, 18, 19, 20",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 5.1, 10.2, 11.3"),
                                                textInput("y2", "Peso total (Camada 1 + camada 2) (mg)", value = "330, 335, 337, 342, 345, 350",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 230.1, 310.2, 411.3"),
                                                textInput("peso2", label = "Peso Desejado da Camada 2 (mg):", value = "90", placeholder = "90"),
                                                textInput("desvio2", label = "Desvio de Peso aceitável na camada 2 + ou - (%):", value = "5", placeholder = "5"),
                                                hr(),
                                                hr(),
                                                tags$b("Plot:"),
                                                textInput("xlab2", label = "Título do eixo x:", value = "Força de Compressão kN", placeholder = "xlabel"),
                                                textInput("ylab2", label = "Título do eixo y:", value = "Peso camada 2 mg", placeholder = "ylabel"),
                                                checkboxInput("se2", "Inserir intervalo de confiança na regressão?", FALSE),
                                                hr()
                                                
                                        ),
                                        box(
                                                DT::dataTableOutput("tbl2"),
                                                br(),
                                                uiOutput("data2"),
                                                br(),
                                                plotOutput("plot2", height = 500),
                                                br(),
                                                tags$b("Interpretação:"),
                                                uiOutput("interpretation2"),
                                                br(),
                                                tags$b("Faixa de Força de Trabalho para o peso desejado na Camada 2:"),
                                                uiOutput("pesodesejado2"),
                                                br()
                                                
                                        )
                                )),
                        tabItem(tabName = "camada3",
                                fluidRow(
                                        box(
                                                title = "Dados da camada 3",
                                                textInput("f13", "Força Compressão camada 1 (KN)", value = "11, 11.5, 11.7, 11.9, 12, 12.5",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 5.1, 10.2, 11.3"),
                                                textInput("f23", "Força Compressão camada 2 (KN)", value = "14, 14.5, 15.7, 14.9, 15, 16.5",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 5.1, 10.2, 11.3"),
                                                textInput("x3", "Força Compressão camada 3 (KN)", value = "21, 22, 23, 24, 25, 26",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 5.1, 10.2, 11.3"),
                                                textInput("y3", "Peso total (camada 1 + camada 2 + camada 3) (mg)", value = "430, 435, 439, 442, 445, 450",
                                                          placeholder = "Insira os valores separados por vírgula, p.ex. 230.1, 310.2, 411.3"),
                                                textInput("peso3", label = "Peso Desejado da Camada 3 (mg):", value = "120", placeholder = "120"),
                                                textInput("desvio3", label = "Desvio de Peso aceitável na camada 3 + ou - (%):", value = "5", placeholder = "5"),
                                                hr(),
                                                hr(),
                                                tags$b("Plot:"),
                                                textInput("xlab3", label = "Título do eixo x:", value = "Força de Compressão kN", placeholder = "xlabel"),
                                                textInput("ylab3", label = "Título do eixo y:", value = "Peso camada 3 mg", placeholder = "ylabel"),
                                                checkboxInput("se3", "Inserir intervalo de confiança na regressão?", FALSE),
                                                hr()
                                        ),
                                        box(
                                                DT::dataTableOutput("tbl3"),
                                                br(),
                                                uiOutput("data3"),
                                                br(),
                                                plotOutput("plot3", height = 500),
                                                br(),
                                                tags$b("Interpretação:"),
                                                uiOutput("interpretation3"),
                                                br(),
                                                tags$b("Faixa de Força de Trabalho para o peso desejado na Camada 2:"),
                                                uiOutput("pesodesejado3"),
                                                br()
                                        )
                                )),
                        tabItem(tabName = "parcalc",
                                fluidRow(
                                        box(
                                                tags$b("Camada 1"),
                                                verbatimTextOutput("summary1")
                                        ),
                                        box(
                                                tags$b("Camada 2"),
                                                verbatimTextOutput("summary2")
                                        ),
                                        box(
                                                tags$b("Camada 3"),
                                                verbatimTextOutput("summary3")
                                        )
                                )),
                        tabItem(tabName = "versions",
                                fluidRow(column(3, h3("Histórico de versões:"))),
                                fluidRow(column(6, 
                                                div(br(), br(),
                                                    p("V.0 = Layout sem side bar e 2 camadas"),
                                                    br(),
                                                    p("V.1 = Layout com side bar e 3 camadas"),
                                                    br(),
                                                    p("V.2 = Correção bug Impressão e histórico de versões"),
                                                    )))
                        ))
        )
)


server <- function(input, output) {
        extract <- function(text){
                text <- gsub(" ", " ", text)
                split <- strsplit(text, ",", fixed = FALSE)[[1]]
                as.numeric(split)
        }
        
        #data output 1
        output$tbl1 <- DT::renderDataTable({
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                yval1 <- extract(input$yval1)
                xval1 <- extract(input$xval1)
                DT::datatable(data.frame(x1, y1, xval1, yval1),
                              extensions = "Buttons",
                              colnames = c('Força camada 1 (kN)', 'Peso (mg)', 'Força Validação camada 1 (kN)', 'Peso Validação (mg)'),
                              options = list(
                                      lengthChange = FALSE,
                                      dom = "Blfrtip",
                                      buttons = c("copy", "csv", "excel", "pdf", "print")
                              ))
        })
        output$plot1 <- renderPlot({
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                yval1 <- extract(input$yval1)
                xval1 <- extract(input$xval1)
                #dat <- as.data.frame(x,y)
                fit1 <- lm(y1 ~ x1)
                newx1 <- seq(min(x1), max(x1),by = 0.05)
                conf_interval1 <- predict(fit1, newdata = data.frame(x1 = newx1), 
                                          interval="confidence",
                                          level = 0.95)
                yvalid <- round(predict(fit1, newdata = data.frame(x1 = xval1)),2)
                mse_train <- mean(fit1$residuals^2) 
                mse_val <- mean((yval1 - yvalid)^2)
                plot(x1, y1, pch = 19, col = "black", cex = 1.5, 
                     xlab = input$xlab, 
                     ylab = input$ylab)
                abline(fit1, col = "red", lwd = 2)
                points(xval1, yval1, pch = 19, col = "blue", cex = 1.5)
                points(xval1, yvalid, col = "salmon", cex = 2)
                legend("topleft", inset = 0.05, legend = c("Pontos Exp.", "fit", "Val. Exp.", "Val. Calc."),
                       col = c("black", "red", "blue", "salmon"), lty=1:2, cex=1) 
                if(input$se == TRUE){
                        lines(newx1, conf_interval1[,2], col="blue", lty=2)
                        lines(newx1, conf_interval1[,3], col="blue", lty=2)
                        legend("topleft", inset = 0.05, legend = c("Pontos Exp.", "fit", "Val. Exp.","Val. Calc.", "C.I 95%"),
                               col = c("black", "red", "blue", "salmon", "blue"), lty=1:2, cex=1)                       
                }
        })
        output$pesodesejado1 <- renderUI({
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                yval1 <- extract(input$yval1)
                xval1 <- extract(input$xval1)
                #dat <- as.data.frame(x,y)
                fit1 <- lm(y1 ~ x1)
                yvalid <- round(predict(fit1, newdata = data.frame(x1 = xval1)),2)
                mse_train <- round(mean(fit1$residuals^2),2) 
                mse_val <- round(mean((yval1 - yvalid)^2),2)
                ymin1 <- extract(input$peso1)*(1 - extract(input$desvio1)/100)
                ymax1 <- extract(input$peso1)*(1 + extract(input$desvio1)/100)
                fmin1 <- round((ymin1 - (fit1$coefficients[1]))/fit1$coefficients[2],2)
                fmed1 <- round((extract(input$peso1) - (fit1$coefficients[1]))/fit1$coefficients[2],2)
                fmax1 <- round((ymax1 - (fit1$coefficients[1]))/fit1$coefficients[2],2)
                withMathJax(
                        br(),
                        paste0("Erro quadrático médio modelo: ", mse_train, "mg"),
                        br(),
                        paste0("Erro quadrático médio validação: ", mse_val, "mg"),
                        br(),
                        br(),
                        paste0("Peso mínimo aceitável: ", ymin1, "mg"),
                        br(),
                        paste0("Peso máximo aceitável: ", ymax1, "mg"),
                        br(),
                        br(),
                        paste0("Força média: ", fmed1, "kN"),
                        br(),
                        paste0("Força mínima: ", fmin1, "kN"),
                        br(),
                        paste0("Força máxima: ", fmax1, "kN"),
                        br(),
                        br()
                )
        })
        
        output$data1 <- renderUI({
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                if(anyNA(x1) | length(x1) < 2 | anyNA(y1) | length(y1) < 2) {
                        "Entrada Inválida ou quantidade de dados insuficiente"
                } else if (length (x1) != length (y1)){
                        "NÃºmero de dados de x deve ser igual ao número de dados de y"
                } else {
                        withMathJax(
                                paste0("\\(\\bar{x} = \\) ", round(mean(x1), 3)),
                                br(),
                                paste0("\\(\\bar{y} = \\) ", round(mean(y1), 3)),
                                br(),
                                paste0("\\(n = \\) ", length(x1))
                        )
                }
        })
        output$summary1 <- renderPrint({
                y1 <- as.numeric(extract(input$y1))
                x1 <- as.numeric(extract(input$x1))
                fit1 <- lm(y1 ~ x1)
                summary(fit1)
        })
        output$interpretation1 <- renderUI({
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                if (summary(fit1)$coefficients[1, 4] < 0.05 & summary(fit1)$coefficients[2, 4] < 0.05) {
                        withMathJax(
                                paste0("Para o valor hipotético de ", input$xlab, " = 0, o valor médio de ", input$ylab, " = ", round(fit1$coef[[1]], 3), "."),
                                br(),
                                paste0("Para o aumento de uma unidade em ", input$xlab, ", ", input$ylab, ifelse(round(fit1$coef[[2]], 3) >= 0, " aumenta (em média) ", " diminui (em média) "), abs(round(fit1$coef[[2]], 3)), ifelse(abs(round(fit1$coef[[2]], 3)) >= 2, " mg", " unidade"), ".")
                        )
                } else if (summary(fit1)$coefficients[1, 4] < 0.05 & summary(fit1)$coefficients[2, 4] >= 0.05) {
                        withMathJax(
                                paste0("Para o valor hipotético de ", input$xlab, " = 0, o valor médio de ", input$ylab, " = ", round(fit1$coef[[1]], 3), "."),
                                br(),
                                paste0("\\( \\beta_1 \\)", " não é significativamente diferente de 0 (p-value = ", round(summary(fit1)$coefficients[2, 4], 3), ") entãoo, não há uma relação significativa entre as variáveis ", input$xlab, " e ", input$ylab, ".")
                        )
                } else if (summary(fit1)$coefficients[1, 4] >= 0.05 & summary(fit1)$coefficients[2, 4] < 0.05) {
                        withMathJax(
                                paste0("\\( \\beta_0 \\)", " nÃ£o Ã© significativamente diferente de 0 (p-value = ", round(summary(fit1)$coefficients[1, 4], 3), ") então quando ", input$xlab, " = 0, o valor médio de ", input$ylab, " não é significativamente diferente de 0."),
                                br(),
                                paste0("Para o aumento de uma unidade em x ", input$xlab, ", ", input$ylab, ifelse(round(fit1$coef[[2]], 3) >= 0, " aumenta (em média) ", " diminui (em média) "), abs(round(fit1$coef[[2]], 3)), ifelse(abs(round(fit1$coef[[2]], 3)) >= 2, " Unidades", " mg"), ".")
                        )
                } else {
                        withMathJax(
                                paste0("\\( \\beta_0 \\)", " e ", "\\( \\beta_1 \\)", " não são significativamente diferentes de 0 (p-values = ", round(summary(fit1)$coefficients[1, 4], 3), " e ", round(summary(fit1)$coefficients[2, 4], 3), ", respectivamente) então, a média de ", input$ylab, " não é significativamente diferente de 0.")
                        )
                }
        })
        #data output Camada 2
        output$tbl2 <- DT::renderDataTable({
                y2 <- extract(input$y2)
                x2 <- extract(input$x2)
                #fit da camada 1 para camada 2
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                pcam2 <- y2 - p12
                DT::datatable(data.frame(f12, p12, x2, y2, pcam2),
                              extensions = "Buttons",
                              colnames = c('Força cam. 1 (kN)', 'Peso est. cam. 1 mg', 'Força cam. 2 (kN)', "Peso total mg", "Peso camada 2 mg"),
                              options = list(
                                      lengthChange = FALSE,
                                      dom = "Blfrtip",
                                      buttons = c("copy", "csv", "excel", "pdf", "print")
                              ))
        })
        output$plot2 <- renderPlot({
                p2 <- extract(input$y2)
                x2 <- extract(input$x2)
                #dat <- as.data.frame(x,y)
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                y2 <- as.numeric(p2) - as.numeric(p12)
                fit2 <- lm(y2 ~ x2)
                newx2 = seq(min(x2),max(x2),by = 0.05)
                conf_interval2 <- predict(fit2, newdata=data.frame(x2 = newx2), 
                                          interval="confidence",
                                          level = 0.95)
                plot(x2, y2, pch = 1,  
                     xlab = input$xlab2, 
                     ylab = input$ylab2)
                abline(fit2, col = "red", lwd = 2)
                legend("topleft", inset = 0.05, legend = c("Pontos Exp.", "fit"),
                       col = c("black", "red"), lty=1:2, cex=1) 
                if(input$se2 == TRUE){
                        lines(newx2, conf_interval2[,2], col="blue", lty=2)
                        lines(newx2, conf_interval2[,3], col="blue", lty=2)
                        legend("topleft", inset = 0.05, legend = c("Pontos Exp.", "fit", "C.I 95%"),
                               col = c("black", "red", "blue"), lty=1:2, cex=1)                       
                }
                
        })
        output$pesodesejado2 <- renderUI({
                p2 <- extract(input$y2)
                x2 <- extract(input$x2)
                #dat <- as.data.frame(x,y)
                #dat <- as.data.frame(x,y)
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                y2 <- as.numeric(p2) - as.numeric(p12)
                fit2 <- lm(y2 ~ x2)
                ymin2 <- extract(input$peso2)*(1 - extract(input$desvio2)/100)
                ymax2 <- extract(input$peso2)*(1 + extract(input$desvio2)/100)
                fmin2 <- round((ymin2 - (fit2$coefficients[1]))/fit2$coefficients[2],2)
                fmed2 <- round((extract(input$peso2) - (fit2$coefficients[1]))/fit2$coefficients[2],2)
                fmax2 <- round((ymax2 - (fit2$coefficients[1]))/fit2$coefficients[2],2)
                withMathJax(
                        paste0("Peso mínimo aceitável camada 2: ", ymin2, "mg"),
                        br(),
                        paste0("Peso máximo aceitável camada 2: ", ymax2, "mg"),
                        br(),
                        br(),
                        paste0("Força média camada 2: ", fmed2, "kN"),
                        br(),
                        paste0("Força mínima camada 2: ", fmin2, "kN"),
                        br(),
                        paste0("Força máxima camada 2: ", fmax2, "kN"),
                        br(),
                        br()
                )
        })
        
        output$data2 <- renderUI({
                y2 <- extract(input$y2)
                x2 <- extract(input$x2)
                if(anyNA(x2) | length(x2) < 2 | anyNA(y2) | length(y2) < 2) {
                        "Entrada Inválida ou quantidade de dados insuficiente"
                } else if (length (x2) != length (y2)){
                        "Numero de dados de x deve ser igual ao número de dados de y"
                } else {
                        withMathJax(
                                paste0("\\(\\bar{x} = \\) ", round(mean(x2), 3)),
                                br(),
                                paste0("\\(\\bar{y} = \\) ", round(mean(y2), 3)),
                                br(),
                                paste0("\\(n = \\) ", length(x2))
                        )
                }
        })
        output$summary2 <- renderPrint({
                p2 <- as.numeric(extract(input$y2))
                x2 <- as.numeric(extract(input$x2))
                #dat <- as.data.frame(x,y)
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                y2 <- as.numeric(p2) - as.numeric(p12)
                fit2 <- lm(y2 ~ x2)
                summary(fit2)
        })
        output$interpretation2 <- renderUI({
                p2 <- as.numeric(extract(input$y2))
                x2 <- as.numeric(extract(input$x2))
                #dat <- as.data.frame(x,y)
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                y2 <- as.numeric(p2) - as.numeric(p12)
                fit2 <- lm(y2 ~ x2)
                summary(fit2)
                if (summary(fit2)$coefficients[1, 4] < 0.05 & summary(fit2)$coefficients[2, 4] < 0.05) {
                        withMathJax(
                                paste0("Para o valor hipotético de ", input$xlab, " = 0, o valor médio de ", input$ylab, " = ", round(fit2$coef[[1]], 3), "."),
                                br(),
                                paste0("Para o aumento de uma unidade em ", input$xlab, ", ", input$ylab, ifelse(round(fit2$coef[[2]], 3) >= 0, " aumenta (em média) ", " diminui (em média) "), abs(round(fit2$coef[[2]], 3)), ifelse(abs(round(fit2$coef[[2]], 3)) >= 2, " mg", " unidade"), ".")
                        )
                } else if (summary(fit2)$coefficients[1, 4] < 0.05 & summary(fit2)$coefficients[2, 4] >= 0.05) {
                        withMathJax(
                                paste0("Para o valor hipotético de ", input$xlab, " = 0, o valor médio de ", input$ylab, " = ", round(fit2$coef[[1]], 3), "."),
                                br(),
                                paste0("\\( \\beta_1 \\)", " não é significativamente diferente de 0 (p-value = ", round(summary(fit2)$coefficients[2, 4], 3), ") então, não há uma relação significativa entre as variáveis ", input$xlab, " e ", input$ylab, ".")
                        )
                } else if (summary(fit2)$coefficients[1, 4] >= 0.05 & summary(fit2)$coefficients[2, 4] < 0.05) {
                        withMathJax(
                                paste0("\\( \\beta_0 \\)", " não é significativamente diferente de 0 (p-value = ", round(summary(fit2)$coefficients[1, 4], 3), ") então quando ", input$xlab, " = 0, o valor médio de ", input$ylab, " não é significativamente diferente de 0."),
                                br(),
                                paste0("Para o aumento de uma unidade em x ", input$xlab, ", ", input$ylab, ifelse(round(fit2$coef[[2]], 3) >= 0, " aumenta (em média) ", " diminui (em média) "), abs(round(fit2$coef[[2]], 3)), ifelse(abs(round(fit2$coef[[2]], 3)) >= 2, " mg", " unidade"), ".")
                        )
                } else {
                        withMathJax(
                                paste0("\\( \\beta_0 \\)", " e ", "\\( \\beta_1 \\)", " não são significativamente diferentes de 0 (p-values = ", round(summary(fit2)$coefficients[1, 4], 3), " e ", round(summary(fit2)$coefficients[2, 4], 3), ", respectivamente) então, a média de ", input$ylab, " não é significativamente diferente de 0.")
                        )
                }
        })
        #data output Camada 3
        output$tbl3 <- DT::renderDataTable({
                p3 <- as.numeric(extract(input$y3))
                x3 <- as.numeric(extract(input$x3))
                #fit da camada 1 para camada 3
                y1 <- as.numeric(extract(input$y1))
                x1 <- as.numeric(extract(input$x1))
                fit1 <- lm(y1 ~ x1)
                f13 <- extract(input$f13)
                p13 <- round(predict(fit1, newdata = data.frame(x1 = f13)),2)
                pcam13 <- p13
                #fit da camada 2 para camada 3
                p2 <- as.numeric(extract(input$y2))
                x2 <- as.numeric(extract(input$x2))
                #dat <- as.data.frame(x,y)
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                y2 <- as.numeric(p2) - as.numeric(p12)
                fit2 <- lm(y2 ~ x2)
                f23 <- extract(input$f23)
                p23 <- round(predict(fit2, newdata = data.frame(x2 = f13)),2)
                pcam23 <- p23
                
                pcam33 <- p3 - (pcam13 + pcam23)
                #fit da camada 2 para camada 3
                DT::datatable(data.frame(f13, p13, f23, p23, x3, p3, pcam33),
                              extensions = "Buttons",
                              colnames = c('Força cam. 1 (kN)', 'Peso est. cam. 1 mg','Força cam. 2 (kN)', 'Peso est. cam. 2 mg', 'Força cam. 3 (kN)', "Peso total mg", "Peso camada 3 mg"),
                              options = list(
                                      lengthChange = FALSE,
                                      dom = "Blfrtip",
                                      buttons = c("copy", "csv", "excel", "pdf", "print")
                              ))
        })
        output$data3 <- renderUI({
                y3 <- extract(input$y3)
                x3 <- extract(input$x3)
                if(anyNA(x3) | length(x3) < 2 | anyNA(y3) | length(y3) < 2) {
                        "Entrada Inválida ou quantidade de dados insuficiente"
                } else if (length (x3) != length (y3)){
                        "Numero de dados de x deve ser igual ao número de dados de y"
                } else {
                        withMathJax(
                                paste0("\\(\\bar{x} = \\) ", round(mean(x3), 3)),
                                br(),
                                paste0("\\(\\bar{y} = \\) ", round(mean(y3), 3)),
                                br(),
                                paste0("\\(n = \\) ", length(x3))
                        )
                }
        })
        output$plot3 <- renderPlot({
                p3 <- extract(input$y3)
                x3 <- extract(input$x3)
                #fit da camada 1 para camada 3
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f13 <- extract(input$f13)
                p13 <- round(predict(fit1, newdata = data.frame(x1 = f13)),2)
                pcam13 <- p13
                #fit da camada 2 para camada 3
                p2 <- as.numeric(extract(input$y2))
                x2 <- as.numeric(extract(input$x2))
                #dat <- as.data.frame(x,y)
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                y2 <- as.numeric(p2) - as.numeric(p12)
                fit2 <- lm(y2 ~ x2)
                f23 <- extract(input$f23)
                p23 <- round(predict(fit2, newdata = data.frame(x2 = f13)),2)
                pcam23 <- p23
                pcam33 <- p3 - (pcam13 + pcam23)
                fit3 <- lm(pcam33 ~ x3)
                newx3 = seq(min(x3),max(x3),by = 0.05)
                conf_interval3 <- predict(fit3, newdata=data.frame(x3 = newx3), 
                                          interval="confidence",
                                          level = 0.95)
                plot(x3, pcam33, pch = 1,  
                     xlab = input$xlab3, 
                     ylab = input$ylab3)
                abline(fit3, col = "red", lwd = 2)
                legend("topleft", inset = 0.05, legend = c("Pontos Exp.", "fit"),
                       col = c("black", "red"), lty=1:2, cex=1) 
                if(input$se3 == TRUE){
                        lines(newx3, conf_interval3[,2], col="blue", lty=2)
                        lines(newx3, conf_interval3[,3], col="blue", lty=2)
                        legend("topleft", inset = 0.05, legend = c("Pontos Exp.", "fit", "C.I 95%"),
                               col = c("black", "red", "blue"), lty=1:2, cex=1)                       
                }
                
        })
        output$interpretation3 <- renderUI({
                p3 <- extract(input$y3)
                x3 <- extract(input$x3)
                #fit da camada 1 para camada 3
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f13 <- extract(input$f13)
                p13 <- round(predict(fit1, newdata = data.frame(x1 = f13)),2)
                pcam13 <- p13
                #fit da camada 2 para camada 3
                p2 <- as.numeric(extract(input$y2))
                x2 <- as.numeric(extract(input$x2))
                #dat <- as.data.frame(x,y)
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                y2 <- as.numeric(p2) - as.numeric(p12)
                fit2 <- lm(y2 ~ x2)
                f23 <- extract(input$f23)
                p23 <- round(predict(fit2, newdata = data.frame(x2 = f13)),2)
                pcam23 <- p23
                pcam33 <- p3 - (pcam13 + pcam23)
                fit3 <- lm(pcam33 ~ x3)
                summary(fit3)
                if (summary(fit3)$coefficients[1, 4] < 0.05 & summary(fit3)$coefficients[2, 4] < 0.05) {
                        withMathJax(
                                paste0("Para o valor hipotético de ", input$xlab, " = 0, o valor médio de ", input$ylab, " = ", round(fit3$coef[[1]], 3), "."),
                                br(),
                                paste0("Para o aumento de uma unidade em ", input$xlab, ", ", input$ylab, ifelse(round(fit3$coef[[2]], 3) >= 0, " aumenta (em média) ", " diminui (em média) "), abs(round(fit3$coef[[2]], 3)), ifelse(abs(round(fit3$coef[[2]], 3)) >= 2, " mg", " unidade"), ".")
                        )
                } else if (summary(fit3)$coefficients[1, 4] < 0.05 & summary(fit3)$coefficients[2, 4] >= 0.05) {
                        withMathJax(
                                paste0("Para o valor hipotético de ", input$xlab, " = 0, o valor médio de ", input$ylab, " = ", round(fit3$coef[[1]], 3), "."),
                                br(),
                                paste0("\\( \\beta_1 \\)", " não é significativamente diferente de 0 (p-value = ", round(summary(fit3)$coefficients[2, 4], 3), ") então, não há uma relação significativa entre as variáveis ", input$xlab, " e ", input$ylab, ".")
                        )
                } else if (summary(fit3)$coefficients[1, 4] >= 0.05 & summary(fit3)$coefficients[2, 4] < 0.05) {
                        withMathJax(
                                paste0("\\( \\beta_0 \\)", " não é significativamente diferente de 0 (p-value = ", round(summary(fit3)$coefficients[1, 4], 3), ") então quando ", input$xlab, " = 0, o valor médio de ", input$ylab, " não é significativamente diferente de 0."),
                                br(),
                                paste0("Para o aumento de uma unidade em x ", input$xlab, ", ", input$ylab, ifelse(round(fit3$coef[[2]], 3) >= 0, " aumenta (em média) ", " diminui (em média) "), abs(round(fit3$coef[[2]], 3)), ifelse(abs(round(fit3$coef[[2]], 3)) >= 2, " mg", " unidade"), ".")
                        )
                } else {
                        withMathJax(
                                paste0("\\( \\beta_0 \\)", " e ", "\\( \\beta_1 \\)", " não são significativamente diferentes de 0 (p-values = ", round(summary(fit2)$coefficients[1, 4], 3), " e ", round(summary(fit2)$coefficients[2, 4], 3), ", respectivamente) então, a média de ", input$ylab, " não é significativamente diferente de 0.")
                        )
                }
        })
        output$pesodesejado3 <- renderUI({
                p3 <- extract(input$y3)
                x3 <- extract(input$x3)
                #fit da camada 1 para camada 3
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f13 <- extract(input$f13)
                p13 <- round(predict(fit1, newdata = data.frame(x1 = f13)),2)
                pcam13 <- p13
                #fit da camada 2 para camada 3
                p2 <- as.numeric(extract(input$y2))
                x2 <- as.numeric(extract(input$x2))
                #dat <- as.data.frame(x,y)
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                y2 <- as.numeric(p2) - as.numeric(p12)
                fit2 <- lm(y2 ~ x2)
                f23 <- extract(input$f23)
                p23 <- round(predict(fit2, newdata = data.frame(x2 = f13)),2)
                pcam23 <- p23
                pcam33 <- p3 - (pcam13 + pcam23)
                fit3 <- lm(pcam33 ~ x3)
                summary(fit3)
                ymin3 <- extract(input$peso3)*(1 - extract(input$desvio3)/100)
                ymax3 <- extract(input$peso3)*(1 + extract(input$desvio3)/100)
                fmin3 <- round((ymin3 - (fit3$coefficients[1]))/fit3$coefficients[2],2)
                fmed3 <- round((extract(input$peso3) - (fit3$coefficients[1]))/fit3$coefficients[2],2)
                fmax3 <- round((ymax3 - (fit3$coefficients[1]))/fit3$coefficients[2],2)
                withMathJax(
                        paste0("Peso mínimo aceitável camada 3: ", ymin3, "mg"),
                        br(),
                        paste0("Peso máximo aceitável camada 3: ", ymax3, "mg"),
                        br(),
                        br(),
                        paste0("Força média camada 3: ", fmed3, "kN"),
                        br(),
                        paste0("Força mínima camada 3: ", fmin3, "kN"),
                        br(),
                        paste0("Força máxima camada 3: ", fmax3, "kN"),
                        br(),
                        br()
                )
        })
        output$summary3 <- renderPrint({
                p3 <- extract(input$y3)
                x3 <- extract(input$x3)
                #fit da camada 1 para camada 3
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f13 <- extract(input$f13)
                p13 <- round(predict(fit1, newdata = data.frame(x1 = f13)),2)
                pcam13 <- p13
                #fit da camada 2 para camada 3
                p2 <- as.numeric(extract(input$y2))
                x2 <- as.numeric(extract(input$x2))
                #dat <- as.data.frame(x,y)
                y1 <- extract(input$y1)
                x1 <- extract(input$x1)
                fit1 <- lm(y1 ~ x1)
                f12 <- extract(input$f12)
                p12 <- round(predict(fit1, newdata = data.frame(x1 = f12)),2)
                y2 <- as.numeric(p2) - as.numeric(p12)
                fit2 <- lm(y2 ~ x2)
                f23 <- extract(input$f23)
                p23 <- round(predict(fit2, newdata = data.frame(x2 = f13)),2)
                pcam23 <- p23
                pcam33 <- p3 - (pcam13 + pcam23)
                fit3 <- lm(pcam33 ~ x3)
                summary(fit3)
        })
        
        output$downloadReport <- downloadHandler(
                filename = function() {
                        paste("my-report", sep = ".", switch(
                                input$format, PDF = "pdf", HTML = "html", Word = "docx"
                        ))
                },
                
                content = function(file) {
                        src <- normalizePath("report.Rmd")
                        
                        # temporarily switch to the temp dir, in case you do not have write
                        # permission to the current working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, "report.Rmd", overwrite = TRUE)
                        
                        library(rmarkdown)
                        out <- render("report.Rmd", switch(
                                input$format,
                                PDF = pdf_document(), HTML = html_document(), Word = word_document()
                        ))
                        file.rename(out, file)
                }
        )      
}


shinyApp(ui, server)