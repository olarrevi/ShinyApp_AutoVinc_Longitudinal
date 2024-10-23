library(renv)
renv::restore()

library(shiny)
library(DT)
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(haven)
library(bslib)
library(tidyxl)
library(gridlayout)
library(purrr)
library(fuzzyjoin)
library(ggplot2)

options(shiny.maxRequestSize = 100 * 1024^2)

# Función para convertir .sav a dataframe con etiquetas como columnas
sav_to_labels <- function(db) {
  map_df(names(db), ~tibble(
    variable = .x,
    label_o = as.character(attr(db[[.x]], "label"))
  ))
}

# Función para normalizar etiquetas
normalize_labels <- function(var_fun) {
  var_fun %>%
    tolower() %>%
    gsub("[^[:alpha:]]", "", .) %>%
    stringi::stri_trans_general("Latin-ASCII")
}

# Función para actualizar nombres de columnas
actualizar_nombres <- function(df, label) {
  colnames(df) <- c("variable", "label(OG)", label)
  df
}

# Función para seleccionar y excluir NAs
seleccionar_y_excluir_nas <- function(excel_codi, label, col_inici, codi_columna) {
  excel_codi %>%
    select(!!sym(col_inici), !!sym(label), !!sym(codi_columna)) %>%
    na.exclude()
}

# Función para realizar join y filtrar
realizar_join_y_filtrar <- function(df, df_codi, label, dist_max) {
  df_unir <- stringdist_left_join(df, df_codi,
                                  by = label,
                                  method = "lv",
                                  max_dist = dist_max,
                                  distance_col = "distancia")
  
  list(
    df_filtrat_no_na = df_unir %>% filter(!if_any(ends_with(".y"), is.na)) %>% select(-ends_with(".y")),
    df_unir_na = df_unir %>% filter(if_any(ends_with(".y"), is.na)) %>% select(1:3)
  )
}

realizar_coincidencias <- function(df_sin_na, col_total, label) {
  data.frame(
    coinc = nrow(df_sin_na),
    percent = nrow(df_sin_na) / as.integer(col_total),
    any = label
  )
}

# UI de la nueva aplicación
ui <- page_navbar(
  title = "Vinculació Automàtica",
  selected = "Informació general",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "Configuración",
    fileInput("file1", "Cargar archivo de vinculación (.xlsx)"),
    uiOutput(outputId = "select_sheet_vinc"),
    fileInput("file2", "Cargar archivo de diccionario (.xlsx)"),
    uiOutput(outputId = "select_col"),
    fileInput("file3", "Cargar archivo .sav"),
    numericInput(
      min = 1,
      max = 10,
      inputId = "dist_max",
      label = "Distancia máxima",
      value = 3
    ),
    textInput(
      inputId = "colfinal",
      label = "Nombre de la columna final"
    ),
    textInput(
      inputId = "codi_columna",
      label = "Nombre de la columna del código",
      value = "codi"
    ),
    checkboxInput(
      value = TRUE,
      inputId = "reverse_labels",
      label = "Usar etiquetas en orden inverso"
    ),
    actionButton(inputId = "process", label = "Procesar"),
    downloadButton("downloadData", "Descargar DataFrames")
  ),
  nav_panel(
    title = "Informació general",
    grid_container(
      layout = c(
        "gridtaulaanys gridtaulaanys",
        "gridtop5dupli gridplotvinc "
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "0.98fr",
        "1.02fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "gridplotvinc",
        card_body(plotOutput(outputId = "plotvinc"))
      ),
      grid_card(
        area = "gridtaulaanys",
        card_body(
          DTOutput(outputId = "labelxanys", width = "100%")
        )
      ),
      grid_card(
        area = "gridtop5dupli",
        card_body(
          DTOutput(outputId = "top5_duplicados", width = "100%")
        )
      )
    )
  ),
  nav_panel(
    title = "Variables Vinculades",
    DTOutput(outputId = "df_final", width = "100%")
  ),
  nav_panel(
    title = "Duplicats",
    grid_container(
      layout = c(
        "gridtauladupli   gridtauladupli  ",
        "gridplotduplicat gridplotduplicat"
      ),
      row_sizes = c(
        "0.5fr",
        "1.5fr"
      ),
      col_sizes = c(
        "1.73fr",
        "0.27fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "gridtauladupli",
        card_body(
          DTOutput(outputId = "df_duplicados", width = "100%")
        )
      ),
      grid_card(
        area = "gridplotduplicat",
        card_body(plotOutput(outputId = "duplicados_plot"))
      )
    )
  ),
  nav_panel(
    title = "Variables no Vinculades",
    DTOutput(outputId = "df_no_vinculados", width = "100%")
  ),
  nav_panel(
    title = "Taula Final",
    DTOutput(outputId = "taula_final", width = "100%")
  )
)

# Server de la nueva aplicación
server <- function(input, output, session) {
  taula_final <- reactiveVal(NULL)
  df_final_filtrat_sin_duplicados <- reactiveVal(NULL)
  df_unir_na <- reactiveVal(NULL)
  duplicados <- reactiveVal(NULL)
  
  excel_codi <- reactive({
    req(input$file2)
    data <- read_xlsx(input$file2$datapath)
    data.frame(data)
  })
  
  output$select_col <- renderUI({
    req(excel_codi())
    selectInput("columns", "Seleccionar Anys",
                choices = colnames(excel_codi()),
                selected = colnames(excel_codi())[-c(1:2)],
                multiple = TRUE)
  })
  
  # Agregar selector de hoja para excel_vinc
  observeEvent(input$file1, {
    req(input$file1)
    sheets <- excel_sheets(input$file1$datapath)
    updateSelectInput(session, "sheet_vinc", choices = sheets, selected = sheets[1])
  })
  
  output$select_sheet_vinc <- renderUI({
    req(input$file1)
    sheets <- excel_sheets(input$file1$datapath)
    selectInput("sheet_vinc", "Seleccionar Hoja de Trabajo", choices = sheets, selected = sheets[1])
  })
  
  observeEvent(input$process, {
    req(input$file1, input$file2, input$file3)
    excel_codi <- read_xlsx(input$file2$datapath)
    excel_vinc <- read_xlsx(input$file1$datapath, sheet = input$sheet_vinc)
    col_etiqueta <- names(excel_vinc)[1]
    codi_columna <- input$codi_columna
    
    if(names(excel_codi)[1] != names(excel_vinc)[1]){ #Si el nom de la etiqueta noms no coincideix, força a que coincideixi
      print(paste0("No coincidencia: DF_diccionari: ",
                   names(excel_codi)[1],
                   ", DF_vinculacio: ",
                   names(excel_vinc)[1]))
      names(excel_codi)[1] <- names(excel_vinc)[1]
    }
    
    df_nova <- sav_to_labels(read_sav(input$file3$datapath)) %>%
      mutate(label_a = normalize_labels(label_o))
    
    dist_max <- input$dist_max
    nom_columna_f <- "label21"
    labels <- if (input$reverse_labels) rev(input$columns) else input$columns
    
    df_final <- data.frame()
    df_coincidencias <- data.frame()
    df_unir_na_local <- NULL
    
    for (label in labels) {
      colnames_to_use <- if (is.null(df_unir_na_local)) df_nova else df_unir_na_local
      colnames_to_use <- actualizar_nombres(colnames_to_use, label)
      
      print(paste0("Actualitzar_noms OK "))
      
      df_codi <- seleccionar_y_excluir_nas(excel_codi, label, col_etiqueta, codi_columna)
      
      print(paste0("Seleccionar y excluir NAs OK: ", names(df_codi)))
      
      resultados <- realizar_join_y_filtrar(colnames_to_use, df_codi, label, dist_max)
      
      print(paste0("realitzar joins OK: ", names(resultados)))
      
      df_filtrat_no_na <- resultados$df_filtrat_no_na
      df_unir_na_local <- resultados$df_unir_na
      df_parc_coincidencias <- realizar_coincidencias(df_filtrat_no_na, nrow(df_nova), label)
      df_coincidencias <- bind_rows(df_coincidencias, df_parc_coincidencias)
      df_final <- bind_rows(df_final, df_filtrat_no_na)
    }
    
    df_final_filtrat <- df_final  %>%
      select(-matches("\\.x$")) %>%
      group_by(!!sym(codi_columna)) %>%
      filter(distancia == min(distancia)) %>%
      ungroup()
    
    duplicados_count <- df_final_filtrat %>%
      group_by(variable) %>%
      summarise(count = n()) %>%
      filter(count > 1) %>%
      mutate(color = ifelse(count < 5, FALSE, TRUE)) %>%
      ungroup() %>%
      arrange(desc(count))
    
    duplicados_local <- df_final_filtrat %>%
      group_by(variable) %>%
      filter(n() > 1 & row_number() == 1) %>%
      ungroup() %>%
      distinct()
    
    df_final_filtrat_sin_duplicados_local <- anti_join(df_final_filtrat, duplicados_local, by = "variable")
    df_graph <- data.frame(
      tipus = c("Vinculat", "No vinculat"),
      valor = c(nrow(df_final_filtrat_sin_duplicados_local) / nrow(df_nova) * 100,
                (nrow(df_nova) - nrow(df_final_filtrat_sin_duplicados_local)) / nrow(df_nova) * 100)
    )
    colfinal <- input$colfinal
    taula_final_local <- left_join(
      excel_vinc,
      df_final_filtrat_sin_duplicados_local %>%
        select(variable, `label(OG)`, !!sym(codi_columna)) %>%
        rename(!!sym(colfinal) := variable),
      by = codi_columna
    ) %>%
      select(-ends_with(".y")) %>%
      rename_with(~ gsub("\\.x$", "", .), matches("\\.x$")) %>% 
      select(1:which(names(.) == colfinal))
    
    taula_final(taula_final_local)
    df_final_filtrat_sin_duplicados(df_final_filtrat_sin_duplicados_local)
    df_unir_na(df_unir_na_local)
    duplicados(duplicados_local)
    
    output$labelxanys <- renderDataTable({ df_coincidencias })
    output$top5_duplicados <- renderDataTable({ duplicados_count %>% head(5) })
    output$df_final <- renderDataTable({ df_final_filtrat_sin_duplicados_local })
    output$df_no_vinculados <- renderDataTable({ df_unir_na_local })
    output$taula_final <- renderDataTable({ taula_final_local })
    output$df_duplicados <- renderDataTable({ duplicados_local })
    
    output$plotvinc <- renderPlot({
      ggplot(df_graph, aes(x = "", y = valor, fill = tipus)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(round(valor, 1), "%")), position = position_stack(vjust = 0.5)) +
        theme_void()
    })
    
    output$duplicados_plot <- renderPlot({
      ggplot(duplicados_count, aes(x = reorder(variable, -count), y = count, fill = color)) +
        geom_bar(stat = "identity", width = 0.8) +
        scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "lightgrey")) +
        labs(title = "Número de Duplicados por Variable", x = "Variable", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("dataframes-", Sys.Date(), ".xlsx", sep = "") },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Vinculacio Final")
      writeData(wb, "Vinculacio Final", taula_final())
      addWorksheet(wb, "Vinculado")
      writeData(wb, "Vinculado", df_final_filtrat_sin_duplicados())
      addWorksheet(wb, "No Vinculados")
      writeData(wb, "No Vinculados", df_unir_na())
      addWorksheet(wb, "Duplicados")
      writeData(wb, "Duplicados", duplicados())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
