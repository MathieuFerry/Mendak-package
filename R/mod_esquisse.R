#' @importFrom bslib bs_theme
#' @importFrom dplyr across all_of arrange bind_rows count filter left_join n rename select summarise sym top_n ungroup where
#' @importFrom DT datatable DTOutput formatRound JS renderDT
#' @importFrom esquisse esquisse_server esquisse_ui
#' @importFrom factoextra fviz_ca_col fviz_eig fviz_pca_var
#' @importFrom FactoMineR CA PCA
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 aes coord_flip element_text geom_bar geom_boxplot geom_histogram geom_hline geom_point geom_smooth geom_text geom_vline ggplot guide_legend guides labs position_fill position_stack scale_color_manual scale_fill_brewer scale_size_continuous scale_y_continuous theme theme_minimal theme_void
#' @importFrom ggpubr ggscatter group_by mutate
#' @importFrom ggrepel geom_text_repel
#' @importFrom lubridate dmy my ymd
#' @importFrom openxlsx read.xlsx
#' @importFrom paletteer paletteer_d
#' @importFrom quanteda as.tokens convert corpus dfm dfm_group dfm_subset dfm_trim docnames docvars fcm fcm_select kwic print rowSums stopwords t tokens tokens_remove tokens_select tokens_tolower topfeatures types
#' @importFrom quanteda.textplots textplot_network textplot_wordcloud
#' @importFrom quanteda.textstats as.matrix textstat_frequency textstat_keyness textstat_summary
#' @importFrom rainette clusters_by_doc_table cutree cutree_rainette cutree_rainette2 rainette rainette_plot rainette_stats rainette2 rainette2_plot split_segments
#' @importFrom RColorBrewer brewer.pal
#' @importFrom readr read_file
#' @importFrom readxl read_excel
#' @importFrom shiny actionButton br checkboxGroupInput checkboxInput column conditionalPanel div downloadButton downloadHandler eventReactive fileInput fluidPage fluidRow h3 HTML incProgress mainPanel moduleServer navbarMenu navbarPage NS numericInput observe observeEvent p plotOutput radioButtons reactive reactiveVal reactiveValues renderPlot renderText renderUI req selectInput shinyApp showNotification sidebarLayout sidebarPanel sliderInput span tabPanel tabsetPanel tagList textInput uiOutput updateSelectInput verbatimTextOutput withProgress
#' @importFrom sortable sortable_js
#' @importFrom stats cor quantile reorder
#' @importFrom stringr str_count
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom tidyr pivot_wider
#' @importFrom tools file_ext
#' @importFrom udpipe udpipe udpipe_download_model
#' @importFrom utils head read.csv read.csv2 str tail write.csv
#' @importFrom writexl write_xlsx
NULL


mod_esquisse_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("launch"), "Lancer Esquisse"),
    conditionalPanel(
      condition = sprintf("input['%s'] > 0", ns("launch")),
      tagList(
        conditionalPanel(
          condition = sprintf("!input['%s'] || input['%s'].length == 0", ns("convert_to_date"), ns("convert_to_date")),
          selectInput(
            ns("convert_to_date"),
            label = "Variables à convertir en Date (facultatif)",
            choices = NULL,  # <-- THIS WAS MISSING
            multiple = TRUE
          )
        ),
        esquisse_ui(ns("esquisse"), header = FALSE)
      )
    )
  )
}

mod_esquisse_server <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_r <- reactiveValues(data = data.frame(), name = "placeholder")

    observeEvent(input$launch, {
      req(data_in())
      data_r$data <- data_in()
      data_r$name <- "Données"

      # After launch, list factor variables as candidates
      updateSelectInput(session, "convert_to_date",
                        choices = names(data_in())[sapply(data_in(), is.factor)])
    })

    observeEvent(input$convert_to_date, {
      req(input$convert_to_date)

      for (var in input$convert_to_date) {
        vec <- as.character(data_r$data[[var]])

        # Check if the strings contain a 4-digit number (likely a year)
        has_year <- grepl("\\b\\d{4}\\b", vec)

        if (!any(has_year)) {
          # No year detected at all: skip conversion
          showNotification(
            paste("Conversion ignorée pour la variable", var, ": absence d'année (ex : 2025) détectée."),
            type = "warning",
            duration = 7
          )
          next  # move to next variable
        }

        success <- FALSE

        tmp <- suppressWarnings(lubridate::dmy(vec, locale = "fr_FR.UTF-8"))
        if (all(!is.na(tmp))) {
          data_r$data[[var]] <- tmp
          success <- TRUE
        } else {
          tmp_en <- suppressWarnings(lubridate::dmy(vec))
          if (all(!is.na(tmp_en))) {
            data_r$data[[var]] <- tmp_en
            success <- TRUE
          } else {
            tmp2 <- suppressWarnings(lubridate::ymd(vec, truncated = 2))
            if (all(!is.na(tmp2))) {
              data_r$data[[var]] <- tmp2
              success <- TRUE
            } else {
              tmp3 <- suppressWarnings(lubridate::my(vec, locale = "fr_FR.UTF-8"))
              if (all(!is.na(tmp3))) {
                data_r$data[[var]] <- as.Date(tmp3)
                success <- TRUE
              } else {
                tmp3_en <- suppressWarnings(lubridate::my(vec))
                if (all(!is.na(tmp3_en))) {
                  data_r$data[[var]] <- as.Date(tmp3_en)
                  success <- TRUE
                }
              }
            }
          }
        }

        if (!success) {
          showNotification(
            paste("Impossible de convertir la variable", var, "en Date."),
            type = "warning",
            duration = 5
          )
        }
      }
    })

    esquisse_server(
      id = "esquisse",
      data_rv = data_r
    )
  })
}
