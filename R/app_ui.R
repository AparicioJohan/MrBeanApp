#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    bs4DashPage(
      title = "MrBean",
      skin = NULL,
      freshTheme = NULL,
      preloader = NULL,
      options = NULL,
      fullscreen = TRUE,
      help = FALSE,
      dark = NULL,
      scrollToTop = FALSE,
      header = bs4DashNavbar(
        title = dashboardBrand(
          title = "MrBean",
          color = "white",
          href = "https://apariciojohan.github.io/MrBeanApp/",
          image = "www/beans3.png",
          opacity = 0.8
        ),
        status = "white",
        fixed = TRUE,
        # HTML("<script type='text/javascript' src='https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js' data-name='bmc-button' data-slug='mrbean' data-color='#FFFFFF' data-emoji=''  data-font='Cookie' data-text='Buy MrBean a coffee' data-outline-color='#000' data-font-color='#000' data-coffee-color='#fd0' ></script>"),
        "Web Application for Data Analysis!",
        rightUi = bs4DropdownMenu(
          type = "messages",
          badgeStatus = "danger",
          href = "http://buymeacoffee.com/mrbean",
          messageItem(
            from = "MrBean",
            message = "If you want to contribute...",
            time = "today", image = "www/beans3.png",
            href = "http://buymeacoffee.com/mrbean"
          )
        )
      ),
      sidebar = bs4DashSidebar(
        skin = "light",
        status = "success",
        elevation = 3,
        fixed = TRUE,
        bs4SidebarMenu(
          id = "tabs",
          bs4SidebarHeader("Menu"),
          bs4SidebarMenuItem(
            "Home",
            tabName = "home", icon = shiny::icon("home", verify_fa = FALSE)
          ),
          # Import data
          bs4SidebarMenuItem(
            "Data",
            icon = shiny::icon("database"),
            startExpanded = F,
            bs4SidebarMenuItem(
              text = "Upload",
              tabName = "Data",
              icon = shiny::icon("file-upload", verify_fa = FALSE)
            ),
            bs4SidebarMenuItem(
              text = "Descriptives",
              tabName = "descriptives",
              icon = shiny::icon("chart-line")
            ),
            bs4SidebarMenuItem(
              text = "Distribution",
              tabName = "distrib",
              icon = shiny::icon("chart-area")
            )
          ),
          bs4SidebarHeader("SpATS"),
          # Single spatial analysis SpATS
          bs4SidebarMenuItem(
            text = "Single-Site",
            icon = shiny::icon("braille"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Model Specs",
              tabName = "modelo",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "BLUPs/BLUEs",
              tabName = "blupspat",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Residuals",
              tabName = "resispat",
              icon = shiny::icon("circle", verify_fa = FALSE)
            )
          ),
          # Multiple-single analysis
          bs4SidebarMenuItem(
            "Site-by-Site",
            icon = shiny::icon("sitemap"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Model Specs",
              tabName = "msa",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Results",
              tabName = "msa_result",
              icon = shiny::icon("circle", verify_fa = FALSE)
            )
          ),
          # Multiple trait
          bs4SidebarMenuItem(
            "Trait-by-Trait",
            icon = shiny::icon("ruler"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Model Specs",
              tabName = "multi_trait",
              icon = shiny::icon("circle", verify_fa = FALSE)
            )
          ),
          bs4SidebarHeader("ASReml"),
          # Single spatial analysis ASReml
          bs4SidebarMenuItem(
            text = "Single-Site",
            icon = shiny::icon("braille"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Model Specs",
              tabName = "spats_asreml",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "BLUPs/BLUEs",
              tabName = "spats_asreml_effects",
              icon = shiny::icon("circle", verify_fa = FALSE)
            )
          ),
          # Un-replicated analysis
          bs4SidebarMenuItem(
            "Unreplicated",
            icon = shiny::icon("crosshairs"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Model Specs",
              tabName = "aug_model",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "BLUPs/BLUEs",
              tabName = "aug_result",
              icon = shiny::icon("circle", verify_fa = FALSE)
            )
          ),
          # Model selector
          bs4SidebarMenuItem(
            text = "Model Selector",
            icon = shiny::icon("hand-pointer"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              HTML(
                paste(
                  "Model Specs",
                  bs4Badge("new",
                    position = "right",
                    color = "success"
                  )
                )
              ),
              tabName = "asreml_selector",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              HTML(
                paste(
                  "BLUPs/BLUEs",
                  bs4Badge("new",
                    position = "right",
                    color = "success"
                  )
                )
              ),
              tabName = "asr_sel_selected",
              icon = shiny::icon("circle", verify_fa = FALSE)
            )
          ),
          bs4SidebarHeader("Two-Stage Analysis"),
          # Two-Stage MET
          bs4SidebarMenuItem(
            "MET Analysis",
            icon = shiny::icon("chart-pie"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              HTML(
                paste(
                  "Model Specs",
                  bs4Badge("new",
                    position = "right",
                    color = "info"
                  )
                )
              ),
              tabName = "met",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              HTML(
                paste(
                  "Results",
                  bs4Badge("new",
                    position = "right",
                    color = "info"
                  )
                )
              ),
              tabName = "met_result",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              HTML(
                paste(
                  "Factor Analytic",
                  bs4Badge("new",
                    position = "right",
                    color = "info"
                  )
                )
              ),
              tabName = "met_fa",
              icon = shiny::icon("circle", verify_fa = FALSE)
            )
          ),
          # GBLUP
          bs4SidebarHeader("GBLUP"),
          bs4SidebarMenuItem(
            text = "Genomic Prediction",
            icon = shiny::icon("dna"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = HTML(
                paste(
                  "Model Specs",
                  bs4Badge("new",
                           position = "right",
                           color = "danger"
                  )
                )
              ),
              tabName = "gblup",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              HTML(
                paste(
                  "Results",
                  bs4Badge("new",
                           position = "right",
                           color = "danger"
                  )
                )
              ),
              tabName = "gblup_results",
              icon = shiny::icon("circle", verify_fa = FALSE)
            )
          ),
          bs4SidebarHeader("Traditional Designs"),
          # lme4 basic models
          bs4SidebarMenuItem(
            text = "Analysis with lme4",
            icon = shiny::icon("chart-bar"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Model Specs",
              tabName = "mixed",
              icon = shiny::icon("circle", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "BLUPs/BLUEs",
              tabName = "boxes",
              icon = shiny::icon("circle", verify_fa = FALSE)
            )
          ),
          bs4SidebarHeader("About"),
          bs4SidebarMenuItem(
            text = "info",
            tabName = "valueboxes",
            icon = shiny::icon("leaf")
          )
        )
      ),
      body = bs4DashBody(
        bs4TabItems(
          # chooseSliderSkin("Modern"),
          bs4TabItem(
            tabName = "home",
            mod_home_module1_ui("home_module1_ui_1")
          ),
          # Import data
          bs4TabItem(
            tabName = "Data",
            mod_import_dt_ui("import_dt_ui_1")
          ),
          bs4TabItem(
            tabName = "descriptives",
            HTML('<h1 style="font-weight: bold; color: #00a65a;">Descriptive Plots</h1>'),
            fluidRow(
              column(
                width = 6,
                mod_descrip_scatter_ui("descrip_scatter_1")
              ),
              column(
                width = 6,
                mod_descrip_boxplot_ui("descrip_boxplot_1")
              )
            )
          ),
          bs4TabItem(
            tabName = "distrib",
            mod_distribution_ui("distribution_ui_1")
          ),
          # Single spatial analysis SpATS
          bs4TabItem(
            tabName = "modelo",
            mod_spats_single_ui("spats_single_ui_1"),
            mod_info_spats_ui("info_spats_ui_1")
          ),
          bs4TabItem(
            tabName = "blupspat",
            mod_effects_spats_ui("effects_spats_ui_1")
          ),
          bs4TabItem(
            tabName = "resispat",
            mod_residuals_spats_ui("residuals_spats_ui_1"),
            HTML("<script data-name='BMC-Widget' src='https://cdnjs.buymeacoffee.com/1.0.0/widget.prod.min.js' data-id='mrbean' data-description='Support me on Buy me a coffee!' data-message='Thank you for visiting.' data-color='#28a745' data-position='right' data-x_margin='18' data-y_margin='18'></script>")
          ),
          # Multiple-single analysis
          bs4TabItem(
            tabName = "msa",
            mod_MSA_ui("MSA_ui_1")
          ),
          bs4TabItem(
            tabName = "msa_result",
            mod_MSA_results_ui("MSA_results_ui_1")
          ),
          bs4TabItem(
            tabName = "multi_trait",
            mod_M_traits_ui("M_traits_ui_1")
          ),
          # Single spatial analysis ASReml
          bs4TabItem(
            tabName = "spats_asreml",
            mod_spats_asreml_ui("spats_asreml_ui_1")
          ),
          bs4TabItem(
            tabName = "spats_asreml_effects",
            mod_spats_asreml_effects_ui("spats_asreml_effects_ui_1")
          ),
          # Un-replicated analysis
          bs4TabItem(
            tabName = "aug_model",
            mod_aug_model_ui("aug_model_ui_1")
          ),
          bs4TabItem(
            tabName = "aug_result",
            mod_aug_result_ui("aug_result_ui_1")
          ),
          # Model selector
          bs4TabItem(
            tabName = "asreml_selector",
            mod_asreml_selector_ui("asreml_selector_ui_1")
          ),
          bs4TabItem(
            tabName = "asr_sel_selected",
            mod_asreml_selector_effects_ui("asreml_selector_effects_ui_1")
          ),
          # Two-Stage MET
          bs4TabItem(
            tabName = "met",
            mod_MET_ui("MET_ui_1")
          ),
          bs4TabItem(
            tabName = "met_result",
            mod_MET_results_ui("MET_results_ui_1")
          ),
          bs4TabItem(
            tabName = "met_fa",
            mod_MET_FA_ui("MET_FA_ui_1")
          ),
          # GBLUP
          bs4TabItem(
            tabName = "gblup",
            mod_GBLUP_ui("GBLUP_1")
          ),
          bs4TabItem(
            tabName = "gblup_results",
            mod_GBLUP_results_ui("GBLUP_results_1")
          ),
          # lme4 basic models
          bs4TabItem(
            tabName = "mixed",
            mod_lme4_single_ui("lme4_single_ui_1")
          ),
          bs4TabItem(
            tabName = "boxes",
            mod_residuals_lme4_ui("residuals_lme4_ui_1")
          ),
          # About
          bs4TabItem(
            tabName = "valueboxes",
            mod_about_ui("about_ui_1")
          )
        )
      ),
      controlbar = bs4DashControlbar(
        skin = "light",
        # pinned = TRUE,
        br(),
        col_4(),
        col_4(
          h5("Go to:"),
          actionLink(
            inputId = "toAwesome00",
            label = "Home",
            icon = icon("home", verify_fa = FALSE)
          ),
          br(),
          actionLink(
            inputId = "toAwesome11",
            label = "Data",
            icon = icon("database")
          ),
          br(),
          actionLink(
            inputId = "toAwesome22",
            label = "Spatial",
            icon = icon("braille")
          ),
          br(),
          actionLink(
            inputId = "toAwesome33",
            label = "About",
            icon = icon("chart-column", verify_fa = FALSE)
          ),
          br()
        ),
        col_4()
      ),
      footer = bs4DashFooter(
        fixed = TRUE,
        left = tagList(
          "v.2.0.9",
          HTML("&nbsp; &nbsp; &nbsp; &nbsp;"),
          "Alliance Bioversity & CIAT 2019 - 2024"
        ),
        right = NULL
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MrBean"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    # shinyalert::useShinyalert(),
    rintrojs::introjsUI(),
    shinytoastr::useToastr(),
    waiter::use_waiter(),
    sever::useSever()
  )
}
