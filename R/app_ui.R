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
      sidebar_collapsed = F,
      navbar = bs4DashNavbar(
        status = "white",
        fixed = F,
        HTML("<script type='text/javascript' src='https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js' data-name='bmc-button' data-slug='mrbean' data-color='#FFFFFF' data-emoji=''  data-font='Cookie' data-text='Buy MrBean a coffee' data-outline-color='#000' data-font-color='#000' data-coffee-color='#fd0' ></script>"),
        "Web Application for Spatial Analysis!",
        rightUi = bs4DropdownMenu(
          show = FALSE,
          labelText = "!",
          status = "danger",
          src = "http://buymeacoffee.com/mrbean",
          bs4DropdownMenuItem(
            message  = "If you want to contribute...",
            type = "notification"
          )
        )
      ),
      sidebar = bs4DashSidebar(
        skin = "light",
        status = "success",
        title = "Mr.Bean",
        brandColor = "white",
        url = "https://mrpackages.netlify.app/",
        src = "www/beans3.png",
        elevation = 3,
        opacity = 0.8, 
        fixed = F,
        bs4SidebarMenu(id = "tabs",
                       bs4SidebarHeader("Menu"),
                       bs4SidebarMenuItem(
                         "Home", tabName = "home", icon = "home"
                       ),
                       # Import data
                       bs4SidebarMenuItem(
                         "Data",icon = "database",startExpanded = F,
                         bs4SidebarMenuItem( 
                           text = "Upload", tabName = "Data", icon = "file-upload" 
                         ),
                         bs4SidebarMenuItem(
                           text = "Descriptives", tabName = "descriptives",icon = "chart-line" 
                         ),
                         bs4SidebarMenuItem(
                           text = "Distribution",
                           tabName = "distrib",
                           icon = "chart-area" 
                         )
                       ),
                       bs4SidebarHeader("SpATS"),
                       # Single spatial analysis SpATS
                       bs4SidebarMenuItem(
                         text = "Single-Site", icon = "braille", startExpanded = F,
                         bs4SidebarMenuSubItem(
                           text = "Model Specs", tabName = "modelo", icon = "circle-thin"
                         ),
                         bs4SidebarMenuSubItem(
                           text = "BLUPs/BLUEs", tabName = "blupspat", icon = "circle-thin"
                         ),
                         bs4SidebarMenuSubItem(
                           text = "Residuals", tabName = "resispat", icon = "circle-thin"
                         )
                       ),
                       # Multiple-single analysis
                       bs4SidebarMenuItem(
                         "Site-by-Site", icon = "sitemap", startExpanded = F,
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs", bs4Badge("new", position = "right", status = "danger"))) ,
                           tabName = "msa", icon = "circle-thin"
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("Results", bs4Badge("new", position = "right", status = "danger"))) ,
                           tabName = "msa_result", icon = "circle-thin"
                         )
                       ),
                       # Multiple trait 
                       bs4SidebarMenuItem(
                         "Trait-by-Trait", icon = "ruler", startExpanded = F,
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs", bs4Badge("new", position = "right", status = "danger"))) ,
                           tabName = "multi_trait", icon = "circle-thin"
                         )
                       ),
                       bs4SidebarHeader("ASReml"),
                       # Single spatial analysis ASReml
                       bs4SidebarMenuItem(
                         text = "Single-Site", icon = "braille", startExpanded = F,
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs", bs4Badge("new", position = "right", status = "success"))) ,
                           tabName = "spats_asreml", icon = "circle-thin"
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("BLUPs/BLUEs", bs4Badge("new", position = "right", status = "success"))) ,
                           tabName = "spats_asreml_effects", icon = "circle-thin"
                         )
                       ),
                       # Un-replicated analysis
                       bs4SidebarMenuItem(
                         "Unreplicated", icon = "crosshairs", startExpanded = F,
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs", bs4Badge("new", position = "right", status = "danger"))) ,
                           tabName = "aug_model", icon = "circle-thin"
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("BLUPs/BLUEs", bs4Badge("new", position = "right", status = "danger"))) ,
                           tabName = "aug_result", icon = "circle-thin"
                         )
                       ),
                       # Model selector
                       bs4SidebarMenuItem(
                         text = "Model Selector", icon = "hand-pointer", startExpanded = F,
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs", bs4Badge("new", position = "right", status = "success"))) ,
                           tabName = "asreml_selector", icon = "circle-thin"
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("BLUPs/BLUEs", bs4Badge("new", position = "right", status = "success"))) ,
                           tabName = "asr_sel_selected", icon = "circle-thin"
                         )
                       ),
                       bs4SidebarHeader("Two-Stage Analysis"),
                       # Two-Stage MET
                       bs4SidebarMenuItem(
                         "MET Analysis", icon = "chart-pie", startExpanded = F,
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs", bs4Badge("new", position = "right", status = "info"))) ,
                           tabName = "met", icon = "circle-thin"
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("Results", bs4Badge("new", position = "right", status = "info"))) ,
                           tabName = "met_result", icon = "circle-thin"
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("Factor Analytic", bs4Badge("new", position = "right", status = "info"))) ,
                           tabName = "met_fa", icon = "circle-thin"
                         )
                       ),
                       bs4SidebarHeader("Traditional Designs"),
                       # lme4 basic models
                       bs4SidebarMenuItem(
                         text = "Analysis with lme4", icon = "chart-bar", startExpanded = F,
                         bs4SidebarMenuSubItem(
                           text = "Model Specs", tabName = "mixed", icon = "circle-thin"
                         ),
                         bs4SidebarMenuSubItem(
                           text = "BLUPs/BLUEs", tabName = "boxes", icon = "circle-thin"
                         )
                       ),
                       bs4SidebarHeader("About"),
                       bs4SidebarMenuItem(
                         text = "info", tabName = "valueboxes", icon = "leaf"
                       )
        )
      ),
      body = bs4DashBody(
        chooseSliderSkin("Modern"), 
        bs4TabItems( 
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
              column(width=6,
                     mod_descrip_raw_ui("descrip_raw_ui_1", "Scatterplot", "fadeInLeftBig",  "fadeOutLeftBig", T)
                     ),
              column(width=6,
                     mod_descrip_raw_ui("descrip_raw_ui_2", "Boxplot", "fadeInRightBig",  "fadeOutRightBig", F )
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
            mod_spats_single_ui("spats_single_ui_1") ,
            mod_info_spats_ui("info_spats_ui_1")
          ),
          bs4TabItem(
            tabName = "blupspat",
            mod_effects_spats_ui("effects_spats_ui_1")
          ),
          bs4TabItem(
            tabName = "resispat",
            mod_residuals_spats_ui("residuals_spats_ui_1"),
            HTML( "<script data-name='BMC-Widget' src='https://cdnjs.buymeacoffee.com/1.0.0/widget.prod.min.js' data-id='mrbean' data-description='Support me on Buy me a coffee!' data-message='Thank you for visiting.' data-color='#28a745' data-position='right' data-x_margin='18' data-y_margin='18'></script>" )
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
        title = "Go to:",
        column(
          width = 12,
          align = "center",
          actionLink(inputId = "toAwesome00", label = "Home", icon = icon("home")),br(),
          actionLink(inputId = "toAwesome11", label = "Data", icon = icon("database")),br(),
          actionLink(inputId = "toAwesome22", label = "Spatial", icon = icon("braille")),br(),
          actionLink(inputId = "toAwesome33", label = "About", icon = icon("bar-chart-o")),br()            )
      ),
      footer = bs4DashFooter(
        fixed = F,
        copyrights = a(
          href = "https://www.linkedin.com/in/johan-steven-aparicio-arce-b68976193/", 
          target = "_blank", "J.aparicio@cgiar.org"
        ),
        right_text = "2020"
      ),
      title = "MrBean"
        
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'MrBean'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    rintrojs::introjsUI(),
    shinytoastr::useToastr(),
    waiter::use_waiter(),
    sever::use_sever()
  )
}

