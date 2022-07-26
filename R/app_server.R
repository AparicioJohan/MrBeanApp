#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  sever::sever()

  # Home
  callModule(mod_home_module1_server, "home_module1_ui_1")

  observeEvent(input$toAwesome00, {
    updatebs4TabItems(session = session, inputId = "tabs", selected = "home")
  })

  observeEvent(input$toAwesome11, {
    updatebs4TabItems(session = session, inputId = "tabs", selected = "Data")
  })

  observeEvent(input$toAwesome22, {
    updatebs4TabItems(session = session, inputId = "tabs", selected = "modelo")
  })

  observeEvent(input$toAwesome33, {
    updatebs4TabItems(session = session, inputId = "tabs", selected = "valueboxes")
  })

  # Import Data
  data <- callModule(mod_import_dt_server, "import_dt_ui_1")
  data

  # Descriptives
  mod_descrip_scatter_server("descrip_scatter_1", data = data, plot = 1)
  mod_descrip_boxplot_server("descrip_boxplot_1", data = data, plot = 2)

  # Distribution
  callModule(mod_distribution_server, "distribution_ui_1", data = data)

  # SpATS
  effects <- callModule(mod_spats_single_server, "spats_single_ui_1", data = data)
  effects
  observeEvent(effects$Rlink(), updatebs4TabItems(session, inputId = "tabs", selected = "resispat"))
  callModule(mod_info_spats_server, "info_spats_ui_1", Model = effects)
  callModule(mod_effects_spats_server, "effects_spats_ui_1", Model = effects)
  callModule(mod_residuals_spats_server, "residuals_spats_ui_1", Model = effects)

  # Lme4
  lme4_model <- callModule(mod_lme4_single_server, "lme4_single_ui_1", data = data)
  lme4_model
  observeEvent(lme4_model$Rlink(), updatebs4TabItems(session, inputId = "tabs", selected = "boxes"))
  callModule(mod_residuals_lme4_server, "residuals_lme4_ui_1", model = lme4_model)

  # MSA
  msa <- callModule(mod_MSA_server, "MSA_ui_1", data = data)
  msa
  observeEvent(msa$check_mod(), updatebs4TabItems(session, inputId = "tabs", selected = "msa_result"))

  callModule(mod_MSA_results_server, "MSA_results_ui_1", msa = msa)

  # MSA trait
  callModule(mod_M_traits_server, "M_traits_ui_1", data = data)

  # Augmented
  aug <- callModule(mod_aug_model_server, "aug_model_ui_1", data = data)
  aug
  callModule(mod_aug_result_server, "aug_result_ui_1", model = aug)

  # ASReml
  ASRml <- callModule(mod_spats_asreml_server, "spats_asreml_ui_1", data = data)
  ASRml
  callModule(mod_spats_asreml_effects_server, "spats_asreml_effects_ui_1", model = ASRml)

  # ASReml Selector
  selector <- callModule(mod_asreml_selector_server, "asreml_selector_ui_1", data = data)
  selector
  callModule(mod_asreml_selector_effects_server, "asreml_selector_effects_ui_1", model = selector)

  # MET
  MET <- callModule(mod_MET_server, "MET_ui_1")
  MET
  observeEvent(MET$check_mod(), updatebs4TabItems(session, inputId = "tabs", selected = "met_result"))

  MET_res <- callModule(mod_MET_results_server, "MET_results_ui_1", model = MET)
  MET_res
  observeEvent(MET_res(), updatebs4TabItems(session, inputId = "tabs", selected = "met_fa"))

  callModule(mod_MET_FA_server, "MET_FA_ui_1", model = MET)
  
  # GBLUP
  GBLUP_result <- mod_GBLUP_server("GBLUP_1")
  mod_GBLUP_results_server("GBLUP_results_1", gblup = GBLUP_result)

  # About
  callModule(mod_about_server, "about_ui_1")
}
