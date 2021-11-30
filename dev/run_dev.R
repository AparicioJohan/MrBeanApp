# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload(pkg = getwd())

# Run the application
run_app()


# 
# ui<-mod_import_dt_ui( "my_module" )
# server  <-function(input,output,session){
# callModule( module = mod_import_dt_server, id = "my_module" , session = session
# )}
# shinyApp(ui, server)

# ui<-mod_effects_spats_ui( "my_module" )
# server  <-function(input,output,session){
#   callModule(mod_effects_spats_server, "my_module")}
# shinyApp(ui, server)


