dataReact <- function(file, choice , header , sep, miss, string = "" , sheet   ,dataBMS, dataBRAPI  ){ 
  
  
  inFile <- file
  Ext <- tools::file_ext(inFile$datapath)
  
  if (choice==1) {
    
    dt <- Dar16C_hiP
    
  } else if(choice==2){
    
    if (is.null(inFile)) { 
      
      dt <- data.frame() 
    
    }  else { 
      
      if(Ext=="xlsx"|Ext=="xls") {
        
        if(miss=="Empty"){ 
          P = "\" \""
        } else {
            P = "NA"
            if(miss=="Other"){
              P = string
              }
            }
        
        # req(sheet!="")
        dt <- as.data.frame(readxl::read_excel(inFile$datapath, col_names = header, na = P,sheet = sheet ))
        
      }  else {
        
        dt <-  read.csv(inFile$datapath, header=header,sep=sep)
        
        if(miss=="Other"){
          P = string
          dt <- read.csv(inFile$datapath, header=header,sep=sep, na.strings = P) 
        }
        
      }
    }
    
  }  else if (choice==3){
    
    dt <- dataBMS
    
  } else if (choice==4){
    dt = dataBRAPI
  }
  
  return(dt)
  
  }


  

