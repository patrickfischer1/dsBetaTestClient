ds.rm.o<-function(x.name=NULL, type='split', datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x.name)){
   stop("Please provide the name of the object to be deleted (eg 'object.name') as the x.name argument", call.=FALSE)
  }
  
	#make transmittable via parser
    x.name.transmit <- paste(x.name,collapse=",")
  
  
  # call the server side function
  #PLEASE NOTE THIS IS - SURPRISINGLY - AN AGGREGATE FUNCTION. ALTHOUGH IT IS MODIFYING AN OBJECT
  #ON THE SERVERSIDE, AND MIGHT THEREFORE BE EXPECTED TO BE AN ASSIGN FUNCTION,
  #AS AN ASSIGN FUNCTION IT WOULD HAVE TO COMPLETE BY WRITING
  #THE MODIFIED OBJECT AS newobj. BUT THIS WOULD BE IMPOSSIBLE BECAUSE THE
  #EFFECT OF THE FUNCTION IS TO DELETE THE OBJECT!!!
  
	calltext <- call("rmDS.o", x.name.transmit)

	output = datashield.aggregate(datasources, calltext)
  
  return(output)
}


