ds.Boole.o<-function(V1=NULL, V2=NULL, Boolean.operator=NULL, numeric.output=TRUE, na.assign="NA",newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # check if user has provided the name of the column or scalar that holds V1
  if(is.null(V1)){
    stop("Please provide the name of the column or scalar that holds V1!", call.=FALSE)
  }

  # check if user has provided the name of a column or scalar holding V2 or has declared a scalar value: eg '3'
  if(is.null(V2)){
    stop("Please provide the name of a column or scalar holding V2 or declare a scalar in character format: eg '3'", call.=FALSE)
  }

  # check if user has provided a Boolean operator in character format: eg '==' or '>=' or '<' or '!='
  if(is.null(Boolean.operator)){
    stop("Please provide a Boolean operator in character format: eg '==' or '>=' or '<' or '!='", call.=FALSE)
  }
  
  #check if na.assign has legal value
  if(!(na.assign=="NA"||na.assign=="0"||na.assign=="1")){
    stop("Error: na.assign must be a character string taking value 'NA', '0' or '1'- if <na.action> not specified default is 'NA'", call.=FALSE)
  }
  
  

#convert Boolean operator to numeric

BO.n<-0
if(Boolean.operator == "=="){
   BO.n<-1
}

if(Boolean.operator == "!="){
   BO.n<-2
}

if(Boolean.operator == "<"){
   BO.n<-3
}

if(Boolean.operator == "<="){
   BO.n<-4
}

if(Boolean.operator == ">"){
   BO.n<-5
}

if(Boolean.operator == ">="){
   BO.n<-6
}

  # if no value spcified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- paste0(V1,"_Boole")
  }

# CALL THE MAIN SERVER SIDE FUNCTION
  calltext <- call("BooleDS.o", V1, V2, BO.n, na.assign,numeric.output)
  datashield.assign(datasources, newobj, calltext)
  
##########################################################################################################
#MODULE 5: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                                   #
                                                                                                         #
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 #
test.obj.name<-newobj

#TRACER
#return(test.obj.name)
#}                                                                                   #
 
 #
# CALL SEVERSIDE FUNCTION                                                                                #
calltext <- call("testObjExistsDS.o", test.obj.name)													 #
																										 #
object.info<-datashield.aggregate(datasources, calltext)												 #
																										 #
# CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 #
# AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 #
num.datasources<-length(object.info)																	 #
																										 #
																										 #
obj.name.exists.in.all.sources<-TRUE																	 #
obj.non.null.in.all.sources<-TRUE																		 #
																										 #
for(j in 1:num.datasources){																			 #
	if(!object.info[[j]]$test.obj.exists){																 #
		obj.name.exists.in.all.sources<-FALSE															 #
		}																								 #
	if(object.info[[j]]$test.obj.class=="ABSENT"){														 #
		obj.non.null.in.all.sources<-FALSE																 #
		}																								 #
	}																									 #
																										 #
if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 #
																										 #
	return.message<-																					 #
    paste0("Data object <", test.obj.name, "> created in all specified data sources")		 	 		 #
																										 #
	return(list(return.message=return.message))															 #
																										 #
	}else{																								 #
																										 #
    return.message.1<-																					 #
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")#
																										 #
	return.message.2<-																					 #
	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 #
																										 #
	return.message.3<-																					 #
	paste0("Please use ds.ls() to identify where missing")												 #
																										 #
																										 #
	return.message<-list(return.message.1,return.message.2,return.message.3)							 #
																										 #
	return.info<-object.info																			 #
																										 #
	return(list(return.info=return.info,return.message=return.message))									 #
																										 #
	}																									 #
#END OF MODULE 5																						 #
##########################################################################################################

}
#ds.Boole.o

