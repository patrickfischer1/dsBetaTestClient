#' 
#' @title ds.reShape.o calling reShapeDS.o
#' @description This function is similar to R function \code{reshape}
#' @details This is an assign function that uses the native R function reshape() that reshapes a
#' data frame between 'wide' format with repeated measurements in separate columns of the same
#' record and 'long' format with the repeated measurements in separate records. 
#' @param data.name, the name of the data frame.
#' @param varying, names of sets of variables in the wide format that correspond to single
#' variables in long format ('time-varying').
#' @param v.names, the names of variables in the long format that correspond to multiple variables
#' in the wide format
#' @param timevar.name, the name of the variable in long format that differentiates multiple records from the same
#' group or individual. If more than one record matches, the first will be taken
#' @param idvar.name, Names of one or more variables in long format that identify multiple records from
#' the same group/individual. These variables may also be present in wide format.
#' @param drop,	a vector of names of variables to drop before reshaping.
#' @param direction, a character string, matched to either "wide" to reshape to wide 
#' format, or "long" to reshape to long format.
#' @param sep, a character vector of length 1, indicating a separating character in the variable
#' names in the wide format. This is used for guessing v.names and times arguments based on the
#' names in varying. This is also used to create variable names when reshaping to wide format.
#' @param newobj, the name of the new data frame. If this argument is not defined by the user,
#' the name of the new data frame is set to the name of the input data frame with the
#' suffix '_long' if \type{direction} is set to 'long' and with the suffix '_wide' if
#' \type{direction} is set to 'wide'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return the object specified by the newobj argument (or default name newObject) is written to the
#' serverside and a validity message indicating whether the newobject has been correctly
#' created at each source is returned to the client. If it has not been correctly created the return object
#' return.info details in which source the problem exists and whether: (a) the object exists at all; (b) it has meaningful
#' content indicated by a valid class.
#' @author Demetris Avraam, Paul Burton for DataSHIELD Development Team 
#' @export
#'
ds.reShape.o <- function(data.name=NULL, varying=NULL, v.names=NULL, timevar.name="time", idvar.name="id",
                            drop=NULL, direction=NULL, sep=".", newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(data.name)){
    stop("Please provide the name of the list that holds the input vectors!", call.=FALSE)
  }

 
  if (!is.character(sep) || length(sep) != 1L){ 
    stop("'sep' must be a character string", call.=FALSE)
  }
  
  if(is.null(direction)){
    stop("Please provide the direction of the conversion. The 'direction' argument must be either 'long' or 'wide'", call.=FALSE)
  }
  
  if (direction != 'long' & direction !='wide'){ 
    stop("'direction' must be either 'long' or 'wide'", call.=FALSE)
  }
  
  if(!is.null(varying)){
    varying.transmit <- paste(varying,collapse=",")
  }else{
    varying.transmit <- NULL
  }

  if(!is.null(v.names)){
    v.names.transmit <- paste(v.names,collapse=",")
  }else{
    v.names.transmit <- NULL
  }
  
  if(!is.null(drop)){
    drop.transmit <- paste(drop,collapse=",")
  }else{
    drop.transmit <- NULL
  }
  
  # create a name by default if user did not provide a name for the new object
  if(is.null(newobj)){
    if (direction=='wide'){
      newobj <- paste0(data.name, "_wide")
    }
    if (direction=='long'){
      newobj <- paste0(data.name, "_long")
    }  
  }

  ############################## 
  # call the server side function
  calltext <- call("reShapeDS.o", data.name, varying.transmit, v.names.transmit, timevar.name, idvar.name, drop.transmit, direction, sep)
  datashield.assign(datasources, newobj, calltext)
 
#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#																											#
																											#							
# CALL SEVERSIDE FUNCTION                                                                                	#
calltext <- call("testObjExistsDS.o", test.obj.name)													 	#
																											#
object.info<-datashield.aggregate(datasources, calltext)												 	#
																											#
# CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 	#
# AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 	#
num.datasources<-length(object.info)																	 	#
																											#
																											#
obj.name.exists.in.all.sources<-TRUE																	 	#
obj.non.null.in.all.sources<-TRUE																		 	#
																											#
for(j in 1:num.datasources){																			 	#
	if(!object.info[[j]]$test.obj.exists){																 	#
		obj.name.exists.in.all.sources<-FALSE															 	#
		}																								 	#
	if(object.info[[j]]$test.obj.class=="ABSENT"){														 	#
		obj.non.null.in.all.sources<-FALSE																 	#
		}																								 	#
	}																									 	#
																											#
if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 	#
																											#
	return.message<-																					 	#
    paste0("A data object <", test.obj.name, "> has been created in all specified data sources")		 	#
																											#
																											#
	}else{																								 	#
																											#
    return.message.1<-																					 	#
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")	#
																											#
	return.message.2<-																					 	#
	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 	#
																											#
	return.message.3<-																					 	#
	paste0("Please use ds.ls() to identify where missing")												 	#
																											#
																											#
	return.message<-list(return.message.1,return.message.2,return.message.3)							 	#
																											#
	}																										#
																											#
	calltext <- call("messageDS.o", test.obj.name)															#
    studyside.message<-datashield.aggregate(datasources, calltext)											#
																											#	
	no.errors<-TRUE																							#
	for(nd in 1:num.datasources){																			#
		if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){			#
		no.errors<-FALSE																					#
		}																									#
	}																										#	
																											#
																											#
	if(no.errors){																							#
	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
	return(list(is.object.created=return.message,validity.check=validity.check))						    #
	}																										#
																											#
if(!no.errors){																								#
	validity.check<-paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")   #
	return(list(is.object.created=return.message,validity.check=validity.check,					    		#
	            studyside.messages=studyside.message))			                                            #
	}																										#
																											#
#END OF CHECK OBJECT CREATED CORECTLY MODULE															 	#
#############################################################################################################
 
}
#ds.reShape.o



