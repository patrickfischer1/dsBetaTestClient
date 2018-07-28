ds.recodeValues.o<-function(var.name=NULL, values2replace.vector=NULL, new.values.vector=NULL, newobj=NULL, force.output.format="no",datasources=NULL){

   # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
 
 
    # check user has provided the name of the variable to be recoded
  if(is.null(var.name)){
    stop("Please provide the name of the variable to be recoded: eg 'xxx'", call.=FALSE)
  }

    # check user has provided the vector specifying the set of values to be replaced
  if(is.null(values2replace.vector)){
    stop("Please provide a vector specifying the values to be replaced eg c(1,7,NA)", call.=FALSE)
  }

    # check user has provided the vector specifying the set of values to be replaced
  if(is.null(new.values.vector)){
    stop("Please provide a vector specifying the new values to be set eg c(3,NA,4)", call.=FALSE)
  }
  
    # check values2replace.vector and new.values.vector have the same length
  if(length(values2replace.vector)!=length(new.values.vector)){
    stop("Please ensure that values2replace.vector and new.values.vector have same length and are in the same order", call.=FALSE)
  }
  

    # check no duplicate values in values2replace.vector
  if(length(values2replace.vector)!=length(unique(values2replace.vector))){
    stop("No value may appear more than once in the values2replace.vector", call.=FALSE)
  }
    # simple work around for a bug in the format for values2replace.vector

if(length(values2replace.vector)==1&&is.na(values2replace.vector)){
stop("SORRY THIS NEEDS A WORK AROUND FOR A SMALL BUT IRRITATING BUG.
If you want to specify just NA or c(NA) as the values2replace.vector, please specify as c(x,NA)
and the new.value.vector as c(x,y). Where x is one of the other levels in the vector to be recoded,
and y is the value to which you want to convert the NAs in the input vector", call.=FALSE)
  }



#DETERMINE WHETHER new.values.vector CONTAINS NON-NUMERIC ELEMENTS (IF SO CAN ONLY GET NUMERIC OUTPUT
#BY force.output.format="numeric" AND NON-NUMERICS WILL THEN BE SET AS NaN)

	#is new.values.vector all NA?
	nvv.all.NA<-(sum(is.na(new.values.vector))==length(new.values.vector))
	nvv.numeric<-is.numeric(new.values.vector)

 numeric.output.format.possible<-(nvv.all.NA||nvv.numeric)
 
    #is values2replace.vector numeric?

	v2r.numeric<-is.numeric(values2replace.vector)
 
 
  values2replace.transmit<-paste0(as.character(values2replace.vector),collapse=",")
  
  new.values.transmit<-paste0(as.character(new.values.vector),collapse=",")
  

	 if(is.null(newobj)){newobj<-paste0(var.name,"_recoded")}

 
    calltext1 <- call("recodeValuesDS1.o", var.name, values2replace.transmit, new.values.transmit)
  	return.warning.message<-datashield.aggregate(datasources, calltext1)
	
    calltext2 <- call("recodeValuesDS2.o", var.name, values2replace.transmit, new.values.transmit,numeric.output.format.possible,force.output.format,v2r.numeric)
  	datashield.assign(datasources, newobj, calltext2)

	numsources<-length(datasources)
	for(s in 1:numsources){
	num.messages<-length(return.warning.message[[s]])
	if(num.messages==1){
	cat("\nSource",s,"\n",return.warning.message[[s]][[1]],"\n\n")
	}else{
	cat("\nSource",s,"\n")
		for(m in 1:(num.messages-1)){
		cat(return.warning.message[[s]][[m]],"\n")
		}
		cat(return.warning.message[[s]][[num.messages]],"\n\n")
	}
}
	

#############################################################################################################
#DataSHIELD MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                             #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#
																											#
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
    paste0("Data object <", test.obj.name, "> created in all specified data sources")		 	 		 	#

																											#
	return(list(return.message=return.message))															 	#
			
			
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
	return.info<-object.info																			 	#
																		
	return(list(return.info=return.info,return.message=return.message))									 	#
																											#
	}																									 	#
#END OF MODULE 5																						 	#
#############################################################################################################

	
	
}
#ds.recodeValues.o

