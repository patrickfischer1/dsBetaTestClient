#' 
#' @title ds.recodeValues.o
#' @description Converts the values of a specified set of individual elements in a vector into a specified
#' set of new values
#' @details The elements to change are identified solely by value, not position of the elements in
#' the vector - this is because of the potential disclosure risk of the latter approach. Recoding is
#' undertaken by replacing all elements in the input vector, which is specified by the argument <var.name>,
#' that have a value equal to the kth element of <values2replace.vector> with the value corresponding to the kth
#' element of <new.values.vector>. This implies strict ordered one-to-one mapping. There is no need to ensure
#' that the values in either <values2replace.vector> or <new.values.vector> are numerically or alphabetically
#' ordered. If the only change required is to modify. There is one quirk in the function: specifically, if
#' the only requirement is to replace NA (missing) values in <var-name> with a new non-missing value, the
#' argument <values2replace.vector> cannot simply be specified as NA or c(NA) because this represents a vector
#' with no non-missing values which confuses the analytic processing. As an easy work round however, you
#' can instead specify a second value in the two value replacement vectors that maps to itself and thereforechanges
#' changes nothing. So, for example, <value2replace.vector> can be specified as c(3,NA) and <new.values.vector> as c(3,99).
#' This then changes NAs to 99s while leaving values of 3 as 3. 
#' @param varname A character string specifying the name of the vector whose values are to be changed
#' @param values2replace.vector A vector containing the value of elements in <var.name> that are to be changed.
#' This must either be specified in format c(1,2,8,...) where all values are numeric or, more generally,
#' as c(1,2,"hhh",-1,"b",NA,5,...) where the values specified can include numerics, characters (in inverted commas)
#' or NA (missing) values. Note when NA is specified it is specified as NA not "NA". The <values2replace.vector>
#' must not be specified as a single character string: e.g. "c(1,2,8)". No value in <values2replace.vector>
#' can be repeated. Values that do not occur in the <var.name> vector can be included (they simply do nothing).
#' @param new.values.vector A vector of the same length as <values2replace.vector> to which the values in
#' <values2replace.vector> are to be changed in recoding the vector <var.name>. The specification format is identical
#' to that for <values2replace.vector> except that values in <new.values.vector> can be repeated.
#' @param newobj A character string specifying the name of the vector to which the new recoded variable is to be written.
#' If no <newobj> argument is specified, the recoded variable name defalults to "var.name_recoded" where <var.name>
#' is the first argument of the function.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources> argument is not specified
#' the default set of opals will be used. The default opals are called default.opals and the default can be set
#' using the function {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' without inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to apply the function
#' solely to e.g. the second opal server in a set of three, the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify: e.g. datasources=opals.em[c(1,3)]
#' @return the object specified by the <newobj argument (or default name varname_recoded) which is written to the
#' serverside . In addition, a validity message indicating whether <newobj> has been correctly
#' created at each source is returned to the client. There are some circumstances in which it will be reported that
#' <newobj> has been created in every datasource but in one or more data servers <newobj> will contain an error message
#' rather than the recoded vector. The reason for this will appear as a warning in the screen output as the function
#' is run - it will generally be because one or more of the inputs or outputs to the function fails to satisfy
#' the disclosure thresholds that have been specified for your analysis. As well as appearing on the screen at run time,
#' the warning is also written as a studysideMessage, which is saved as a list object named <newobj>$studysideMessage.
#' If you wish to see the studysideMessage at a later date you can use the {ds.message.o} function. If you type
#' ds.message.o("newobj") it will print out the relevant studysideMessage from any datasource in which there was an
#' error in creating <newobj> and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("newobj") will return the message:
#' "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Burton PR, Gaye, A.
#' @export
#'
ds.recodeValues.o <- function(var.name=NULL, values2replace.vector=NULL, new.values.vector=NULL, newobj=NULL, force.output.format="no",datasources=NULL){

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

