#' 
#' @title ds.make
#' @description Makes (calculates) a new object in the R environment on the server side. ds.make is equivalent to ds.assign, but runs slightly faster.
#' It defines a datashield object via any allowed function or an arithemetic expression hence creating a new object
#' in the server side R environments. The function is a wrapper for the 'opal' package function 'datashield.assign'.
#' @details If the newObject is created successfully, the function will verify its existence on the required servers.
#' Otherwise it will report that there has been an error and the data object has not been created.
#' @param toAssign A character string specifying the function call or the arithmetic expression
#' that generates the newObject. In general the string should be reasonably simple to avoid blocking by the parser.
#' In general, a complex (many brackets) expressions can almost always be broken down into a series of simple steps - e.g.
#' see example 1 below. If toAssign is a simple pre-existing data object, it will simply be copied and assigned as having a second name
#' as specified by the newobj argument - e.g. see example 1 below.
#' @param newobj The name of the new object to be created. If no newobj name is provided, the new object is named 'newObject' by default,
#' otherwise the name can be specified using the newobj argument.
#' @param datasources specifies the particular opal object(s) to use. 
#' the default set of opals will be used. The default opals are always called default.opals.
#' This parameter is set without inverted commas: e.g. datasources=opals.em or datasources=default.opals
#' If you wish to specify the second opal server in a set of three, the parameter is specified:
#' e.g. datasources=opals.em[2]. If you wish to specify the first and third opal servers in a set specify:
#' e.g. datasources=opals.em[2,3]
#' @return the object specified by the newobj argument (or default name newObject) is written to the
#' serverside and a validity message indicating whether the newobject has been correctly
#' created at each source is returned to the client. If it has not been correctly created the return object
#' return.info details in which source the problem exists and whether: (a) the object exists at all; (b) it has meaningful
#' content indicated by a valid class. 
#' @author Burton PR, Gaye, A.
#' @export
#' @examples {
#'
#' ##EXAMPLE 1
#' ##CONVERT PROPORTIONS IN prop.rand TO log(odds) IN logodds.rand
#' #ds.make.o("(prop.rand)/(1-prop.rand)","odds.rand")
#' #ds.make.o("log(odds.rand)","logodds.rand")
#' 
#' 
#'
#' ##EXAMPLE 2
#' ##MISCELLANEOUS ARITHMETIC OPERATORS: ARBITRARY CALCULATION
#' ##USE DEFAULT NEW OBJECT NAME
#' #ds.make.o("((age.60+bmi.26)*(noise.56-pm10.16))/3.2")
#' 
#' 
#'
#' ##EXAMPLE 3
#' ##MISCELLANEOUS OPERATORS WITHIN FUNCTIONS (female.n is binary 1/0 so female.n2 = female.n
#' ##and so they cancel out in code for second call to ds.make.o and so that call is
#' ##equivalent to copying log.surv to output.test.1)  
#' #ds.make.o("female.n^2","female.n2")
#' #ds.make.o("(2*female.n)+(log.surv)-(female.n2*2)","output.test.1") 
#' #ds.make.o("exp(output.test.1)","output.test")
#' }
#' 
ds.make.o<-function(toAssign=NULL, newobj="newObject", datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  
  if(is.null(toAssign)){
    stop("Please give the name of object to assign or an expression to evaluate and assign.!\n", call.=FALSE)
  }
  
  # now do the business
  datashield.assign(datasources, newobj, as.symbol(toAssign))

 
  
##########################################################################################################
#MODULE 5: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                                   #
                                                                                                         #
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 #
test.obj.name<-newobj                                                                                    #
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
	return.message<-list(return.message.1,return.message.2,return.message.3)                             #
																										 #
	return.info<-object.info																			 #
																										 #
	return(list(return.info=return.info,return.message=return.message))									 #
																										 #
	}																									 #
#END OF MODULE 5																						 #
##########################################################################################################

}

# ds.make.o  

  
