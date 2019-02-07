#' 
#' @title ds.seq.o calling seqDS.o
#' @description Generates useful sequences to support data management and analysis
#' @details An assign function that uses the native R function seq() to create
#' any one of a flexible range of sequence vectors that can then be used to help
#' manage and analyse data. As it is an assign function the resultant vector is
#' written as a new object onto all of the specified data source servers. For
#' the purposes of creating the DataSHIELD equivalent to seq() in native R we
#' have used all of the original arguments (see below) except the <to> argument.
#' This simplifies the function and prevents some combinations of arguments that
#' lead to an error in native R. The effect of the <to> argument - see help(seq) in
#' native R - is to specify the terminal value of the sequence. However,
#' when using seq() one can usually specify other arguments (see below) to mimic
#' the desire effect of <to>. These include: <from>, the starting value of the
#' sequence; <by>, its increment (+ or -), and <length.out> the length of the final vector
#' in each data source.
#' @param FROM.value.char, a number given as a character denoting the starting value of the sequence. 
#' The default value is set to 1.
#' @param BY.value.char, a number given as a character indicating the increment of the sequence.
#' The default value is set to 1.
#' @param LENGTH.OUT.value.char, a non-negative number given as character denoting the desired 
#' length of the sequence. If the argument \code{ALONG.WITH.name} is set, then the
#' \code{LENGTH.OUT.value.char} argument must miss out. 
#' @param ALONG.WITH.name, is the name of a serverside vector in inverted commas that is used to
#' determine the length of the created vector. The length of the assigned sequence will then be
#' equal to the length of the vector specified by the \code{ALONG.WITH.name} argument even if
#' a value for the \code{LENGTH.OUT.value.char} argument is set. If you want to specify the output
#' length with the \code{LENGTH.OUT.value.char} argument you must miss out the \code{ALONG.WITH.name}
#' argument altogether
#' @param newobj This a character string providing a name for the output
#' sequence vector which defaults to 'newObject' if no name is specified.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return the object specified by the newobj argument (or default name newObject) is written to the
#' serverside and a validity message indicating whether the newobject has been correctly
#' created at each source is returned to the client. The object is a sequence vector. If it has not
#' been correctly created the return object return.info details in which source the problem exists
#' and whether: (a) the object exists at all; (b) it has meaningful content indicated by a valid class.
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
#'
ds.seq.o <- function(FROM.value.char="1", BY.value.char="1", LENGTH.OUT.value.char=NULL, ALONG.WITH.name=NULL,
                   newobj="newObj", datasources=NULL){

  ###datasources
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  ###FROM.value.char
  # check FROM.value.char is valid
  FROM.valid<-1
  if(!(is.null(FROM.value.char))) {
		if(!is.character(FROM.value.char))FROM.valid<-0
		if(!is.numeric(eval(parse(text=FROM.value.char))))FROM.valid<-0
	}
	if(!FROM.valid){
    return("Error: If FROM.value.char is non.NULL, it must be a real number in inverted commas eg '-3.7' or '0'")
	}

  ###BY.value.char
  # check BY.value.char is valid
  BY.valid<-1
  if(!(is.null(BY.value.char))) {
		if(!is.character(BY.value.char))BY.valid<-0
		if(!is.numeric(eval(parse(text=BY.value.char))))BY.valid<-0
	}
	if(!BY.valid){
    return("Error: If FROM.value.char is non.NULL, it must be a real number in inverted commas eg '5' or '-98.7321'")
	}

  ###LENGTH.OUT.value.char
  # check LENGTH.OUT.value.char is valid
	LENGTH.OUT.valid<-1
  if(!(is.null(LENGTH.OUT.value.char))) {
		if(!is.character(LENGTH.OUT.value.char)) LENGTH.OUT.valid<-0
		if(!is.numeric(eval(parse(text=LENGTH.OUT.value.char)))) LENGTH.OUT.valid<-0
	}
	if(!LENGTH.OUT.valid){
    return("Error: If LENGTH.OUT.value.char is non.NULL, it must be an integer in inverted commas eg '87187'")
	}

  ###ALONG.WITH.name
  # check if user has correctly provided the name of a column to hold ALONG.WITH.name
  if(!(is.null(ALONG.WITH.name) || is.character(ALONG.WITH.name))){
    return("Error: If ALONG.WITH.name is non.NULL, it must specify the name of a serverside vector in inverted commas")
	}

  ###Either LENGTH.OUT.value.char or ALONG.WITH.name must be non-NULL
  if(is.null(LENGTH.OUT.value.char)&&is.null(ALONG.WITH.name)){
    return("Error: Either LENGTH.OUT.value.char or ALONG.WITH.name must be non-NULL, they cannot both be NULL")
	}


  # CALL THE PRIMARY SERVER SIDE FUNCTION
  calltext <- call("seqDS.o", FROM.value.char, BY.value.char, LENGTH.OUT.value.char, ALONG.WITH.name)
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
#ds.seq.o

