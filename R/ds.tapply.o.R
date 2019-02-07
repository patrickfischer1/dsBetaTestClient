#' 
#' @title ds.tapply.o calling tapplyDS.o
#' @description Apply a Function to summarize a variable over one or more factors
#' @details An aggregate function that uses the native R function tapply() to apply a function to each
#' cell of a ragged array, that is to each (non-empty) group of values given by a unique
#' combination of the levels of certain factors. For example, the function can be used to
#' calculate the mean of BMI over males and females. 
#' @param X.name, the name of the variable to be summarized. The user must set the name as a
#' character string in inverted commas
#' @param INDEX.names, the name of a single factor or a list of factors to index the variable 
#' to be summarized. The user must specify this argument in inverted commas.
#' @param FUN.name, the name of a valid function to be applied. The present version of this
#' function allows the user to choose one of the four main summarizing functions that are 'mean',
#' 'sd', 'sum', or 'quantile'.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return an array of the summarized values created by the tapplyDS.o function. The array has
#' the same number of dimensions as INDEX has components.
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
#'
ds.tapply.o <- function(X.name=NULL, INDEX.names=NULL, FUN.name=NULL, datasources=NULL){

  ###datasources
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  ###X.name
  # check if user has provided the name of the column that holds X.name
  if(is.null(X.name)){
    return("Error: Please provide the name of the variable to be summarized, as a character string")
  }

  ###INDEX.names
  # check if user has provided the name of the column(s) that holds INDEX.names
  if(is.null(INDEX.names)){
    Err.1 <- "Error: Please provide the name of the single factor or"
    Err.2 <- "the list of factors to index the variable to be summarized."
    Err.3 <- "In either case the argument must be specified in inverted commas"
    return(list(Error.message=Err.1, Err.cont2=Err.2, Err.cont3=Err.3))
  }

  if(!is.null(INDEX.names)){
    INDEX.names.transmit <- paste(INDEX.names,collapse=",")
  }else{
    INDEX.names.transmit <- NULL
  }

  ###FUN.name  
  # check if user has provided a valid summarizing function
  if(is.null(FUN.name)){
    return("Error: Please provide a valid summarizing function, as a character string")
  }

  # CALL THE PRIMARY SERVER SIDE FUNCTION
  calltext <- call("tapplyDS.o", X.name, INDEX.names.transmit, FUN.name)
  output <- datashield.aggregate(datasources, calltext)
 
  return(output)
  
}
#ds.tapply.o

