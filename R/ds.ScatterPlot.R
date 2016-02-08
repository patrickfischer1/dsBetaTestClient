#' 
#' @title Generates a scatter plot for the centroids of each n nearest neighbours
#' @description This function generates a scatter plot for the centroids of each n nearest
#' neighbours from the original dataset.
#' @details As the generation of a scatter plot from original data is disclosive and is not
#' permitted in DataSHIELD, this function allows the user to plot a scatter plot of the
#' centroids of each original data point and its n-1 nearest neighbours. The user
#' can choose any value for n equal to or greater than 3.
#' @param x a character, the name of a numeric vector, the x-variable.
#' @param y a character, the name of a numeric vector, the y-variable.
#' @param n the number of the nearest neghbours for which their centroid is calculated. 
#' By default the value of n is set to be equal to 3. The user can choose any value for n 
#' equal to or greater than 3.
#' @param type a character which represent the type of graph to display. A scatter plot for
#' combined data is generated when the \code{type} is set to 'combine' (default). One 
#' scatter plot for each single study is generated when the \code{type} is set to 'split'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return one or more scatter plots depending on the argument \code{type}
#' @author Avraam, D.
#' @export
#' @examples {
#' 
#'   # load the file that contains the login details
#'   data(logindata)
#' 
#'   # login and assign the required variables to R
#'   myvar <- list("PM_BMI_CONTINUOUS", "LAB_TSC")
#'   opals <- datashield.login(logins=logindata, assign=TRUE, variables=myvar)
#' 
#'   # Example 1: generate a combined scatter plot (the default behaviour)
#'   ds.ScatterPlot(x='D$PM_BMI_CONTINUOUS', y='D$LAB_TSC')
#'
#'   # Example 2: generate a combined scatter plot of the centroids of each 5 nearest neighbours
#'   ds.ScatterPlot(x='D$PM_BMI_CONTINUOUS', y='D$LAB_TSC', n=5)
#' 
#'   # Example 3: generate a scatter plot for each study separately
#'   ds.ScatterPlot(x='D$PM_BMI_CONTINUOUS', y='D$LAB_TSC', type="split")
#'  
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.ScatterPlot <- function (x=NULL, y=NULL, n=3, type="combine", datasources=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
    if(is.null(x)){
        stop("Please provide the x-variable", call.=FALSE)
    }

    if(is.null(y)){
        stop("Please provide the y-variable", call.=FALSE)
    }
    
    if(is.null(datasources)){
        datasources <- findLoginObjects()
    }

  # the input variable might be given as column table (i.e. D$object)
  # or just as a vector not attached to a table (i.e. object)
  # we have to make sure the function deals with each case
  objects <- c(x, y)
  xnames <- extract(objects)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- isDefined(datasources, varnames[i])
    }else{
      defined <- isDefined(datasources, obj2lookfor[i])
    }
  }
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  typ.x <- checkClass(datasources, x)
  typ.y <- checkClass(datasources, y)

  # the input objects must be numeric or integer vectors
  if(typ.x != 'integer' & typ.x != 'numeric'){
    message(paste0(x, " is of type ", typ.x, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }
  if(typ.y != 'integer' & typ.y != 'numeric'){
    message(paste0(y, " is of type ", typ.y, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }
  
  # the number of n must be greater than or equal to 3 
  if(n < 3){   
    stop("n must be greater than or equal to 3.", call.=FALSE)
  }
 
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  x.lab <- xnames[[length(xnames)]]
  ynames <- extract(y)
  y.lab <- ynames[[length(ynames)]]
  
  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)

  # call the server-side function that generates the x and y coordinates of the centroids
  call <- paste0("ScatterPlotDS(", x, ",", y, ",", n, ")")
  output <- datashield.aggregate(datasources, call)

  pooled.points.x <- c()
  pooled.points.y <- c()
  for (i in 1:num.sources){
    pooled.points.x[[i]] <- output[[i]][[1]]
    pooled.points.y[[i]] <- output[[i]][[2]]
  }
  pooled.points.x <- unlist(pooled.points.x)
  pooled.points.y <- unlist(pooled.points.y)

  # plot and return the scatter plot depending on the argyment "type"
  if(type=="combine"){
    combined.scatter <- plot(pooled.points.x, pooled.points.y, xlab=x.lab, ylab=y.lab, main="Scatter Plot of the Combined Data")
    return(combined.scatter)
  }else{
    if(type=="split"){
      # set the graph area and plot
        if(num.sources > 1){
          if((num.sources %% 2) == 0){ numr <- num.sources/2 }else{ numr <- (num.sources+1)/2}
          numc <- 2
      	  par(mfrow=c(numr,numc))
          scatter <- list()
          for(i in 1:num.sources){
            title <- paste("Scatter Plot of ", stdnames[i], sep="")
	    x <- output[[i]][[1]]
            y <- output[[i]][[2]]
            scatter[[i]] <- plot(x, y, xlab=x.lab, ylab=y.lab, main=title)			
          }
	    return(scatter)
        }else{
          stop('Function argument "type" has to be either "combine" or "split"')
  	  }
    }
  }
}
