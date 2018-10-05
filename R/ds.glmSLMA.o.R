#'
#'  @title ds.glm calling glmDS1, glmDS2
ds.glmSLMA<-function(formula=NULL, family=NULL, offset=NULL, weights=NULL, dataName=NULL,
                     checks=FALSE, maxit=15, CI=0.95, viewIter=FALSE, viewVarCov=FALSE, viewCor=FALSE, datasources=NULL) {
  #THIS VERSION DOES NO MORE THAN ADD try() TO MATRIX INVERSION SO SHOULD WORK JUST THE SAME AS USUAL IF MATRIX IS INVERTABLE
  
  # details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # verify that 'formula' was set
  if(is.null(formula)){
    stop(" Please provide a valid regression formula!", call.=FALSE)
  }
  
  # check if user gave offset or weights directly in formula, if so the argument 'offset' or 'weights'
  # to provide name of offset or weights variable
  if(sum(as.numeric(grepl('offset', formula, ignore.case=TRUE)))>0 ||
     sum(as.numeric(grepl('weights', formula, ignore.case=TRUE)))>0)
  {
    cat("\n\n WARNING: you may have specified an offset or regression weights")
    cat("\n as part of the model formula. In ds.glm (unlike the usual glm in R)")
    cat("\n you must specify an offset or weights separately from the formula")
    cat("\n using the offset or weights argument.\n\n")
  }
  
  formula <- as.formula(formula)
  
  
  # check that 'family' was set
  if(is.null(family)){
    stop(" Please provide a valid 'family' argument!", call.=FALSE)
  }
  
  # if the argument 'dataName' is set, check that the data frame is defined (i.e. exists) on the server site
  if(!(is.null(dataName))){
    defined <- isDefined(datasources, dataName)
  }
  
  # beginning of optional checks - the process stops if any of these checks fails #
  if(checks){
    message(" -- Verifying the variables in the model")
    # call the function that checks the variables in the formula are defined (exist) on the server site and are not missing at complete
    
    glmChecks(formula, dataName, offset, weights, datasources)
  }else{
    #message("WARNING:'checks' is set to FALSE; variables in the model are not checked and error messages may not be intelligible!")
  }
  
  #MOVE ITERATION COUNT BEFORE ASSIGNMENT OF beta.vect.next  
  #Iterations need to be counted. Start off with the count at 0
  #and increment by 1 at each new iteration
  iteration.count<-0
  
  # number of 'valid' studies (those that passed the checks) and vector of beta values
  numstudies <- length(datasources)
  
  
  #ARBITRARY LENGTH FOR START BETAs AT THIS STAGE BUT IN LEGAL TRANSMISSION FORMAT ("0,0,0,0,0")
  beta.vect.next <- rep(0,5)
  beta.vect.temp <- paste0(as.character(beta.vect.next), collapse=",")
  
  
  #IDENTIFY THE CORRECT DIMENSION FOR START BETAs VIA CALLING FIRST COMPONENT OF glmDS
  
  cally1 <- call('glmDS1.o', formula, family, weights, dataName)
  
  study.summary.0 <- datashield.aggregate(datasources, cally1)
  
  
  at.least.one.study.data.error<-0
  
  for(hh in 1:numstudies) {
    if(study.summary.0[[hh]]$errorMessage!="No errors"){
      at.least.one.study.data.error<-1
    }
  }
  
  
  num.par.glm<-NULL
  coef.names<-NULL
  
  if(at.least.one.study.data.error==0){
    num.par.glm<-study.summary.0[[1]][[1]][[2]]
    coef.names<-study.summary.0[[1]][[2]]
  }
  
  y.invalid<-NULL
  Xpar.invalid<-NULL
  w.invalid<-NULL
  glm.saturation.invalid<-NULL
  errorMessage<-NULL
  
  for(ss in 1:numstudies)
  {
    y.invalid<-c(y.invalid,study.summary.0[[ss]][[3]])
    Xpar.invalid<-rbind(Xpar.invalid,study.summary.0[[ss]][[4]])
    w.invalid<-c(w.invalid,study.summary.0[[ss]][[5]])
    glm.saturation.invalid <-c(glm.saturation.invalid,study.summary.0[[ss]][[6]])
    errorMessage<-c(errorMessage,study.summary.0[[ss]][[7]])
  }
  
  y.invalid<-as.matrix(y.invalid)
  sum.y.invalid<-sum(y.invalid)
  dimnames(y.invalid)<-list(names(datasources),"Y VECTOR")
  
  Xpar.invalid<-as.matrix(Xpar.invalid)
  sum.Xpar.invalid<-sum(Xpar.invalid)
  dimnames(Xpar.invalid)<-list(names(datasources),coef.names)
  
  
  w.invalid<-as.matrix(w.invalid)
  sum.w.invalid<-sum(w.invalid)
  dimnames(w.invalid)<-list(names(datasources),"WEIGHT VECTOR")
  
  glm.saturation.invalid<-as.matrix(glm.saturation.invalid)
  sum.glm.saturation.invalid<-sum(glm.saturation.invalid)
  dimnames(glm.saturation.invalid)<-list(names(datasources),"MODEL OVERPARAMETERIZED")
  
  errorMessage<-as.matrix(errorMessage)
  dimnames(errorMessage)<-list(names(datasources),"ERROR MESSAGES")
  
  
  
  output.blocked.information.1<-"MODEL FITTING TERMINATED AT FIRST ITERATION:"
  output.blocked.information.2<-"ANY VALUES OF 1 IN THE FOLLOWING TABLES DENOTE"
  output.blocked.information.3<-"POTENTIAL DISCLOSURE RISKS. PLEASE USE THE ARGUMENT"
  output.blocked.information.4<-"[datasources=] TO EXCLUDE STUDIES WITH DATA ERRORS"
  
  
  
  
  if(sum.y.invalid>0||sum.Xpar.invalid>0||sum.w.invalid>0||sum.glm.saturation.invalid>0||at.least.one.study.data.error==1){
    return(list(
      output.blocked.information.1,
      output.blocked.information.2,
      output.blocked.information.3,
      output.blocked.information.4,
      y.vector.error=y.invalid,
      X.matrix.error=Xpar.invalid,
      weight.vector.error=w.invalid,
      glm.overparameterized=glm.saturation.invalid,
      errorMessage=errorMessage
    ))
    stop("DATA ERROR") 
  }
  
  
  
  beta.vect.next <- rep(0,num.par.glm)
  beta.vect.temp <- paste0(as.character(beta.vect.next), collapse=",")
  
  
  #Provide arbitrary starting value for deviance to enable subsequent calculation of the
  #change in deviance between iterations
  dev.old<-9.99e+99
  
  #Convergence state needs to be monitored.
  converge.state<-FALSE
  
  #Define a convergence criterion. This value of epsilon corresponds to that used
  #by default for GLMs in R (see section S3 for details)
  epsilon<-1.0e-08
  
  f<-NULL
  
  #NOW CALL SECOND COMPONENT OF glmDS TO GENERATE SCORE VECTORS AND INFORMATION MATRICES
  
  cally2 <- call('glmDSSLMA2', formula, family, offset, weights, dataName)
  
  study.summary <- datashield.aggregate(datasources, cally2)
  
}
# ds.glmSLMA 
