#' 
#' @title ds.dataFrameSort.o calling dataFrameSortDS.o
#' @description Sorts a data frame using a specified sort key
#' @details A data frame is a list of variables all with the same number of rows,
#' which is of class 'data.frame'. ds.dataFrameSort.o will sort a specified
#' data.frame on the serverside using a sort key also on the serverside. The
#' sort key can either sit in the data.frame or outside it. The sort key
#' can be forced to be interpreted as either alphabetic or numeric
#' but not both. If neither interpretation is forced, the sort.key will
#' interpreted naturally: as numeric if it is numeric, otherwise as alphabetic
#' ie as if it is a vector of character strings.
#'
#' It should be noted that although we are all well used to seeing numbers
#' sorted numerically, and character strings (words) sorted alphabetically
#' when a numeric vector is sorted alphabetically, the order can look confusing.
#' It is worth mentioning this because Opal sometimes sorts its data tables
#' alphabetically using ID. This can be confusing and one of the reasons for
#' using the ds.dataFrameSort.o() function is to re-sort a data.frame derived
#' from an Opal data table so its order is more natural. To explain alphabetic
#' and numeric sorting further, here are some illustrations.
#' SORTING NUMBERS:
#' vector.2.sort = (-192   76  841   NA 1670  163  147  101 -112 -231   -9  119  112   NA)
#' numeric.sort = (-231 -192 -112   -9   76  101  112  119  147  163  841 1670   NA   NA)
#' alphabetic.sort = (-112 -192 -231   -9  101  112  119  147  163 1670   76  841   NA   NA)
#' Notes:
#' Ascending numeric sorting of a numeric vector orders the values in
#' naturally increasing manner with negative values first. The positioning of NAs
#' (missing values) defaults to last.
#' Ascending alphabetic sorting of a numeric vector treats each number as a word
#' and then working from the front of the word, sorts all numbers by the first
#' character of the word, then breaks ties using the second character and so on.
#' ie as is usual in alphabetic sorting of alphabetic words. The sort order can
#' look strange in certain settings. (1) If a number is negative, the first character is
#' '-' which lies ahead of any of the numeric digits in the alphanumeric alphabet. Thus
#' negative numbers appear first (as in numeric sorting) but the ordering of the
#' negative numbers is then the opposite way round to expectation. Thus, instead of
#' the order -231, -192, -112, as in the numeric sort one obtains -112, -192, -231.
#' This is because these three numbers 'tie' on the first character '-' so are then
#' sorted on their second characters. The individual numeric digits have the same
#' alphabetic order as numeric order (0,1,2, ... , 8, 9). This means that having tied
#' on '-' the numbers with second character '1' come before those with second character
#' '2'. (2) Under a numeric sorts, and assuming no decimal point or negative sign,
#' numbers with more digits are of larger magnitude than those with fewer. But under
#' an alphabetic sort all that matters is the sort order of the first character
#' that is not tied with the corresponding number in another number.
#' So the numerically sorted set of numbers 76 841 1670 becomes 1670 76  841
#' under alphabetic sorting.
#' SORTING CHARACTER STRINGS (WORDS):
#' words.2.sort = ("a"  "L"  "p"  "h"  "A"  ""   "nu" "Me" "R"  "IC" "")
#' alphabetic.sort = (""   ""   "a"  "A"  "h"  "IC" "L"  "Me" "nu" "p"  "R")
#' There are few surprises here, perhaps the only thing to note is that
#' missing values (empty strings) get ordered first by default rather than last. 
#' @param df.name a character string providing the name for the data.frame
#' to be sorted
#' @param sort.key.name a character string providing the name for the sort key
#' @param sort.descending logical, if TRUE the data.frame will be sorted
#' by the sort key in descending order. Default = FALSE (sort order ascending)
#' @param sort.alphabetic logical, if TRUE the sort key is treated as if alphabetic
#' Default=FALSE.
#' @param sort.numeric logical, if TRUE the sort key is treated as if numeric
#' Default=FALSE.
#' @param newobj This a character string providing a name for the output
#' data.frame which defaults to '<df.name>.sorted' if no name is specified
#' where <df.name> is the first argument of ds.dataFrameSort.o().
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return the object specified by the <newobj> argument (or default name <df.name>.sorted)
#' which is written to the serverside. In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.dataFrameSort.o() also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author DataSHIELD Development Team
#' @export
ds.dataFrameSort.o<-function(df.name=NULL, sort.key.name=NULL, sort.descending=FALSE, sort.alphabetic=FALSE,sort.numeric=FALSE, newobj=NULL, datasources=NULL){

   # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
 
  if(is.null(newobj)){newobj<-paste0(df.name,".sorted")}
  
    calltext <- call("dataFrameSortDS.o", df.name, sort.key.name, sort.descending, sort.alphabetic, sort.numeric)
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
#ds.dataFrameSort.o

