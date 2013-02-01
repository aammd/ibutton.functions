## functions for the analysis of ibuttons.
## these functions should work on any ibutton files

## add the filename as a column alongside the result of read.csv
read.csv.with.name <- function(f, ...){
  cbind(ibutton=sub("\\.csv$", "", basename(f)), read.csv(f, ...))
}

## read all the ibutton files in one folder
read.ibutton.folder <- function(folder,...){
  # foldername <- paste0("./",folder)
  ibut.files <- dir(path=folder,full=TRUE,pattern="*.csv")
  ibut.dat <- lapply(ibut.files,read.csv.with.name,skip=13,stringsAsFactors=FALSE)
  names(ibut.dat) <- lapply(ibut.files,function(f) sub("\\.csv$", "", basename(f)))
  ibut.dat
}

## check for broken ones
## for use on the LIST of ibuttons (before they are bound together)
id.broken <- function(ibutt.list){
  n.obs <- sapply(ibutt.list,nrow)
  med.n.obs <- median(n.obs)
  prob.broken <- which(n.obs<med.n.obs)
  names(ibutt.list)[prob.broken]
}

## This function extracts the registration number for an ibutton from the "preamble"
## (ie the 13 lines of information at the start of an ibutton file)
## it is a workhorse for the next function
preamble.extract.registration.number <- function(ibutton.preamble){
  location.reg.number <- grep(ibutton.preamble,pattern="Registration Number",useBytes=TRUE)
  split.line <- strsplit(ibutton.preamble[location.reg.number],split=" ") 
  sapply(split.line,function(x) grep(x,pattern="[0-9,A-Z]{16}",value=TRUE))
}

## read all the ibutton files in one folder:
## read only the top of each file
## extract from that the registration numbers
get.registration.numbers <- function(folder,...){
  # foldername <- paste0("./",folder)
  ibut.files <- dir(path=folder,full=TRUE,pattern="*.csv")
  ibutton.preamble <- lapply(ibut.files,function(x) readLines(x,n=13))
  sapply(ibutton.preamble,preamble.extract.registration.number)
}

## function which combines given list of ibuttons into a workable dataframe
## add them all together
## convert the 'date' format
ibuttons.to.data.frame <- function(ibut.list){
  ## attach all together in a complete dataframe
  ibutton.long.dataframe <- do.call(rbind,ibut.list)
  ibutton.long.dataframe[["Date.Time"]] <- strptime(ibutton.long.dataframe[["Date.Time"]],format="%d/%m/%y %I:%M:%S %p")
  ibutton.long.dataframe
}