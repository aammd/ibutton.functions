## this file demonstrates how to use a collection of functions to load and
## analyze ibutton temperature data
## written by 
## Andrew MacDonald, Feb 2012


# load in required packages -----------------------------------------------

library(reshape)
library(lattice)

# load my functions -------------------------------------------------------

source('ibutton.functions.R')

# analysis ----------------------------------------------------------------

ibutton.data <- read.ibutton.folder("example.data/")

## which ones failed?

lengths.A <- sapply(blkA,nrow)
lengths.B <- sapply(blkB,nrow)
lengths.C <- sapply(blkC,nrow)

## let's see the head of the ibuttons which failed define "file" as less than
## 1500 (close to the maximum duration of all the ibuttons)
lapply(blkA[which(lengths.A<1500)],head)
lapply(blkB[which(lengths.B<1500)],head)
which(lengths.C<1500)

apply(blkC,head)

## automate this process: which ibuttons recorded less than the median number of
## datapoints for that temporal block?
id.broken(blkA)
id.broken(blkB)
id.broken(blkC)

## 10a and 15a seems to be 

## check for people recording data from the same ibutton in files with different names.

table(get.registration.numbers("./Costa Rica Data loggers 2012/Time Block A/"))
table(get.registration.numbers("./Costa Rica Data loggers 2012/Time Block B/"))
table(get.registration.numbers("./Costa Rica Data loggers 2012/Time Block c/"))

## correct the dates and make dataframe
blockA.dataframe <- ibuttons.to.data.frame(blkA)
blockB.dataframe <- ibuttons.to.data.frame(blkB)
blockC.dataframe <- ibuttons.to.data.frame(blkC)
head(blockB.dataframe)

## graphing.  must be improved.
with(subset(blockA.dataframe,ibutton=="10a"),plot(Date.Time,Value,type="l"))
#with(data2,tapply(labels,plot(time,Value,type="l")))

for(i in levels(blockA.dataframe$ibutton)){
  x <- which(blockA.dataframe$ibutton==i)
  lines(blockA.dataframe$Date.Time[x],blockA.dataframe$Value[x])
}