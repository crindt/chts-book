suppressMessages(require(knitr))
suppressMessages(require(pander))
suppressMessages(require(testit))

# set some defaults
opts_chunk$set(eval=TRUE,echo=TRUE,messages=FALSE,
               warnings=TRUE,errors=TRUE,cache=TRUE,dpi=300,
               size='tiny',collapse=TRUE,my.relfontsize=-1,
               fig.height=4)

# function to provide shorthand for loading chunks.  Basically,
# assumes code for a chunk named CHUNK will be in the R/CHUNK.R file.
quickload <- function(chunk,eval=FALSE) {
  fname <- paste("R/",chunk,".R",sep="")
  if ( eval )
    source(fname)
  else
    sub=read_chunk(fname,labels=chunk)
}

# A function to write progress to the console, but *not* the output document.
# Calls to this function are used to monitor the processing of the Rmd files.
echoProgress <- function(str) {
    write(str,stderr())
}

#panderOptions(table.split.cells=50,table.alignment.default='left')

qfmt <- function(val) { return(format(big.mark=",",val)) }

pp <- function(n) { return(paste(rd,n,sep="/")) }
p <- function(ps) { return(paste(rd,ps,sep="/")) }

if ( !exists("lev") ) { lev <- 0 }  # section nesting
SEC <- function(level,text,as.string=FALSE) {
    head <- gsub("^\\s*(#+)\\s*$","\\1",level)
    ret <- paste("\n",paste(rep('#',nchar(head)+lev),collapse="")," ",text,"\n\n",sep="")
    if ( as.string ) {
      return(ret)
    } else {
      cat(ret)
    }
}
