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

if ( !exists("lev") ) { lev <<- 0 }  # section nesting
SEC <- function(level,text,as.string=FALSE) {
    head <- gsub("^\\s*(#+)\\s*$","\\1",level)
    ret <- paste("\n",paste(rep('#',nchar(head)+lev),collapse="")," ",text,"\n\n",sep="")
    if ( as.string ) {
      return(ret)
    } else {
      cat(ret)
    }
}
NESTLEV <- function(depth=1) { lev <<- lev + depth; return("") }
UNNESTLEV <- function(depth=1) { lev <<- lev - depth; return("") }

suppressMessages(require(chts2011))
suppressMessages(require(dplyr))
var.values <<- function(vn) {
    return(strsplit((chts_md %>% filter(VAR.NAME==vn))[1,'VALUES'],'\n')[[1]])
}

suppressMessages(require(pander))
var.value.summary <<- function(vn,tab='hh',head='###',val.list=TRUE,desc=NA) {
    vlist <- var.values(vn)
    vdesc <- desc
    if ( is.na(vdesc) ) {
        vdesc <- chts_md %>% filter(VAR.NAME==vn & TABLE==tab) %>% select(Variable.Description)
    }

    SEC(head,paste(vn,': ',vdesc,"\n",sep=""))
    cat("\n")
    if( val.list ) {
        cat(paste("Valid values for ",vdesc," (`",vn,"`) are as follows:\n",sep=""))
        pandoc.list(vlist)
    }
}
