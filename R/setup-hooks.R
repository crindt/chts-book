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
pd <- function(ps) { return(paste(rd,'data',ps,sep="/")) }
po <- function(ps) { return(paste(rd,'output',ps,sep="/")) }
pto <- function(ps) { return(paste(rd,'output/temp',ps,sep="/")) }

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

filterRmd <- function(fname,lev=0) {
    con <- file(fname, "r")
    ld <- data.frame(line=readLines(con)) %>%
        mutate(yamlcnt=ifelse(grepl("^---",line),1,0)) %>%
        mutate(yamlsum=cumsum(yamlcnt)) %>%
        mutate(yamlsum=cumsum(ifelse(row_number()==1 | lag(yamlsum)>0, yamlcnt, 0))) %>%
        mutate(yamlblock=(yamlcnt==1 | yamlsum%%2 == 1)) %>%
        filter(!yamlblock) %>%
        mutate(tickcnt=ifelse(grepl("^```",line),1,0)) %>%
        mutate(markdownblock=(cumsum(tickcnt)%%2==0)) %>%
        mutate(line=ifelse(markdownblock,
                           gsub("^#",paste(rep('#',lev+1),collapse=""),line),
                           as.character(line)
                           ))
    close(con)
    return(paste(ld$line,collapse="\n"))
}

nestedInclude <- function(fname, lev=0, ..., options = NULL, envir = parent.frame()) {
  text <- filterRmd(fname,lev)
  cat(knit_child(text=text,options=options,envir=envir,quiet=TRUE,...))
}

pandoc.table.double <- function(x,justify='center',split.cells=Inf,...) {
  d <- dim(x)
  if (length(justify) == d[[2]]) {
    justify <- rep(justify,2)
  }
  if ( length(split.cells) == d[[2]] ) {
    split.cells <- unlist(lapply(
      rep(split.cells,2),
      function(c) {
        v <- as.integer(gsub("(\\d+)%?","\\1",as.character(c)))
        p <- gsub("(\\d+)(%)?","\\2",as.character(c))
        v <- v/2
        return(paste(v,p,sep=""))
      }))      
  }
  if ((d[[1]] %% 2) == 1 ) {
    y <- cbind(x[1:(ceiling(d[[1]]/2)),],rbind(x[(ceiling(d[[1]]/2)+1):d[[1]],],c(NA,NA)))
  } else {
    y <- cbind(x[1:(ceiling(d[[1]]/2)),],x[(ceiling(d[[1]]/2)+1):d[[1]],])
  }

  pandoc.table(y,justify=justify,split.cells=split.cells,...)
}
