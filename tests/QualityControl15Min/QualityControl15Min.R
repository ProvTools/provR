#################################
### QUALITY CONTROL 15-MINUTE ###
#################################

# Quality control for 15-minute time-series data
# QC tests: maximum value, minimum value, repeated value, slope
# Create plots in gui and save as jpeg files
# Record DDG in text format
# Datetime field = datetime, format = YYYY-MM-DDTHH:MM:SS
# ERB rev. 22-Nov-2013

### R packages

library(chron)
require(tcltk)
require(gWidgets)
options(guiToolkit="tcltk")

library(RDataTracker)

### Functions

read.data <- function(x) {
  # get initial values
  data.file <<- "met-15min.csv"
  variable <<- "airt"
  start.date <<- "2012-01-01"
  end.date <<- "2012-03-31"
  
  # read data file
  zz <- read.csv(x)

  return(zz)
}

select.data <- function(zz) {  
  # select data for analysis
  zz$datetime <- as.character(zz$datetime)
  zz$date <- substr(zz$datetime,1,10)
  zz$time <- paste(substr(zz$datetime,12,16),":00",sep="")
  i <- which(zz$time=="24:00:00")
  zz$date[i] <- as.character(as.Date(zz$date[i])+1)
  zz$time[i] <- "00:00:00"
  zz$dt <- chron(dates=zz$date,times=zz$time,format=c(dates="y-m-d",times="h:m:s"))
  
  zz2 <- zz[,c("date","time","dt",variable)]
  names(zz2)[names(zz2)==variable] <- "var"  
  
  zz3 <- subset(zz2,zz2$dt>=start.date & zz2$dt<=end.date)

  return(zz3)
}

INPUT <- function(message) {
  # open dialog box for user input
  CHOICE <- NA
  w <- gbasicdialog(title=message, handler = function(h,...) CHOICE <<- svalue(input))
  input <- gedit("", initial.msg="", cont=w, width=20)
  addHandlerChanged(input, handler=function (h,...) {
    CHOICE <<- svalue(input)
    dispose(w)
  })
  visible(w, set=TRUE)
  return(CHOICE)
}

get.input.test <- function() {
  # name of test
  x <- INPUT("Enter test (q=quit)")
  x <- as.character(x)
  
  return(x)
}

get.input.num <- function () {
  # get number
  x <- INPUT("Enter number")
  x <- as.numeric(x)

  return(x)
}

apply.test <- function(xx,t,n) {
  xx$flag <- ""
  # maximum value test
  if (t=="max") {
    i <- which(xx$var >= n)
    xx$flag[i] <- "Q"
  }
  # minimum value test
  if (t=="min") {
    i <- which(xx$var <= n)
    xx$flag[i] <- "Q"
  }
  # slope test
  if (t=="slope") {
    for (i in 2:nrow(xx)) {
      if (!is.na(xx$var[i]) & !is.na(xx$var[i-1])) {
        difference <- abs(xx$var[i] - xx$var[i-1])
        if (difference >= n) {
          xx$flag[i] <- "Q"
          xx$flag[i-1] <- "Q"
        }
      }
    }
  }  
  # repeated value test
  if (t=="repeat" & num > 1) {
    for (i in num:nrow(xx)) {
      if (!is.na(xx$var[i])) {
        questionable <- 1
        for (j in 1:(num-1)) {
          if (is.na(xx$var[i-j]) | xx$var[i]!=xx$var[i-j]) questionable <- 0          }
        if (questionable==1) {
          for (j in 1:num) xx$flag[i-j+1] <- "Q"
        } 
      }
    }  
  }

  return(xx)
}

plot.data <- function(xx,t,n,output) {
  # if file, save plot as jpeg
  if (output=="file") {
    dpfile <- paste(getwd(),"/plot.jpeg",sep="")
    jpeg(file=dpfile,width=800,height=500,quality=100)
  }

  rows <- nrow(xx)
  xmin <- xx$dt[1]
  xmax <- xx$dt[rows]
  xlim <- c(xmin,xmax)
  xrange <- xmax-xmin
  daterange <- c(xmin,xmax)

  ymin <- min(xx$var,na.rm=TRUE)
  ymax <- max(xx$var,na.rm=TRUE)
  ylim <- c(ymin,ymax)
  yrange <- ymax-ymin

  title <- paste(variable," ",t," ",n,sep="")

  if (output=="gui") {
	  if (Sys.info()['sysname'] == "Darwin") {
		  X11(15,10)
	  }
	  else {
		  windows(15,10)
	  }
  }
  
  par(mar=c(5.1,5.1,5.1,10.1))

  plot(xaxt="n",xlim,ylim,cex.main=1.7,cex.axis=1.7,cex.lab=1.7,xlab="Date",ylab=variable,main=title)

  if (xrange<=30)axis.Date(1,at=seq(daterange[1],daterange[2],by="day"),format="%d-%b-%Y")
  if (xrange>30 && xrange<100)axis.Date(1,at=seq(daterange[1],daterange[2],by="week"),format="%d-%b-%Y")
  if (xrange>=100 && xrange <=1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="month"),format="%d-%b-%Y")
  if (xrange>1000) axis.Date(1,at=seq(daterange[1],daterange[2],by="year"),format="%b-%Y")

  good <- subset(xx,xx$flag=="")
  ques <- subset(xx,xx$flag=="Q")

  points(good$dt,good$var,lwd=2,col="blue")
  points(ques$dt,ques$var,lwd=2,col="red")

  labs <- c("Good","Questionable")
  cols <- c("blue","red")
  par(xpd=TRUE)
  
  xmin.date = as.Date(xx$date[1])
  xmax.date = as.Date(xx$date[rows])
  xrange.date = xmax.date-xmin.date
  legend(xmax.date+xrange.date/15,ymax,labs,cols,cex=1.0)

}

save.data <- function(fn,xx) {
  file.out <- paste(getwd(),"/",fn,sep="")
  write.csv(xx,file.out,row.names=FALSE)
}

### Main Program

ddg.start("main")

ddg.start("get.data")

all.data <- read.data(data.file)
selected.data <- select.data(all.data)

ddg.finish("get.data")

ddg.start("analyze.data")

input <- ""

while (input != "q") {
  ddg.start("apply.test")

  input <- get.input.test()
  if (input != "q") {
    test <- input
    num <- get.input.num()
    flagged.data <- apply.test(selected.data,test,num)
    plot.data(flagged.data,test,num,"gui")
  } 

  ddg.finish("apply.test")
}

ddg.start("save.results")

save.data("flagged-data.csv",flagged.data)
plot.data(flagged.data,test,num,"file")

ddg.finish("save.results")

ddg.finish("analyze.data")
