###########################################################################
#' CAPE Analysis Based on S&P Index Data
#' @author Matt Barry, Mike Sadler
#' @details Provides time history for S&P cyclically-adjusted P/E
#' @concepts CAPE, P/E
#' @seealso http://us.spindices.com/indices/equity/sp-500
###########################################################################
ignore <- lapply(c("quantmod","ggplot2","scales","tidyr","dplyr","stringr",
                   "lubridate","magrittr","EnvStats","grid","RcppRoll"),
                 require,quietly=FALSE,character.only=TRUE)
options("getSymbols.warning4.0"=FALSE)
options(digits=4,width=100,scipen=100)
plot_date = format(Sys.time(), "%b %d, %Y")
current_year <- year(Sys.time())
direct_method = "last.bumpup"
base_data = "data/sp-500-eps-est-df.csv" # extracted data massaged to CSV

# data path to exchange data
operating_system <- sessionInfo()$R.version$os
datapath <- switch(substr(operating_system,1,4),
                   darw = "/Users/mrb/Data",
                   ming = "c:/Users/Public/Data",
                   ... = stop(paste("Operating system",
                                    operating_system,
                                    "not identified for data location.")))

# not-in-set operator
`%nin%` <- Negate(`%in%`) 

#' @name emit function
#' @description writes plots to disk and to screen, fixed size 1024x768 PNG; creates directory if necessary
#' @param p the plot object
#' @param tag a name tag for the plot file, default 'unknown'
#' @param path the disk path prefix, relative, default 'plots/', must exist
#' @param prefix plot group name prefix, default 'cape_'
#' @param suffix plot name suffix, default '.png'
#' @param showWarnings whether to show warnings on directory creation, passed to dir.create(), default FALSE
#' @param recursive whether to create path elements other than last, passed to dir.create(), default TRUE
#' @param width plot image width in pixels, default 2014
#' @param height plot image height in pixels, default 768
#' @return nothing
emit <- function(p,
                 tag='uknnown',
                 path="plots/",
                 prefix="spinx_",
                 suffix=".png",
                 showWarnings=FALSE,
                 recursive=TRUE,
                 width=1024,
                 height=768) {
  if ("grob" %in% class(p)) {
    grid.newpage()
    grid.draw(p)
  } else {
    print(p)
  }
  dir.create(path,showWarnings,recursive)
  name = paste(path,prefix,tag,suffix,sep='')
  dev.copy(png,width=width,height=height,name)
  dev.off()
}

inflection <- function(i) {
  sprintf("%.0f%%",i*100)
}

apply_extrema <- function(p,extrema,geometric=TRUE) {
  p <- p + lapply(extrema$peaks,function(e) {
    df <- data.frame(edate=e$Date, edist =ifelse(geometric,e$GDist,e$ADist))
    geom_label(data=df,aes(x=edate,y=edist,label=inflection(edist)),inherit.aes=FALSE,
               vjust="outward",fill="green",color="black")
  })
  p <- p + lapply(extrema$trofs,function(e) {
    df <- data.frame(edate=e$Date, edist =ifelse(geometric,e$GDist,e$ADist))
    geom_label(data=df,aes(x=edate,y=edist,label=inflection(edist)),inherit.aes=FALSE,
               vjust="outward",fill="orange",color="black")
  })
  p
}

apply_current <- function(p,gdf,geometric=TRUE) {
  nf <- gdf[nrow(gdf),]
  df <- data.frame(edate=nf$Date,
                   edist=ifelse(geometric,nf$GDist,nf$ADist))
  p <- p + geom_label(data=df,aes(x=edate,y=edist,label=inflection(edist)),inherit.aes=FALSE,
                      vjust="outward",fill="white",color="black")
}

apply_thresholds <- function(p,x,sd) {
  tcolor <- "firebrick"
  p <- p + 
    geom_hline(yintercept=-1*sd,linetype="dotted",color=tcolor) +
    geom_hline(yintercept= 0*sd,linetype="dashed", color=tcolor) +
    geom_hline(yintercept= 1*sd,linetype="dotted",color=tcolor) +
    geom_hline(yintercept= 2*sd,linetype="dotted",color=tcolor) +
    geom_hline(yintercept= 3*sd,linetype="dotted",color=tcolor) +
    annotate(geom="text",x=x,y= 0*sd,color=tcolor,label="mu",parse=TRUE,vjust=1,fontface="bold") +
    annotate(geom="text",x=x,y=-1*sd,color=tcolor,label="-1*sigma",parse=TRUE,vjust=1) +
    annotate(geom="text",x=x,y= 1*sd,color=tcolor,label="1*sigma",parse=TRUE,vjust=1) +
    annotate(geom="text",x=x,y= 2*sd,color=tcolor,label="2*sigma",parse=TRUE,vjust=1) +
    annotate(geom="text",x=x,y= 3*sd,color=tcolor,label="3*sigma",parse=TRUE,vjust=1)
  p     
}


# plot the original S&P data
# one date doesn't sync with quarter ends so tweak it
base.df <- read.csv(base_data,colClasses=c("character",rep("numeric",11),"factor"))
base.df$date <- as.Date(base.df$Date,format="%m/%d/%Y")
base.df <- base.df %>% select(-Date) %>% rename(Date=date)
tbr <- which(base.df$Date=="2011/03/30")
if ( tbr > 0) 
  base.df[tbr,'Date'] <- "2011/03/31"

# FRED data pull
# CPI seasonal CPIAUCSL, GS10 ten-year UST
ignore <- getSymbols(c("CPIAUCSL","GS10"),src="FRED")
CPIAUCSL <- CPIAUCSL["1988/",] # earliest S&P data start
GS10 <- GS10["1988/",]
cpi.df <- data.frame(Date=index(CPIAUCSL),CPI=coredata(CPIAUCSL)[,1])
cpi.df$Quarter <- as.Date(as.yearqtr(cpi.df$Date)) - 1
cpi.df <- cpi.df %>% group_by(Quarter) %>% filter(row_number(Quarter)==1)
cpi.df <- cpi.df %>% select(CPI,Quarter) %>% rename(Date=Quarter)
cpi.now <- as.numeric(cpi.df[nrow(cpi.df),1])

gs10.df <- data.frame(Date=index(GS10),GS10=coredata(GS10)[,1])
gs10.df$Quarter <- as.Date(as.yearqtr(gs10.df$Date)) - 1
gs10.df <- gs10.df %>% group_by(Quarter) %>% filter(row_number(Quarter)==1)
gs10.df <- gs10.df %>% select(GS10,Quarter) %>% rename(Date=Quarter)

# merge CPI and GS10 into base
base.df <- left_join(base.df,cpi.df,by="Date")
base.df <- left_join(base.df,gs10.df,by="Date")
base.df <- base.df %>%
  arrange(Date) %>%
  mutate(Year=year(Date)) %>%
  mutate(Month=month(Date)) %>%
  mutate(CPIS=CPI/cpi.now) %>% 
  mutate(RP=Price*CPIS) %>% 
  mutate(RE=Rep.Earnings*CPIS) %>%
  mutate(RDPS=DPS*CPIS) %>%
  mutate(RSPS=SPS*CPIS) %>% 
  mutate(RBVPS=BVPS*CPIS) %>%
  mutate(RCXPS=CXPS*CPIS) %>%
  mutate(RE10=roll_sum(RE,40,align="right",fill=NA)/40) %>%
  mutate(PE10=RP/RE10)

# compute historical means, arithmetic and geometric
amean <- mean(base.df$PE10,na.rm=TRUE)
asd <- sd(base.df$PE10,na.rm=TRUE)
gmean <- geoMean(base.df$PE10,na.rm=TRUE)
gsd <- geoSD(base.df$PE10,na.rm=TRUE)

# add distance columns then build peaks and troughs lists
base.df %<>% mutate(ADist=(PE10-amean)/amean) %>% mutate(GDist=(PE10-gmean)/gmean)

peak4 <- base.df %>% filter(Year>1998 & Year < 2002) %>% arrange(desc(ADist)) %>% filter(row_number() == 1)
trof4 <- base.df %>% filter(Year>2006 & Year < 2010) %>% arrange(ADist) %>% filter(row_number() == 1)
arithmetic_extrema <- list(peaks=list(peak4),trofs=list(trof4))

peak4 <- base.df %>% filter(Year>1998 & Year < 2002) %>% arrange(desc(GDist)) %>% filter(row_number() == 1)
trof4 <- base.df %>% filter(Year>2006 & Year < 2010) %>% arrange(GDist) %>% filter(row_number() == 1)
geometric_extrema <- list(peaks=list(peak4),trofs=list(trof4))

# reproduce the Shiller index plots for calibration
ptitle <- paste("S&P Composite Index","Current-Dollar CPI-Adjusted","S&P Index Restatement",plot_date,sep=' - ')
gdf <- base.df %>% 
  gather(Plot,Series,RP,RE,RDPS) %>% 
  na.omit
p <- ggplot(gdf,aes(x=Date,y=Series,color=as.factor(Plot))) +
  geom_line() +
  scale_y_continuous(breaks=pretty_breaks()) +  # normal direction
  facet_grid(Plot~.,scales="free_y",space="fixed",as.table = FALSE) +
  ggtitle(ptitle) +
  guides(color="none") +
  xlab(NULL) + ylab("Real Price (RP) | Real Earnings (RE) | Real Dividends Per Share (RDPS)")
emit(p,"spi_base_index_restatement")

# last 3 years
ptitle <- paste("S&P Composite Index","Current-Dollar CPI-Adjusted","Last 3 Years",plot_date,sep=' - ')
gdf <- base.df %>% 
  filter(Year >= (current_year-3)) %>%
  gather(Plot,Series,RP,RE,RDPS) %>% 
  na.omit
p <- ggplot(gdf,aes(x=Date,y=Series,color=as.factor(Plot))) +
  geom_line() +
  scale_y_continuous(breaks=pretty_breaks()) +  # normal direction
  facet_grid(Plot~.,scales="free_y",space="fixed",as.table = FALSE) +
  ggtitle(ptitle) +
  guides(color="none") +
  xlab(NULL) + ylab("Real Price (RP) | Real Earnings (RE) | Real Dividends Per Share (RDPS)")
emit(p,"spi_base_index_l3y")

# plot the extra per share data from S&P
ptitle <- paste("S&P Composite Index","Then-Year-Dollar","S&P Metrics Per Share",plot_date,sep=' - ')
gdf <- base.df %>% 
  gather(Plot,Series,DPS,SPS,BVPS,CXPS) %>% 
  na.omit
p <- ggplot(gdf,aes(x=Date,y=Series,color=as.factor(Plot))) +
  geom_line() +
  scale_y_continuous(breaks=pretty_breaks()) +  # normal direction
  facet_grid(Plot~.,scales="free_y",space="fixed",as.table = FALSE) +
  ggtitle(ptitle) +
  guides(color="none") +
  xlab(NULL) + ylab("Dividends (DPS) | Sales (SPS) | Book Value (BVPS) | Capex (CXPS) ($/share)")
emit(p,"spi_extra_index_restatement")


# reproduce the Shiller CAPE plots for calibration
ptitle <- paste("S&P Composite Index","S&P Restatement",plot_date,sep=' - ')
gdf <- base.df %>% 
  gather(Plot,Series,PE10,GS10) %>% 
  na.omit
p <- ggplot(gdf,aes(x=Date,y=Series,color=as.factor(Plot))) +
  geom_line() +
  scale_y_continuous(breaks=pretty_breaks()) +  # normal direction
  facet_grid(Plot~.,scales="free_y",space="fixed",as.table = FALSE) +
  ggtitle(ptitle) +
  guides(color="none") +
  xlab(NULL) + ylab("Cyclically-Adjusted PE Ratio (CAPE, P/E10) | Long-Term Interest Rates (GS10 %)")
emit(p,"spi_base_cape_reproduction")

# last 3 years
ptitle <- paste("S&P Composite Index","Last 3 Years",plot_date,sep=' - ')
gdf <- base.df %>% 
  filter(Year >= (current_year-3)) %>%
  gather(Plot,Series,PE10,GS10) %>% 
  na.omit
p <- ggplot(gdf,aes(x=Date,y=Series,color=as.factor(Plot))) +
  geom_line() +
  scale_y_continuous(breaks=pretty_breaks()) +  # normal direction
  facet_grid(Plot~.,scales="free_y",space="fixed",as.table = FALSE) +
  ggtitle(ptitle) +
  guides(color="none") +
  xlab(NULL) + ylab("Cyclically-Adjusted PE Ratio (CAPE, P/E10) | Long-Term Interest Rates (GS10 %)")
emit(p,"spi_base_cape_l3y")

# produce percentile charts
gdf <- base.df %>% mutate(Range="Normal")
gdf[with(gdf,is.na(PE10)==FALSE & PE10>30 & Year > 1995 & Year < 2005),'Range'] <- "Tech Bubble"
gdf[with(gdf,is.na(PE10)==FALSE & Year == 2007),'Range'] <- "2007 Crisis"

wdf <- gdf %>% filter(is.na(PE10)==FALSE) %>% filter(row_number()==n()) %>% mutate(Range="We are here")
wtext <- sprintf("We are here %d/%d = %0.1f",wdf$Month,wdf$Year,wdf$PE10)

# historical percentiles
ptitle <- paste("S&P Indices","PE10","[1988-Present] Percentile",plot_date,sep=' - ')
p <- ggplot(gdf,aes(x=PE10)) +
  stat_ecdf(geom="point",na.rm=TRUE,color="blue") +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() + 
  labs(title=ptitle,y="Percentile",x="Price-Earnings Ratio (CAPE, P/E10)") +
  geom_vline(xintercept=wdf$PE10,color="darkgray",linetype="dashed") +
  annotate("label",x=wdf$PE10,y=0.5,fill="white",color="black",label=wtext,vjust="middle",hjust="center")
emit(p,"spi_base_cape_percentile_total")

# historical percentiles by episode
ptitle <- paste("S&P Indices","PE10","[1988-Present] Episode Percentile",plot_date,sep=' - ')
p <- ggplot(gdf,aes(x=PE10,color=Range)) +
  stat_ecdf(na.rm=TRUE) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() + 
  labs(title=ptitle,y="Percentile",x="Price-Earnings Ratio (CAPE, P/E10)") +
  geom_vline(xintercept=wdf$PE10,color="darkgray",linetype="dashed") +
  annotate("label",x=wdf$PE10,y=0.5,fill="white",color="black",label=wtext,vjust="middle",hjust="center",alpha=0.5)
emit(p,"spi_base_cape_percentile_episodes")

# deviation from arithmetic mean
ptitle <- paste("S&P Indices","PE10","Deviation from Arithmetic Mean",plot_date,sep=' - ')
gdf <- base.df %>% filter(Type=="Actual") %>% select(Date,ADist) %>% na.omit
p <- ggplot(gdf,aes(x=Date,y=ADist)) +
  geom_line(color="blue") +
  scale_y_continuous(labels=percent_format(),breaks=pretty_breaks()) +
  labs(title=ptitle,x=NULL,y="Deviation from PE10 Arithmetic Mean (%)")
p <- apply_thresholds(p,gdf[1,'Date'],asd/amean)
p <- apply_current(p,gdf,geometric = FALSE)
p <- apply_extrema(p,arithmetic_extrema,geometric = FALSE)
emit(p,"pe10_deviation_arithmetic")

# deviation from geometric mean
# this version is correct because it computes the proper geometric SD
ptitle <- paste("S&P Indices","PE10","Deviation from Geometric Mean",plot_date,sep=' - ')
gdf <- base.df %>% filter(Type=="Actual") %>% select(Date,GDist) %>% na.omit
p <- ggplot(gdf,aes(x=Date,y=GDist)) +
  geom_line(color="blue") +
  scale_y_continuous(labels=percent_format(),breaks=pretty_breaks()) +
  labs(title=ptitle,x=NULL,y="Deviation from PE10 Geometric Mean (%)")
p <- apply_thresholds(p,gdf[1,'Date'],gsd/gmean)
p <- apply_current(p,gdf)
p <- apply_extrema(p,geometric_extrema)
emit(p,"pe10_deviation_geometric")

# deviation from geometric mean
# this version is bogus because it portrays the improper geometric SD distance using the arithmetic SD
ptitle <- paste("S&P Indices","PE10","Deviation from Geometric Mean (Repro)",plot_date,sep=' - ')
gdf <- base.df %>% filter(Type=="Actual") %>% select(Date,GDist) %>% na.omit
p <- ggplot(gdf,aes(x=Date,y=GDist)) +
  geom_line(color="blue") +
  scale_y_continuous(labels=percent_format(),breaks=pretty_breaks()) +
  labs(title=ptitle,x=NULL,y="Deviation from PE10 Geometric Mean (Arithmetic Deviation %)")
p <- apply_thresholds(p,gdf[1,'Date'],asd/gmean)
p <- apply_current(p,gdf)
p <- apply_extrema(p,geometric_extrema)
emit(p,"pe10_deviation_geometric_repro")

# show the expected earnings
ptitle <- paste("S&P Indices","Reported Earnings","Expected Earnings", plot_date,sep=' - ')
p <- ggplot(base.df %>% select(Date,Type,Rep.Earnings) %>% na.omit,aes(x=Date,y=Rep.Earnings,color=Type)) +
  geom_line() +
  scale_y_continuous(labels=dollar_format(),breaks=pretty_breaks()) +
  labs(title=ptitle,x=NULL,y="Reported (Then-Year $) and Expected Earnings ($)")
emit(p,"spi_expected_earnings")

# TODO add sector equivalents from the spreadsheets
# < end >
