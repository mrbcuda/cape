###########################################################################
#' CAPE Analysis
#' @author Matt Barry, Mike Sadler
#' @details Provides time history for S&P cyclically-adjusted P/E
#' @concepts CAPE, P/E
#' @seealso http://us.spindices.com/indices/equity/sp-500
###########################################################################
ignore <- lapply(c("ggplot2","scales","tidyr","dplyr","stringr","magrittr","lubridate","EnvStats","directlabels","grid"),
                 require,quietly=FALSE,character.only=TRUE)
options("getSymbols.warning4.0"=FALSE)
options(digits=4,width=100,scipen=100)
plot_date = format(Sys.time(), "%b %d, %Y")
direct_method = "last.bumpup"
shiller_data = "data/shiller_data.xls" # original unmodified data from Yale
base_data = "data/base_data.csv" # extracted data massaged to CSV, through 2015


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
                 prefix="cape_",
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

# plot the original Shiller data, up to end of 2015
base.df <- read.csv(base_data)
base.df$DateString <- sprintf("%s-15-%s",base.df$Month,base.df$Year)
base.df$Date <- mdy(base.df$DateString)

# reproduce the Shiller index plots for calibration
ptitle <- paste("Shiller","PE10","Reproduction",plot_date,sep=' - ')
gdf <- base.df %>% 
  gather(Plot,Series,RP,RE) %>% 
  na.omit
p <- ggplot(gdf,aes(x=Date,y=Series,color=as.factor(Plot))) +
  geom_line() +
  scale_y_continuous(breaks=pretty_breaks()) +  # normal direction
  facet_grid(Plot~.,scales="free_y",space="fixed",as.table = FALSE) +
  ggtitle(ptitle) +
  guides(color="none") +
  xlab(NULL) + ylab("Real S&P Composite Index Price (RP $) and Real S&P Composite Earnings (RE $/Share)")
emit(p,"shiller_base_index_reproduction")

# reproduce the Shiller CAPE plots for calibration
ptitle <- paste("Shiller","PE10","Reproduction",plot_date,sep=' - ')
gdf <- base.df %>% 
  gather(Plot,Series,PE10,GS10) %>% 
  na.omit
p <- ggplot(gdf,aes(x=Date,y=Series,color=as.factor(Plot))) +
  geom_line() +
  scale_y_continuous(breaks=pretty_breaks()) +  # normal direction
  facet_grid(Plot~.,scales="free_y",space="fixed",as.table = FALSE) +
  ggtitle(ptitle) +
  guides(color="none") +
  xlab(NULL) + ylab("Price-Earnings Ratio (CAPE, P/E10) and Long-Term Interest Rates (GS10 %)")
emit(p,"shiller_base_cape_reproduction")

# produce percentile charts
gdf <- base.df %>% mutate(Range="Normal")
gdf[with(gdf,is.na(PE10)==FALSE & PE10>30 & Year > 1995 & Year < 2005),'Range'] <- "Tech Bubble"
gdf[with(gdf,is.na(PE10)==FALSE & Year == 1929),'Range'] <- "1929 Crisis"
gdf[with(gdf,is.na(PE10)==FALSE & Year == 2007),'Range'] <- "2007 Crisis"

wdf <- gdf %>% filter(is.na(PE10)==FALSE) %>% filter(row_number()==n()) %>% mutate(Range="We are here")
wtext <- sprintf("We are here %d/%d",wdf$Month,wdf$Year)
# gdf <- bind_rows(gdf,wdf) %>% arrange(Year,Month)
# rm(wdf)

ptitle <- paste("Shiller","PE10","Historical Percentile",plot_date,sep=' - ')
p <- ggplot(gdf,aes(x=PE10)) +
  stat_ecdf(geom="point",na.rm=TRUE,color="blue") +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() + 
  ggtitle(ptitle) +
  ylab("Percentile") + 
  xlab("Price-Earnings Ratio (CAPE, P/E10)") +
  annotate("text",x=wdf$PE10,y=0.5,color="black",label=wtext,vjust=0,hjust=0) +
  geom_vline(xintercept=wdf$PE10,color="darkgray",linetype="dashed")
emit(p,"shiller_base_cape_percentile_total")

# percentiles by episode
ptitle <- paste("Shiller","PE10","Historical Episode Percentile",plot_date,sep=' - ')
p <- ggplot(gdf,aes(x=PE10,color=Range)) +
  stat_ecdf(na.rm=TRUE) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() + 
  ggtitle(ptitle) +
  ylab("Percentile") + 
  xlab("Price-Earnings Ratio (CAPE, P/E10)") +
  annotate("text",x=wdf$PE10,y=0.5,color="black",label=wtext,vjust=0,hjust=0) +
  geom_vline(xintercept=wdf$PE10,color="darkgray",linetype="dashed")
emit(p,"shiller_base_cape_percentile_episodes")

# historical means
amean <- mean(base.df$PE10,na.rm=TRUE)
asd <- sd(base.df$PE10,na.rm=TRUE)
gmean <- geoMean(base.df$PE10,na.rm=TRUE)
gsd <- geoSD(base.df$PE10,na.rm=TRUE)


base.df %<>% na.omit %>% mutate(ADist=(PE10-amean)/amean) %>% mutate(GDist=(PE10-gmean)/gmean)
gdf <- base.df

inflection <- function(i) {
  sprintf("%.0f%%",i*100)
}

ptitle <- paste("Shiller","PE10","Deviation from Arithmetic Mean",plot_date,sep=' - ')
asd_dist <- asd / amean
peak1 <- gdf %>% filter(Year>1900 & Year < 1905) %>% arrange(desc(ADist)) %>% filter(row_number() == 1)
peak2 <- gdf %>% filter(Year>1925 & Year < 1935) %>% arrange(desc(ADist)) %>% filter(row_number() == 1)
peak3 <- gdf %>% filter(Year>1962 & Year < 1968) %>% arrange(desc(ADist)) %>% filter(row_number() == 1)
peak4 <- gdf %>% filter(Year>1998 & Year < 2002) %>% arrange(desc(ADist)) %>% filter(row_number() == 1)
trof1 <- gdf %>% filter(Year>1920 & Year < 1925) %>% arrange(ADist) %>% filter(row_number() == 1)
trof2 <- gdf %>% filter(Year>1930 & Year < 1935) %>% arrange(ADist) %>% filter(row_number() == 1)
trof3 <- gdf %>% filter(Year>1980 & Year < 1985) %>% arrange(ADist) %>% filter(row_number() == 1)
trof4 <- gdf %>% filter(Year>2006 & Year < 2010) %>% arrange(ADist) %>% filter(row_number() == 1)

p <- ggplot(gdf,aes(x=Date,y=ADist)) +
  geom_line(color="blue") +
  scale_y_continuous(labels=percent_format(),breaks=pretty_breaks()) +
  ggtitle(ptitle) +
  geom_hline(yintercept=-1*asd_dist,linetype="dashed",color="red") +
  geom_hline(yintercept= 0*asd_dist,linetype="solid",color="red") +
  geom_hline(yintercept= 1*asd_dist,linetype="dashed",color="red") +
  geom_hline(yintercept= 2*asd_dist,linetype="dashed",color="red") +
  geom_hline(yintercept= 3*asd_dist,linetype="dashed",color="red") +
  annotate(geom="text",x=gdf[1,'Date'],y=-1*asd_dist,color="red",label="-1SD",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 0*asd_dist,color="red",label="Mean",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 1*asd_dist,color="red",label="+1SD",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 2*asd_dist,color="red",label="+2SD",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 3*asd_dist,color="red",label="+3SD",vjust=1) +
  annotate(geom="text",x=peak1$Date,y=peak1$ADist,vjust=-1,color="green",label=inflection(peak1$ADist)) +
  annotate(geom="text",x=peak2$Date,y=peak2$ADist,vjust=-1,color="green",label=inflection(peak2$ADist)) +
  annotate(geom="text",x=peak3$Date,y=peak3$ADist,vjust=-1,color="green",label=inflection(peak3$ADist)) +
  annotate(geom="text",x=peak4$Date,y=peak4$ADist,vjust=-1,color="green",label=inflection(peak4$ADist)) +
  annotate(geom="text",x=trof1$Date,y=trof1$ADist,vjust=2,color="orange",label=inflection(trof1$ADist)) +
  annotate(geom="text",x=trof2$Date,y=trof2$ADist,vjust=2,color="orange",label=inflection(trof2$ADist)) +
  annotate(geom="text",x=trof3$Date,y=trof3$ADist,vjust=2,color="orange",label=inflection(trof3$ADist)) +
  annotate(geom="text",x=trof4$Date,y=trof4$ADist,vjust=2,color="orange",label=inflection(trof4$ADist)) +
  annotate(geom="text",x=gdf[nrow(gdf),'Date'],y=gdf[nrow(gdf),'ADist'],
           vjust=ifelse(gdf[nrow(gdf),'ADist']>0,-1,2),color="black",label=inflection(gdf[nrow(gdf),'ADist'])) +
  xlab(NULL) + ylab("Deviation from PE10 Arithmetic Mean (%)")
emit(p,"pe10_deviation_arithmetic")

# this version is correct because it computes the proper geometric SD
ptitle <- paste("Shiller","PE10","Deviation from Geometric Mean",plot_date,sep=' - ')
gsd_dist <- gsd / gmean
peak1 <- gdf %>% filter(Year>1900 & Year < 1905) %>% arrange(desc(GDist)) %>% filter(row_number() == 1)
peak2 <- gdf %>% filter(Year>1925 & Year < 1935) %>% arrange(desc(GDist)) %>% filter(row_number() == 1)
peak3 <- gdf %>% filter(Year>1962 & Year < 1968) %>% arrange(desc(GDist)) %>% filter(row_number() == 1)
peak4 <- gdf %>% filter(Year>1998 & Year < 2002) %>% arrange(desc(GDist)) %>% filter(row_number() == 1)
trof1 <- gdf %>% filter(Year>1920 & Year < 1925) %>% arrange(GDist) %>% filter(row_number() == 1)
trof2 <- gdf %>% filter(Year>1930 & Year < 1935) %>% arrange(GDist) %>% filter(row_number() == 1)
trof3 <- gdf %>% filter(Year>1980 & Year < 1985) %>% arrange(GDist) %>% filter(row_number() == 1)
trof4 <- gdf %>% filter(Year>2006 & Year < 2010) %>% arrange(GDist) %>% filter(row_number() == 1)
p <- ggplot(gdf,aes(x=Date,y=GDist)) +
  geom_line(color="blue") +
  scale_y_continuous(labels=percent_format(),breaks=pretty_breaks()) +
  ggtitle(ptitle) +
  geom_hline(yintercept=-1*gsd_dist,linetype="dashed",color="red") +
  geom_hline(yintercept= 0*gsd_dist,linetype="solid",color="red") +
  geom_hline(yintercept= 1*gsd_dist,linetype="dashed",color="red") +
  geom_hline(yintercept= 2*gsd_dist,linetype="dashed",color="red") +
  geom_hline(yintercept= 3*gsd_dist,linetype="dashed",color="red") +
  annotate(geom="text",x=gdf[1,'Date'],y=-1*gsd_dist,color="red",label="-1SD",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 0*gsd_dist,color="red",label="Mean",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 1*gsd_dist,color="red",label="+1SD",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 2*gsd_dist,color="red",label="+2SD",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 3*gsd_dist,color="red",label="+3SD",vjust=1) +
  annotate(geom="text",x=peak1$Date,y=peak1$GDist,vjust=-1,color="green",label=inflection(peak1$GDist)) +
  annotate(geom="text",x=peak2$Date,y=peak2$GDist,vjust=-1,color="green",label=inflection(peak2$GDist)) +
  annotate(geom="text",x=peak3$Date,y=peak3$GDist,vjust=-1,color="green",label=inflection(peak3$GDist)) +
  annotate(geom="text",x=peak4$Date,y=peak4$GDist,vjust=-1,color="green",label=inflection(peak4$GDist)) +
  annotate(geom="text",x=trof1$Date,y=trof1$GDist,vjust=2,color="orange",label=inflection(trof1$GDist)) +
  annotate(geom="text",x=trof2$Date,y=trof2$GDist,vjust=2,color="orange",label=inflection(trof2$GDist)) +
  annotate(geom="text",x=trof3$Date,y=trof3$GDist,vjust=2,color="orange",label=inflection(trof3$GDist)) +
  annotate(geom="text",x=trof4$Date,y=trof4$GDist,vjust=2,color="orange",label=inflection(trof4$GDist)) +
  annotate(geom="text",x=gdf[nrow(gdf),'Date'],y=gdf[nrow(gdf),'GDist'],
           vjust=ifelse(gdf[nrow(gdf),'GDist']>0,-1,2),color="black",label=inflection(gdf[nrow(gdf),'GDist'])) +
  xlab(NULL) + ylab("Deviation from PE10 Geometric Mean (%)")
emit(p,"pe10_deviation_geometric")

# this version is bogus because it portrays the improper geometric SD distance using the arithmetic SD
gsd_dist <- asd / gmean
ptitle <- paste("Shiller","PE10","Deviation from Geometric Mean (Repro)",plot_date,sep=' - ')
p <- ggplot(gdf,aes(x=Date,y=GDist)) +
  geom_line(color="blue") +
  scale_y_continuous(labels=percent_format(),breaks=pretty_breaks()) +
  ggtitle(ptitle) +
  geom_hline(yintercept=-1*gsd_dist,linetype="dashed",color="red") +
  geom_hline(yintercept= 0*gsd_dist,linetype="solid",color="red") +
  geom_hline(yintercept= 1*gsd_dist,linetype="dashed",color="red") +
  geom_hline(yintercept= 2*gsd_dist,linetype="dashed",color="red") +
  geom_hline(yintercept= 3*gsd_dist,linetype="dashed",color="red") +
  annotate(geom="text",x=gdf[1,'Date'],y=-1*gsd_dist,color="red",label="-1SD",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 0*gsd_dist,color="red",label="Mean",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 1*gsd_dist,color="red",label="+1SD",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 2*gsd_dist,color="red",label="+2SD",vjust=1) +
  annotate(geom="text",x=gdf[1,'Date'],y= 3*gsd_dist,color="red",label="+3SD",vjust=1) +
  annotate(geom="text",x=peak1$Date,y=peak1$GDist,vjust=-1,color="green",label=inflection(peak1$GDist)) +
  annotate(geom="text",x=peak2$Date,y=peak2$GDist,vjust=-1,color="green",label=inflection(peak2$GDist)) +
  annotate(geom="text",x=peak3$Date,y=peak3$GDist,vjust=-1,color="green",label=inflection(peak3$GDist)) +
  annotate(geom="text",x=peak4$Date,y=peak4$GDist,vjust=-1,color="green",label=inflection(peak4$GDist)) +
  annotate(geom="text",x=trof1$Date,y=trof1$GDist,vjust=2,color="orange",label=inflection(trof1$GDist)) +
  annotate(geom="text",x=trof2$Date,y=trof2$GDist,vjust=2,color="orange",label=inflection(trof2$GDist)) +
  annotate(geom="text",x=trof3$Date,y=trof3$GDist,vjust=2,color="orange",label=inflection(trof3$GDist)) +
  annotate(geom="text",x=trof4$Date,y=trof4$GDist,vjust=2,color="orange",label=inflection(trof4$GDist)) +
  annotate(geom="text",x=gdf[nrow(gdf),'Date'],y=gdf[nrow(gdf),'GDist'],
           vjust=ifelse(gdf[nrow(gdf),'GDist']>0,-1,2),color="black",label=inflection(gdf[nrow(gdf),'GDist'])) +
  xlab(NULL) + ylab("Deviation from PE10 Geometric Mean (%)")
emit(p,"pe10_deviation_geometric_repro")


