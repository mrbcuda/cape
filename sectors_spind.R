###########################################################################
#' Sector performance presentation from S&P Index Data
#' @author Matt Barry
#' @details Provides time history for S&P index sector data
#' @concepts EPS
#' @seealso http://us.spindices.com/indices/equity/sp-500
###########################################################################
ignore <- lapply(c("quantmod","ggplot2","scales","tidyr","dplyr","stringr","lubridate","magrittr"),
                 require,quietly=FALSE,character.only=TRUE)
options("getSymbols.warning4.0"=FALSE)
options(digits=4,width=100,scipen=100)
plot_date = format(Sys.time(), "%b %d, %Y")
current_year <- year(Sys.time())
direct_method = "last.bumpup"
base_data = "data/sp-sec-eps-est-df.csv" # extracted data massaged to CSV

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
#' @param prefix plot group name prefix, default 'spsec_'
#' @param suffix plot name suffix, default '.png'
#' @param showWarnings whether to show warnings on directory creation, passed to dir.create(), default FALSE
#' @param recursive whether to create path elements other than last, passed to dir.create(), default TRUE
#' @param width plot image width in pixels, default 2014
#' @param height plot image height in pixels, default 768
#' @return nothing
emit <- function(p,
                 tag='uknnown',
                 path="plots/",
                 prefix="spsec_",
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



# massaged S&P indices sector data
base.df <- read.csv(base_data,check.names=FALSE) 

for ( i in c(400,500,600) ) {
  # start with composite index
  spx <- base.df %>% 
    filter(Index==i & Sector %in% "Index") %>% 
    gather(Quarter,Value,contains("Q")) %>% 
    mutate(Date=as.Date(as.yearqtr(Quarter)))
  
  ptitle <- paste("S&P Indices","Sector Data",paste("S&P",i,"Composite Index",sep=' '),plot_date,sep=' - ')
  p <- ggplot(spx %>% na.omit,aes(x=Date,y=Value,color=EPS)) +
    geom_line() +
    scale_y_continuous(labels=dollar_format(),breaks=pretty_breaks()) +  
    ggtitle(ptitle) +
    xlab(NULL) + ylab("Quarterly Earnings per Share (EPS)")
  emit(p,paste(i,"index","eps",sep='_'))
  
  # finish with facets by S&P 500 sector
  spsec <- base.df %>% 
    filter(Index==i & Sector %nin% "Index") %>% 
    gather(Quarter,Value,contains("Q")) %>% 
    mutate(Date=as.Date(as.yearqtr(Quarter)))
  
  ptitle <- paste("S&P Indices","Sector Data",paste("S&P",i,"Sectors",sep=' '),plot_date,sep=' - ')
  p <- ggplot(spsec %>% na.omit,aes(x=Date,y=Value,color=EPS)) +
    geom_line() +
    facet_wrap("Sector",ncol=2,scales="free_y",as.table = FALSE) +
    scale_y_continuous(labels=dollar_format(),breaks=pretty_breaks()) +  
    ggtitle(ptitle) +
    xlab(NULL) + ylab("Quarterly Earnings per Share (EPS)")
  emit(p,paste(i,"sector","eps",sep='_'))
}

i <- NULL

# all composite indices
# start with composite index
spx <- base.df %>% 
  filter(Sector %in% "Index") %>% 
  filter(EPS %in% 'Reported') %>%
  gather(Quarter,Value,contains("Q")) %>% 
  mutate(Date=as.Date(as.yearqtr(Quarter))) %>%
  mutate(QOQ=NA)

# build QOQ series
smini <- data.frame()
for ( i in unique(spx$Index)) {
  smini <- bind_rows(smini,spx %>% filter(Index==i) %>% mutate(QOQ=Value/lag(Value)-1))
}

spx <- smini %>%
  mutate(Index=sprintf("S&P-%d",Index)) %>%
  mutate(Index=factor(Index))
smini <- NULL

# absolute values
ptitle <- paste("S&P Indices","Reported Earnings","Composite Indices",plot_date,sep=' - ')
p <- ggplot(spx %>% na.omit,aes(x=Date,y=Value,color=Index)) +
  geom_line() +
  scale_y_continuous(labels=dollar_format(),breaks=pretty_breaks()) +  
  ggtitle(ptitle) +
  xlab(NULL) + ylab("Quarterly Earnings per Share (EPS)")
emit(p,"all_index_eps")

# quarter-over-quarter
ptitle <- paste("S&P Indices","Reported Earnings QOQ","Composite Indices",plot_date,sep=' - ')
p <- ggplot(spx %>% filter(year(Date)>2009) %>% na.omit,aes(x=Date,y=QOQ,color=Index)) +
  geom_line() +
  scale_y_continuous(labels=percent_format(),breaks=pretty_breaks()) +
  geom_hline(yintercept=0,color="gray",linetype="dashed") +
  ggtitle(ptitle) +
  xlab(NULL) + ylab("Quarterly Earnings per Share Change Q-O-Q (%)")
emit(p,"all_index_eps_qoq")


# finish with individual sectors
spsec <- base.df %>% 
  filter(Sector %nin% "Index") %>% 
  filter(EPS %in% 'Reported') %>%
  gather(Quarter,Value,contains("Q")) %>% 
  mutate(Date=as.Date(as.yearqtr(Quarter)))

# build QOQ series
smini <- data.frame()
for ( i in unique(spsec$Sector)) {
  smini <- bind_rows(smini,spsec %>% filter(Sector %in% i) %>% mutate(QOQ=Value/lag(Value)-1))
}

spsec <- smini %>%
  mutate(Index=sprintf("S&P-%d",Index)) %>%
  mutate(Index=factor(Index))
smini <- NULL

ptitle <- paste("S&P Indices","Reported Earnings","Sector Data",plot_date,sep=' - ')
p <- ggplot(spsec %>% na.omit,aes(x=Date,y=Value,color=Index)) +
  geom_line() +
  facet_wrap("Sector",ncol=2,scales="free_y",as.table = FALSE) +
  scale_y_continuous(labels=dollar_format(),breaks=pretty_breaks()) +  
  ggtitle(ptitle) +
  xlab(NULL) + ylab("Quarterly Earnings per Share (EPS)")
emit(p,"all_sector_eps")

# quarter-over-quarter
ptitle <- paste("S&P Indices","Reported Earnings QOQ","Sector Data",plot_date,sep=' - ')
p <- ggplot(spsec %>% filter(year(Date)>2009) %>% na.omit,aes(x=Date,y=QOQ,color=Sector)) +
  geom_line() +
  scale_y_continuous(labels=percent_format(),breaks=pretty_breaks()) +
  facet_grid(Index~.,scales="free_y",as.table=FALSE) +
  geom_hline(yintercept=0,color="gray",linetype="dashed") +
  ggtitle(ptitle) +
  xlab(NULL) + ylab("Quarterly Earnings per Share Change Q-O-Q (%)")
emit(p,"all_sector_eps_qoq")

# reported by sector and index
for ( s in unique(spsec$Sector) ) {
  ptitle <- paste("S&P Indices","Reported Earnings",s,plot_date,sep=' - ')
  p <- ggplot(spsec %>% filter(Sector %in% s) %>% filter(year(Date)>2009) %>% na.omit,aes(x=Date,y=Value,color=Index)) +
    geom_line() +
    scale_y_continuous(labels=dollar_format(),breaks=pretty_breaks()) +
    ggtitle(ptitle) +
    xlab(NULL) + ylab("Quarterly Earnings per Share (EPS)")
  sp <- str_to_lower(gsub(' ','_',s,fixed=TRUE))
  emit(p,paste("sector_eps",sp,sep='_'))
}


# < end >
