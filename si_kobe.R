############################################################
## Function purpose: Kobe plot function from Costello et al
## Date: 12/2/2016
## Author: Tyler Clavelle
###########################################################
# data <- global
# zone <- 'Global'
# library(MASS)

si_kobe <- function(data, data_u, zone = 'Global', year = 2012, summary_points = F, focus_cntry = F){
# zone<-c('Global')

names<-data.frame(zone,stringsAsFactors=F) # df of names and corresponding codes for every FAO major region

if(focus_cntry ==F) {pdf(file=paste('Global Kobe.pdf',sep=''))}
if(focus_cntry !=F) {pdf(file=paste('Global Kobe ', focus_cntry,' Focus.pdf',sep=''))}

par(mfrow=c(1,1),mar=c(.1,.1,.1,.1),oma=c(4,4,1,1))

for(a in 1:1) # only loop over global
{
  if(zone[a]=='Global') 
  {
    temp <- data
    
    tempName <- zone
  }
  
  if(zone[a]!='Global')
  {
    temp<-data[data$Year==year & data$Country==zone,]
    
    tempName<-zone
  }
  
  if(nrow(temp)>0)
  {
    
    if (any(temp$BvBmsy<0,na.rm=T)){temp$BvBmsy<- exp(temp$BvBmsy)}
    
    BaselineData<- subset(temp,subset=Year==year & is.na(FvFmsy)==F & is.na(BvBmsy)==F)
    
    KobeMedians<- BaselineData %>%
      mutate(bxcatch=BvBmsy*Catch,fxcatch=FvFmsy*Catch) %>%
      group_by(Year) %>%
      summarize(MedianB=median(BvBmsy,na.rm=T),MedianF=median(FvFmsy,na.rm=T),
                WtMeanB=sum(bxcatch,na.rm=T)/sum(Catch,na.rm=T),
                WtMeanF=sum(fxcatch,na.rm=T)/sum(Catch,na.rm=T),
                WtGeomMeanB=exp(sum(MSY * log(BvBmsy),na.rm=T)/sum(MSY,na.rm=T)),
                WtGeomMeanF=exp(sum(MSY * log(FvFmsy + 1e-3),na.rm=T)/sum(MSY,na.rm=T)))
    
    BaselineData$FvFmsy[BaselineData$FvFmsy>4]<- 4
    
    BaselineData$BvBmsy[BaselineData$BvBmsy>2.5]<- 2.5
    
    DensityData<- (kde2d(BaselineData$BvBmsy,BaselineData$FvFmsy,n=100,lims=c(0,2.5,0,4)))
    
    FTrend<- temp %>% 
      group_by(Year,Dbase) %>% 
      summarize(FTrend=mean(FvFmsy,na.rm=T))
    
    KobeColors<- colorRampPalette(c("powderblue", "white",'#FFFF99'))
    
    DbaseColors<- colorRampPalette(c('Red','Black','Green'))
    
    image(DensityData$x,DensityData$y,DensityData$z,xlim=c(0,2.5),ylim=c(0,4),col=KobeColors(50),xaxt='n',yaxt='n',bty='o',las=1)
    
    axis(side=2,at=seq(1,3),las=1)      
    axis(side=1,at=seq(0,2,.5),las=1)
    mtext(outer=T,side=1,expression(B/B[MSY]),line=2)
    mtext(outer=T,side=2,expression(F/F[MSY]),line=2)
    
    
    inCols<-as.numeric(as.factor(BaselineData$Dbase))		# inCols should be the current colors
    OutCols<-inCols
    
    # RGBcols	<-c("#000000","#FF0000","#00CC00") #black, red, green
    # RGBcols<-c('#A0A0A0','#FC6969','#0B610B')
    RGBcols<-c('#000000','#FF0000','#0B610B')
    
    
    AddTrans	<-99		# adjusts the transparency--lower is more transparent
    
    if(focus_cntry != F) {AddTrans <- 10}
    
    for(x in 1:3)
      OutCols[inCols==x]<-paste(RGBcols[x],AddTrans,sep="")
    
    points(BaselineData$BvBmsy,BaselineData$FvFmsy,pch=16,col=OutCols,cex=0.75+(BaselineData$Catch)/max(BaselineData$Catch,na.rm=T))
    points(BaselineData$BvBmsy,BaselineData$FvFmsy,pch=1,col=OutCols,cex=0.75+(BaselineData$Catch)/max(BaselineData$Catch,na.rm=T))
    
    if(focus_cntry != F){
      
      focus <- data_u %>%
        filter(Country %in% focus_cntry & Year == year)
      
      BaselineData <- subset(BaselineData, !(Country %in% focus_cntry))
      
      OutCols[inCols==x]<-paste(RGBcols[x],99,sep="")
      
      points(focus$BvBmsy,focus$FvFmsy,pch=16,col=OutCols,cex=0.75+(focus$Catch)/max(focus$Catch,na.rm=T))
      points(focus$BvBmsy,focus$FvFmsy,pch=1,col=OutCols,cex=0.75+(focus$Catch)/max(focus$Catch,na.rm=T))
      
    }
    
    if(summary_points == T){
      AddTrans	<-99		# adjusts the transparency--lower is more transparent
    points(KobeMedians$MedianB,KobeMedians$MedianF,pch=17,col=RGBcols[3], cex=1.5)
    points(KobeMedians$WtGeomMeanB,KobeMedians$WtGeomMeanF,pch=15,col=RGBcols[3], cex=1.5)
    }
    
    box()
    abline(h=1,v=1,lty=2,lwd=3)
    if(focus_cntry != F) {tempName <- focus_cntry}
    legend("topright",tempName,bty='n')
    # legend('center',legend=unique(BaselineData$Dbase),pch=16,col=(as.factor(unique(BaselineData$Dbase))),cex=0.7,bty='n') 
  } 
  #     show(a)
}

dev.off()
}

