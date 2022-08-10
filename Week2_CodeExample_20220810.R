#For guidance, Nature's standard figure sizes are 89 mm wide (single column) and 183 mm wide (double column). 
#The full depth of a Nature page is 247 mm. Figures can also be a column-and-a-half where necessary (120-136 mm).


###########
#plot WM SP by lat long with mangrove 
############
if(TRUE){
  rm(list = ls())
  library(rworldmap)
  library(rgdal)
  
  worldmap <- getMap(resolution = "coarse")
  
  wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangroveModel/")
  data0 <- read.csv(paste0(wd,'result/landingWithVar.csv'),stringsAsFactors = FALSE)  
  
  lds <- unique(data0$landing)
  
  data <- data.frame()
  for(ll in 1:length(lds)){
    data <- rbind(data,data0[which(data0$landing==lds[ll]),][1,])  
  }
  
  #data <- data[which(data$LAT>=-30 & data$LAT<=30),]
  
  basins <- read.csv(paste0(wd,'data/basins/basinsPolygon.csv'),stringsAsFactors = FALSE)
  colnames(basins)[1]<-'Region'
  regList <- unique(basins$Region)
  
  #prepare data
  if(TRUE){
    data$landingLon2 <- data$landingLon
    for (rr in 1:nrow(data)){
      if(data[rr,'landingLon']<0){data[rr,'landingLon2']<-data[rr,'landingLon2']+360}
    }
    
    
    bsn <- unique(data$Basin)
    bsn <- c("NInd","NWPac","NEPac", "NAtl", "SInd", "SWPac")
    pchs <- c("NI","NW","NE", "NA", "SI", "SW")
    #cols <- c('orange','darkgreen','blue','red','purple','yellow2')
    cols <- rep('black',6)
    
    data$col <- NA
    data$pch <- NA
    for (b in 1:length(bsn)){
      data[which(data$basin==bsn[b]),'col'] <- cols[b]
      data[which(data$basin==bsn[b]),'pch'] <- pchs[b]
    }
  }
  
  #format
  if(TRUE){
    ylm <- c(0.1,0.3)  
    mrk_tck_y <- seq(0.1,0.3,0.05)
    mrk_lbl_y <- seq(0.1,0.3,0.1)
    
    wmcut <- 156
    spcut <- 15
    
    alpha <- 0.5
    pch <- 21
    cex_pch <- 0.5
    
    ln_width <- 1.5
    ln_width95 <- 1
    ln_type_pred <- 1
    ln_type_95 <- 3
    
    col_text <- rgb(2, 17, 94, maxColorValue = 255)
    col_mng <- 'darkgreen'
    cex_panel <- 0.9
    cex_note <- 0.8
    cex_axs <- 0.8
    cex_lab <- 0.8
    
    axis_tck1 <- -0.03
    axis_tck2 <- -0.03
    axis_mrkLine_1 <- -0.8
    axis_mrkLine_2 <- -0.8
    axis_lblLine_1 <- 1
    axis_lblLine_2 <- 1.5
  }
  
  
  #plot
  if(TRUE){
    tiff(file = paste0(wd,'result/figure/','landingLatLon2.tif'), width = 143 , height= 70, units = 'mm', res=300) 
    par(oma=c(0.85,0,0.85,0),mar=c(0,0,0,0),xpd=FALSE) 
    layout(matrix(c(5,3,3,9,
                    4,1,2,6,
                    4,1,2,6,
                    10,7,7,8), 4, 4, byrow = TRUE),widths = c(0.6,1.55,1.3,0.6),heights = c(1.1,1,1,1.1))
    #map
    if(TRUE){
      plot(worldmap,  col="lightgrey",border = 'lightgrey', bg="white", ylim=c(-40,40),xlim=c(25,180),xaxs="i",yaxs="i")
      for(reg in c( "NInd","SInd" , "NWPac")){
        #reg <- "NInd"
        reg_shp <- readOGR(dsn=paste0(wd,'data/basins/basinsPJ'),paste0(reg))
        plot(reg_shp,add=TRUE,border='darkgrey',lty=1,lwd=0.6)
        mng_shp <- readOGR(dsn=paste0(wd,'data/mangrove/byBasinBuffer'),paste0(reg,'Buffer'))
        plot(mng_shp,add=TRUE,col=adjustcolor(col_mng,alpha.f = 0.2),border=NA,lty=1,lwd=1)
      }
      
      for(reg in c( "SWPac1")){
        #reg <- "NInd"
        mng_shp <- readOGR(dsn=paste0(wd,'data/mangrove/byBasinBuffer'),paste0(reg,'Buffer'))
        plot(mng_shp,add=TRUE,col=adjustcolor(col_mng,alpha.f = 0.2),border=NA,lty=1,lwd=1)
      }
      
      #points(data$landingLon,data$landingLat,pch=21,col='black',lwd=0.5,bg=adjustcolor(data$col,alpha.f = alpha)) 
      text(data$landingLon,data$landingLat,data$pch,cex=cex_pch,col='black') 
      text(35,35,'N Ind',cex=cex_note,pos=4)
      text(100,35,'NW Pac',cex=cex_note,pos=4)
      text(50,-35,'S Ind',cex=cex_note)
      text(150,-35,'S Pac',cex=cex_note)
      
      plot(worldmap,  col="lightgrey", border = 'lightgrey',bg="white", ylim=c(-40,40),xlim=c(-180,-50),xaxs="i",yaxs="i")
      for(reg in c( "SAtl",  "NAtl",  "NEPac" )){
        reg_shp <- readOGR(dsn=paste0(wd,'data/basins/basinsPJ'),paste0(reg))
        plot(reg_shp,add=TRUE,border='darkgrey',lty=1,lwd=0.6)
        mng_shp <- readOGR(dsn=paste0(wd,'data/mangrove/byBasinBuffer'),paste0(reg,'Buffer'))
        plot(mng_shp,add=TRUE,col=adjustcolor(col_mng,alpha.f = 0.2),border=NA,lty=1,lwd=1)
      }
      for(reg in c( "SWPac2")){
        #reg <- "SWPac2"
        mng_shp <- readOGR(dsn=paste0(wd,'data/mangrove/byBasinBuffer'),paste0(reg,'Buffer'))
        plot(mng_shp,add=TRUE,col=adjustcolor(col_mng,alpha.f = 0.2),border=NA,lty=1,lwd=1)
      }
      #points(data$landingLon,data$landingLat,pch=21,col='black',lwd=0.5,bg=adjustcolor(data$col,alpha.f = alpha)) 
      text(data$landingLon,data$landingLat,data$pch,cex=cex_pch,col='black') 
      text(-160,35,'NE Pac',cex=cex_note,pos=4)
      text(-80,35,'N Atl',cex=cex_note,pos=4)
      
    }
    
    #windmax
    if(TRUE){
      #top
      plot(data$landingLon2,data$landingWindMax,xlim=c(25,310),ylim=c(0,300),
           ylab='',xlab='',yaxt="n",xaxt="n",xaxs="i",yaxs="i",frame=FALSE,
           pch=pch,col=NA,lwd=0.5,bg=NA)
      text(data$landingLon2,data$landingWindMax,data$pch,cex=cex_pch)
      
      axis(2, at=seq(0,300,50), labels=FALSE, tck=axis_tck2,  pos=25)
      arrows(25,wmcut,310,wmcut,length=0.00,angle=90,code=3,lty=3,lwd=1)
      axis(1, at=c(25,310), labels=FALSE, tck=0,  pos=0)
      axis(1, at=seq(30,330,30), labels=FALSE, tck=axis_tck1,  pos=0)
      axis(1,at=c(90,180,270),labels=c('90º E',180,'90º W'),lwd=0,line=axis_mrkLine_1,cex.axis=cex_axs)
      #left
      plot(data$landingWindMax,data$landingLat,xlim=c(390,0),ylim=c(-43,43),
           ylab='',xlab='',yaxt="n",xaxt="n",xaxs="i",yaxs="i",frame=FALSE,
           pch=data$pch,col=NA,lwd=0.5,bg=NA)
      text(data$landingWindMax,data$landingLat,data$pch,cex=cex_pch)
      arrows(wmcut,-43,wmcut,43,length=0.00,angle=90,code=3,lty=3,lwd=1)
      axis(3, at=seq(0,300,50), labels=FALSE, tck=axis_tck1,  pos=43)
      axis(4, at=seq(-45,45,15), labels=FALSE, tck=axis_tck2,  pos=0)
      axis(4,at=c(-30,0,30),labels=c('30º S',0,'30º N'),lwd=0,line=axis_mrkLine_2,cex.axis=cex_axs)
      
      #top panels
      plot(c(390,0),c(0,300),xlim=c(390,0),ylim=c(0,300),col=NA,ylab='',xlab='',yaxt="n",xaxt="n",xaxs="i",yaxs="i",frame=FALSE)
      text(50,90,'50',cex=cex_lab,pos = 1)
      arrows(20,50,0,50,length=0.00,angle=90,code=3,lty=1,lwd=1)
      arrows(50,25,50,0,length=0.00,angle=90,code=3,lty=1,lwd=1)
      text(150,190,'150',cex=cex_lab,pos = 1)
      arrows(110,150,0,150,length=0.00,angle=90,code=3,lty=1,lwd=1)
      arrows(150,120,150,0,length=0.00,angle=90,code=3,lty=1,lwd=1)
      text(320,245,'Maximum Wind\n(km/h)',cex=cex_lab,pos = 4)
    }
    
    #speed 
    if(TRUE){
      #right
      plot(data$landingSpeed,data$landingLat,xlim=c(0,71.5),ylim=c(-43,43),
           ylab='',xlab='',yaxt="n",xaxt="n",xaxs="i",yaxs="i",frame=FALSE,
           pch=data$pch,col=NA,lwd=0.5,bg=adjustcolor(NA,alpha.f = alpha))
      text(data$landingSpeed,data$landingLat,data$pch,cex=cex_pch)
      axis(1, at=seq(0,55,5), labels=FALSE, tck=axis_tck1,  pos=-43)
      arrows(spcut,-43,spcut,43,length=0.00,angle=90,code=3,lty=3,lwd=1)  
      axis(2, at=seq(-45,45,15), labels=FALSE, tck=axis_tck2,  pos=0)
      axis(2,at=c(-30,0,30),labels=c('30º S',0,'30º S'),lwd=0,line=axis_mrkLine_2,cex.axis=cex_axs)
      
      #bottom
      plot(data$landingLon2,data$landingSpeed,xlim=c(25,310),ylim=c(55,0),
           ylab='',xlab='',yaxt="n",xaxt="n",xaxs="i",yaxs="i",frame=FALSE,
           pch=data$pch,col=NA,lwd=0.5,bg=adjustcolor(NA,alpha.f = alpha))
      text(data$landingLon2,data$landingSpeed,data$pch,cex=cex_pch)
      axis(4, at=seq(0,55,5), labels=FALSE, tck=axis_tck2,  pos=310)
      arrows(25,spcut,310,spcut,length=0.00,angle=90,code=3,lty=3,lwd=1) 
      axis(3, at=c(25,310), labels=FALSE, tck=0,  pos=0)
      axis(3, at=seq(30,330,30), labels=FALSE, tck=axis_tck1,  pos=0)
      axis(3,at=c(90,180,270),labels=c('90º E',180,'90º W'),lwd=0,line=axis_mrkLine_1,cex.axis=cex_axs)
      #bottom panel
      plot(c(0),c(0),xlim=c(0,71.5),ylim=c(55,0),col=NA,ylab='',xlab='',yaxt="n",xaxt="n",xaxs="i",yaxs="i",frame=FALSE)
      text(10,2,'10',cex=cex_lab,pos = 1)
      arrows(5,10,0,10,length=0.00,angle=90,code=3,lty=1,lwd=1)
      arrows(10,6,10,0,length=0.00,angle=90,code=3,lty=1,lwd=1)
      text(30,21,'30',cex=cex_lab,pos = 1)
      arrows(24,30,0,30,length=0.00,angle=90,code=3,lty=1,lwd=1)
      arrows(30,25,30,0,length=0.00,angle=90,code=3,lty=1,lwd=1)
      text(63,48,'Forward Velocity\n(km/h)',cex=cex_lab,pos = 2)
    }
    
    dev.off()  
  }
}
















