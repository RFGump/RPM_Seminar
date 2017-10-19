#----------------------------------------------------------------------------
# Initial Setup
#----------------------------------------------------------------------------

setwd('Y:\\My Documents\\Presentations\\2013 RPM')
.libPaths('T:\\GLM\\Reference\\R\\64_Packages')

#----------------------------------------------------------------------------
# Reading in shape files
#----------------------------------------------------------------------------

library(maptools)
wthr<-readShapeSpatial('WeatherExample')
us_wthr <-wthr[wthr$ST_ABBREV!='AK' & wthr$ST_ABBREV!='HI',]

library(RColorBrewer)
library(classInt)
breaks <- classIntervals(wthr[['TMAX']],n=11,style='quantile')$brks

intplot<-function(dfrm,var,ints=11,breaks){
  plotclr <- rev(brewer.pal(ints,"Spectral"))
  win.graph(width=5.5,height=4,pointsize=8)
  spplot(dfrm,var,
         main=paste(var,'by County'),
         col='transparent',
         col.regions=plotclr,
         at=breaks)
}

intplot(us_wthr,'TMAX',11,breaks)

us_wthr1<-us_wthr
us_wthr1$TMAX[sample(1:3109,1500)]<-NA
intplot(us_wthr1,'TMAX',11,breaks)

library(gstat)
impute<-function(dfrm,var){
  for(i in 1:length(var)){
    print(system.time(dfrm[[var[i]]][is.na(dfrm[[var[i]]])] <- idw(as.formula(paste(var[i],"~ 1")),
                                                               locations=dfrm[!is.na(dfrm[[var[i]]]),],
                                                               newdata=dfrm[is.na(dfrm[[var[i]]]),],
                                                               idp=2)$var1.pred))
    print(paste(var[i],i,'of',length(var)))
  }
  dfrm
}

us_wthr2<-impute(us_wthr1,'TMAX')
intplot(us_wthr2,'TMAX',11,breaks)
