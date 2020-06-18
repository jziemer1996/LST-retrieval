# compute confusion matrix and kappa coefficient for two string vectors representing classification and reference respectively
# this is done for the class overlap only
accuracy=function(train,test){
  options(scipen=999)
  if(is.factor(train))train=as.character(train)
  if(is.factor(test))test=as.character(test)
  names.classes=sort(na.omit(intersect(train,test)))
  n.classes=length(names.classes)
  # create confusion matrix
  confusion.err=matrix(0,n.classes+1,n.classes+1)
  rownames(confusion.err)=c(names.classes,"Omission")
  colnames(confusion.err)=c(names.classes,"Commission")
  # fill confusion matrix
  for(i in seq(length(train))){
    if(sum(c(train[i],test[i]) %in% names.classes)==2){
      confusion.err[train[i],test[i]]=confusion.err[train[i],test[i]]+1
    }
  }
  confusion.acc=confusion.err
  rownames(confusion.acc)=c(names.classes,"User")
  colnames(confusion.acc)=c(names.classes,"Producer")
  # compute commission and producer's accuracy
  for(x in rownames(confusion.err)[-nrow(confusion.err)]){
    if(x %in% colnames(confusion.err)){
      confusion.acc[x,ncol(confusion.acc)]=confusion.err[x,x]/sum(confusion.err[x,-ncol(confusion.err)])
      confusion.err[x,ncol(confusion.err)]=1-confusion.acc[x,ncol(confusion.acc)]
    }
  }
  # compute omission and user's accuracy
  for(x in colnames(confusion.err)[-ncol(confusion.err)]){
    if(x %in% rownames(confusion.err)){
      confusion.acc[nrow(confusion.acc),x]=confusion.err[x,x]/sum(confusion.err[-nrow(confusion.err),x])
      confusion.err[nrow(confusion.err),x]=1-confusion.acc[nrow(confusion.acc),x]
    }
  }
  # compute accuracies
  acc.user=mean(confusion.acc[nrow(confusion.acc),-ncol(confusion.acc)])
  acc.producer=mean(confusion.acc[-nrow(confusion.acc),ncol(confusion.acc)])
  n.points=sum(confusion.err[-nrow(confusion.err),-ncol(confusion.err)])
  acc.overall=sum(sapply(names.classes,function(x)confusion.err[x,x]))/n.points
  
  confusion.err[nrow(confusion.err),ncol(confusion.err)]=acc.overall
  confusion.acc[nrow(confusion.acc),ncol(confusion.acc)]=acc.overall
  
  # compute kappa
  h.random=sum(sapply(names.classes,function(x)sum(confusion.err[x,-ncol(confusion.err)])*sum(confusion.err[-nrow(confusion.err),x])))/(n.points^2)
  kappa=(acc.overall-h.random)/(1-h.random)
  
  # return results in a list
  return(list("confusion.acc"=confusion.acc,"confusion.err"=confusion.err,
              "acc.overall"=acc.overall,"acc.user"=acc.user,"acc.producer"=acc.producer,
              "kappa"=kappa,"n.points"=n.points))
}
################################################################
#WRAPPER FOR BEAN PLOTTING WITH ADDITIONAL MEDIAN AND STANDARD DEVIATION LINES
beanplotter=function(dataframe,outname,yname,maxval=Inf){
  pdf(file=outname,width=2*ncol(dataframe),height=5)
  par(mar=c(2,3,1,1))#bottom,left,top,right
  par(mgp=c(1.5,0.5,0))#axlabeldist,ticklabeldist,axdist
  beanplot::beanplot(dataframe,main="",xlab="",ylab=yname,axes=F,log="",cex=2.5,cutmax=maxval)
  for(j in 1:ncol(dataframe)){
    start=.95+(j-1)
    median=median(dataframe[,j])
    sdev=sd(dataframe[,j])
    mean=mean(dataframe[,j])
    segments(start,median,(start+.1),median,col="red",lwd=3)
    segments(start,mean-sdev,(start+.1),mean-sdev,col="green",lwd=3)
    segments(start,mean+sdev,(start+.1),mean+sdev,col="green",lwd=3)
  }
  axis(1,at=1:ncol(dataframe),labels=names(dataframe),cex=2.5)
  axis(2,ylab=yname,cex=2.5)
  box()
  dev.off()
}

################################################################
#FROM SOHPIE: clip function

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

################################################################
#SCATTERPLOTTING
#input must be a list with two named vectors
#see function 'quality' for options of the variable measures
graph=function(stack,xname,yname,equalaxis=T,diagonal=T,legend=T,outname=NULL,measures="all"){
  if(!is.null(outname)){
    pdf(file=outname,width=5,height=5)
    par(mar=c(3,3,1,1))#bottom,left,top,right
    par(mgp=c(1.5,0.5,0))
  }
  dcols=densCols(stack[[1]],stack[[2]],colramp=colorRampPalette(c("#000099","#00FEFF","#45FE4F","#FCFF00","#FF9400","#FF3100")))
  if(equalaxis==T){
    min=floor(min(stack[[1]],stack[[2]],na.rm=T))
    max=ceiling(max(stack[[1]],stack[[2]],na.rm=T))
    plot(c(min,max),c(min,max),type="n",xlab=xname,ylab=yname)
  }else{
    min=sapply(1:2,function(x)floor(min(stack[[x]],na.rm=T)))
    max=sapply(1:2,function(x)ceiling(max(stack[[x]],na.rm=T)))
    plot(c(min[1],max[1]),c(min[2],max[2]),type="n",xlab=xname,ylab=yname)
  }
  if(diagonal==T)abline(0,1)
  
  points(stack[[1]],stack[[2]],col=dcols,pch=20,cex=.65)
  #LEGEND WITH QUALITY MEASURES COUMPUTED BY FUNCTION 'QUALITY'
  if(legend==T)legend('bottomright',legend=quality(stack[[2]],stack[[1]],measures=measures),bty="n",cex=0.75,xjust=1)
  
  if(!is.null(outname))dev.off()
}
################################################################
#LOAD .RDATA OBJECT INTO A VARIABLE
load_obj=function(rdata){
  env=new.env()
  nm=load(rdata,env)[1]
  env[[nm]]
}
################################################################
#PLOT VARIABLE IMPORTANCE FROM MODEL TUNING OBJECTS CREATED BY CARET::TRAIN
importance=function(tune,top=NA,plot=T,title=NULL,sorted=T){
  frame=caret::varImp(tune)[[1]]
  if(grepl("Overall",names(frame))){
    imp=frame$Overall
    names(imp)=rownames(frame)
  }else{
    imp=rowMeans(frame)
  }
  if(sorted)imp=sort(imp)
  if(!is.na(top)&length(imp)>top){
    imp=imp[(length(imp)-(top-1)):(length(imp))]
  }
  if(plot==T){
    plot(c(min(imp),max(imp)),c(1,length(imp)),type="n",xlab="importance",ylab="",xaxt="n",yaxt="n",main=title)
    for (j in 1:length(imp)){
      segments(0,j,imp[j],j,col="black",lwd=1)
    }
    points(imp,seq(1:length(imp)),pch=20,col="blue")
    axis(2,at=seq(1,length(imp),by=1),labels=names(imp),las=1)#,cex.axis=0.8
    axis(1)#,cex.axis=.8
  }else{
    return(imp)
  }
}
################################################################
#PLOT MEAN VARIABLE IMPORTANCE FROM A LIST OF MODEL TUNING OBJECTS CREATED BY CARET::TRAIN
importance2=function(tunelist,outname,top=20){
  varimp.l=lapply(tunelist,function(x)caret::varImp(x,scale=F)[[1]])#set scale to T in order to normalize variable importance; see here: ftp://ftp.uni-bayreuth.de/pub/math/statlib/R/CRAN/doc/vignettes/caret/caretVarImp.pdf
  for(i in seq(varimp.l)){
    names(varimp.l[[i]])=paste("rep",i,sep="")
    if(i==1){
      varimp.df=varimp.l[[1]]
    }else{
      varimp.df=merge(varimp.df,varimp.l[[i]],by="row.names",all=T)
      rownames(varimp.df)=varimp.df$Row.names
      varimp.df=varimp.df[,-1]
    }
  }
  varimp.df$mean=apply(varimp.df,1,mean,na.rm=T)
  varimp.df$mean[is.na(varimp.df$mean)]=0
  varimp.df=varimp.df[with(varimp.df,order(mean)),]
  varimp.df$sdev=apply(varimp.df,1,sd,na.rm=T)
  varimp.df$sdev[is.na(varimp.df$sdev)]=0
  imp=varimp.df$mean
  names(imp)=rownames(varimp.df)
  if(length(imp)>top)imp=imp[(length(imp)-(top-1)):(length(imp))]
  
  #control size of importance plots
  pdf(file=outname,width=10,height=4)#original width=8.5
  par(mar=c(3,5.5,1,0.5))#bottom,left,top,right
  par(mgp=c(1.5,0.5,0))
  par(mfrow=c(1,2))#number of subplots y,x
  
  plot(c(min(imp),max(imp)),c(1,length(imp)),type="n",xlab="importance",ylab="",xaxt="n",yaxt="n",main=NULL)
  for (j in 1:length(imp)){
    segments(0,j,imp[j],j,col="black",lwd=1)
  }
  points(imp,seq(1:length(imp)),pch=20,col="blue")
  axis(2,at=seq(1,length(imp),by=1),labels=names(imp),las=1,cex.axis=0.5)#,cex.axis=0.8 <-- control size of y axis font in left plot
  axis(1)#,cex.axis=.8
  
  par(mar=c(3,3,1,1))#bottom,left,top,right
  plot(x=varimp.df$mean,y=seq(varimp.df$mean),type="l",xlab="importance",ylab="variable")
  
  polygon(x=c(varimp.df$mean+varimp.df$sdev,rev(varimp.df$mean-varimp.df$sdev)),
          y=c(seq(varimp.df$mean),rev(seq(varimp.df$mean))),
          col=rgb(159,175,228,alpha=150,maxColorValue=255),border=NA)
  lines(x=varimp.df$mean,y=seq(varimp.df$mean))
  box()
  dev.off()
}

################################################################
#FROM SOPHIE: PLOT-FORMATTED REGRESSION EQUATION
lm_eqn=function(m){
  l=list(a=format(coef(m)[1],digits=2),
         b=format(abs(coef(m)[2]),digits=2))
  if(coef(m)[2]>=0){
    eq=substitute(italic(y)==a+b%.%italic(x),l)
  }else{
    eq=substitute(italic(y)==a-b%.%italic(x),l)
  }
  return(as.expression(eq))
}

################################################################
#GEOREFERENCED SAMPLING OF MULTIPLE RASTER OBJECTS
#Gewichtetes Mittel anhand von "weights=TRUE"
multiextract=function(objectlist,samplingshape,method="bilinear",fun=mean,cores=parallel::detectCores()-1){
  if(length(objectlist)>=2){
    require(foreach)
    require(raster)
    if(cores>length(objectlist))cores=length(objectlist)
    cl=parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl,cores)
    samp=foreach(i=seq(objectlist),.combine=cbind,.packages=c("raster"))%dopar%{
      raster::extract(objectlist[[i]],samplingshape,method=method,fun=fun,sp=F,na.rm=T)
      
    }
    names=sapply(objectlist,function(x)names(x))
    samplingshape@data[,names]=samp
    unregister(cl)
  }else{
    samplingshape=raster::extract(objectlist[[1]],samplingshape,method=method,fun=fun,sp=T,na.rm=T) 
  }
  return(samplingshape)
}
################################################################
#MULTICORE MODEL PREDICTION
multipredict=function(fit,dataset,cores=parallel::detectCores()-1){
  dataset=dataset[,names(fit$trainingData)[-ncol(fit$trainingData)]]
  require(foreach)
  cl=parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl,cores)
  packages=c("rf"=c("randomForest"),"cforest"=c("party"),"svmRadial"=c("kernlab"))
  split=sort(1:nrow(dataset)%%cores)
  prediction=foreach(i=unique(split),.combine=c,.packages=packages[fit$method])%dopar%{
    sub=dataset[split==i,]
    predict(fit,newdata=sub,na.action=na.exclude)
  }
  unregister(cl)
  return(prediction)
}
################################################################
#MULTICORE MODEL LIST PREDICTION
multipredict2=function(fitlist,dataset,cores=parallel::detectCores()-1){
  dataset=dataset[,names(fitlist[[1]]$trainingData)[-ncol(fitlist[[1]]$trainingData)]]
  sub=na.omit(dataset)
  omit=as.vector(na.action(sub))
  require(foreach)
  cl=parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl,cores)
  packages=c("rf"=c("randomForest"),"cforest"=c("party"),"svmRadial"=c("kernlab"))
  split=sort(seq(fitlist)%%cores)
  prediction=foreach(i=unique(split),.combine=c,.packages=packages[fitlist[[1]]$method])%dopar%{
    lapply(seq(fitlist)[split==i],function(x)predict(fitlist[[x]],newdata=sub,na.action=na.exclude))
  }
  unregister(cl)
  if(length(omit)>0){
    for(i in seq(prediction)){
      temp=rep(NA,nrow(dataset))
      temp[-omit]=prediction[[i]]
      prediction[[i]]=temp
    }
  }
  return(prediction)
}
################################################################
#find files (not) matching multiple patterns in their name
#caution! this function is not optimal and easily confusing, always check the output first
#function lacks error handling for cases where certain defined tags are not matched
#in_p,in_s: primary/secondary tags to include
#ex: primary tags to exclude
#all three can be given as single strings or string vectors
#e.g. patternator(path,in_p=c("NORM","INT"),in_s="2014",ex=c("COV05","DB")) will include all files including either "NORM" or "INT", but only
#those that also include "2014". Of these selected files the ones including "COV05" or "DB" will be excluded.
#recursive: Unterverzeichnisse durchsuchen, iterativ =/ rekursiv
patternator=function(path,in_p=c(),in_s=c(),ex=c(),rec=F){
  candidates=list.files(path,full.names=T,recursive=rec)
  include_p=if(length(in_p)>0) candidates[apply(sapply(in_p,grepl,candidates),1,any)] else NA
  include_s=if(length(in_s)>0) include_p[apply(sapply(in_s,grepl,include_p),1,all)] else include_p
  exclude=if(length(ex)>0) include_s[apply(sapply(ex,grepl,include_s),1,any)] else NA
  return(setdiff(include_s,exclude))
}
################################################################
#CONVERT FISHNET SHAPEFILE OBJECT TO RASTER
point2ras=function(shape,parameter,outname=NULL,dtype="FLT4S",ftype="GTiff"){
  require(raster)
  rast=raster()
  resolution=round(shape@coords[[2]]-shape@coords[[1]],0)
  
  extent(rast)=extent(shape)
  for(i in 1:4){extent(rast)[i]=extent(rast)[i]+(resolution/2*c(-1,1,-1,1)[i])}
  
  projection(rast)=projection(shape)
  res(rast)=resolution
  rast=raster::rasterize(shape,rast,parameter,fun=mean,na.rm=T)
  if(is.null(outname)){
    return(rast)
  }else{
    raster::writeRaster(rast,filename=outname,datatype=dtype,format=ftype,bandorder="BSQ",overwrite=T,NAflag=-999)
  }
}

################################################################
#FROM SOPHIE: QUALITY MEASURES
#calculates loads of statitistical measures
quality=function(model,reference,measures="all"){
  regmodel=lm(reference~model,na.action="na.omit")
  npts=length(na.omit(apply(cbind(model,reference),1,max)))
  R2=round((summary(regmodel)$r.squared),digits=2)
  RMSE=round((sqrt(mean((model-reference)^2,na.rm=T))),digits=2)
  RMSE_rel=round(RMSE/mean(reference,na.rm=T)*100,digits=2)
  MAE=round(mean(abs(model-reference),na.rm=T),digits=2)
  #Bias=round(((sum(model)-sum(reference))/npts,na.rm=T),digits=2)
  LE90=round(quantile(abs(model-reference),probs=.9,na.rm=T,names=F),digits=2)
  rp=vector("expression",8)
  rp["N"]=substitute(expression(italic(N)==VALUE),list(VALUE=format(npts)))[2]
  rp["R2"]=substitute(expression(italic(R)^2==VALUE),list(VALUE=format(R2)))[2] 
  rp["MAE"]=substitute(expression(italic(MAE)==VALUE),list(VALUE=format(MAE)))[2]
  rp["Bias"]=substitute(expression(italic(Bias)==VALUE),list(VALUE=format(Bias)))[2]
  rp["RMSE"]=substitute(expression(italic(RMSE)==VALUE),list(VALUE=format(RMSE)))[2]
  rp["rRMSE"]=substitute(expression(italic(relRMSE)==VALUE),list(VALUE=format(RMSE_rel)))[2]
  rp["LE90"]=substitute(expression(italic(LE90)==VALUE),list(VALUE=format(LE90)))[2]
  rp["EQ"]=lm_eqn(regmodel)
  if(length(measures)>1) return(rp[measures]) else return(rp)
}

################################################################
#RESAMPLING OF A DATASET FOR MODELING PURPOSES (RETURNS LIST OF INDICES FOR SUBSETTING THE ORIGINAL DATASET)
#By default, the resample function draws from all reference data values available with fraction=1
#By default, the resample function stratifies the drawing for each strap into five value ranges with strata=5 according to the reference data
#By default, drawing with replacement is switched on
resample=function(stratvar,repeats=500,fraction=1,strata=5,replacement=T){
  
  resample=list()
  
  #check whether dataset is consists of factors
  if(!is.factor(stratvar)){
    
    data=data.frame(stratvar,1:length(stratvar))
    
    #stratify the dataset based on defined quantiles; see end of this doc
    quants=c(0,quantile(data[,1],seq(1/strata,1,length=strata)))
    #for Woody Cover based on TLS data extracted in eCognition; this line was necessary because of many non-unique values
    #if(quants[1] == quants[2])quants[1]=quants[1]-.00001#runif(1, min=.00001, max=.009)
    stratify=split(data,cut(data[,1],quants,include.lowest=T))
    
    #perform resampling (independently for each stratification)
    for(i in 1:repeats){
      resample[[i]]=sort(unname(unlist(lapply(stratify,function(x)x[sample(1:nrow(x),ceiling(nrow(x)*fraction),replace=replacement),2]))))
    }
  }else{
    indices=which(!is.na(stratvar))
    data=data.frame(stratvar[indices],indices)
    names(data)=c("value","index")
    
    count=sapply(levels(data[,"value"]),function(x)length(which(data[,"value"]==x)))
    probs=min(count)/count
    data[,"probs"]=sapply(data[,"value"],function(x)unname(probs[x]))
    
    for(i in 1:repeats){
      resample[[i]]=sort(sample(data[,"index"],ceiling(nrow(data)*fraction),replace=replacement,prob=data[,"probs"]))
    }
  }
  ifelse(length(resample)==1,return(resample[[1]]),return(resample))
}
################################################################
#CREATE SPATIALPOINTSDATAFRAME FOR GEOREFERENCED SYSTEMATIC SAMPLING
sampler=function(boundingbox,distance,outpath=NULL){
  require(raster)
  ext=raster::extent(boundingbox)
  xNum=floor((ext@xmax-ext@xmin)/distance)
  yNum=floor((ext@ymax-ext@ymin)/distance)
  npoints=xNum*yNum
  sampling=sp::spsample(boundingbox,n=npoints,type='regular',offset=c(0,0))
  id=seq(1,nrow(sampling@coords))
  sampling=sp::SpatialPointsDataFrame(sampling,data=data.frame(ID=id))
  if (is.null(outpath)){
    return(sampling)
  }else{
    rgdal::writeOGR(sampling,dsn=outpath,layer=paste("sampling",distance,"m",sep=""),driver='ESRI Shapefile')
  }
}
################################################################
#TRANSFER VALUES OF AN ARRAY/MATRIX TO A RASTER OBJECT
#note: this is almost equivalent to raster::setValues; the latter has been found to rotate square images
setval=function(raster,data){
  data=if(class(data)=="array") raster::brick(data) else raster::raster(data)
  raster::extent(data)=raster::extent(raster)
  raster::projection(data)=raster::projection(raster)
  return(data)
}
################################################################
#WRAPPER FOR SHAPEFILE READING
shapereader=function(name){
  return(rgdal::readOGR(dsn=name,layer=tools::file_path_sans_ext(basename(name)),verbose=F))
}
################################################################
#WRAPPER FOR SHAPEFILE WRITING
shapewriter=function(data,name){
  rgdal::writeOGR(data,dsn=dirname(name),layer=tools::file_path_sans_ext(basename(name)),driver='ESRI Shapefile',verbose=F,overwrite_layer=T)
}
################################################################
# UNREGISTER PARALLEL COMPUTING BACKEND
unregister=function(cluster){
  parallel::stopCluster(cluster)
  env=foreach:::.foreachGlobals
  rm(list=ls(name=env),pos=env)
}
################################################################
#WRITE MATRIX OR RASTER OBJECT TO FILE
writer=function(data,reference=NULL,outname,type="FLT4S",naflag=-999){
  
  format=if(length(dim(data))>2&dim(data)[3]>1) "ENVI" else "GTiff"
  opt=if(format=="GTiff") c("COMPRESS=NONE") else ""
  
  raster::rasterOptions(overwrite=T,datatype=type,setfileext=if(format=="ENVI") F else T)
  
  if(class(data) %in% c("array","matrix")){
    data=setval(reference,data)
  }
  raster::writeRaster(data,filename=outname,format=format,bandorder="BSQ",NAflag=naflag,options=opt)
  # add layer names to header file
  if(format=="ENVI"){
    name.hdr=paste(outname,".hdr",sep="")
    hdr=readLines(name.hdr)
    index=grep("band names",hdr)
    while(!grepl("}",hdr[index]))index=index+1
    bandnames=paste("band names = {",paste(names(data),collapse=", "),"}",sep="")
    data.ignore=paste("data ignore value =",naflag)
    hdr=c(hdr[-(grep("band names",hdr):index)],bandnames,data.ignore)
    writeLines(hdr,name.hdr)
  }
}
################################################################

#Quantile experiments
#if(quants[2] == quants[3])quants[3]=quants[2]+.1
#if(quants[3] >= quants[4])quants[4]=quants[3]+.1
#if(quants[4] >= quants[5])quants[5]=quants[4]+.1
#if(quants[5] >= quants[6])quants[6]=quants[5]+.1
#if(quants[6] >= quants[7])quants[7]=quants[6]+.1
#if(quants[7] >= quants[8])quants[8]=quants[7]+.1
#if(quants[8] >= quants[9])quants[9]=quants[8]+.1
#if(quants[9] >= quants[10])quants[10]=quants[9]+.1
#if(quants[10] >= quants[11])quants[11]=quants[10]+.1