hytestvalue<-function(df,dffenzu,originaldf,method="ttest",adjust.method="BH",qvaluethresh=0.05,
                      FCthreshlow=0.67,FCthreshbig=1.5,tpaired=FALSE,varequal=FALSE,
                      talternative = "two.sided",wilcoxpaired=FALSE,
                      wilcoxalternative="two.sided",logdataif=FALSE,
                      wilcoxexact=TRUE,wilcoxcorrect=TRUE,multigrif=FALSE){
  if(logdataif){
    FCthreshlow<-log2(FCthreshlow)
    FCthreshbig<-log2(FCthreshbig)
  }
  hytestrawdatamedian<-as.data.frame(df)
  datafenzudf<-dffenzu
  n_hytestrawdata<-dim(hytestrawdatamedian)[1]
  names(datafenzudf)<-tolower(names(datafenzudf))
  datafenzudf$class<-toupper(datafenzudf$class)
  cvclassnames<-unique(datafenzudf$class)#dimnames(table(datafenzudf$class))[[1]]
  classnames<-cvclassnames
  strhytestclass<-length(classnames)
  classnamesnum<-as.numeric(table(datafenzudf$class)[cvclassnames])
  if(method=="ttest"){
    p.t.test<-vector()
    times.t.test<-vector()
    p.normal.distribution<-vector()
    for(i in 1:n_hytestrawdata){
      tdf<-data.frame(name=as.factor(rep(classnames,times=classnamesnum)),numval=as.numeric(hytestrawdatamedian[i,which(datafenzudf$class!="QC")]))
      ttest<-t.test(as.numeric(hytestrawdatamedian[i,which(datafenzudf$class==classnames[1])]),as.numeric(hytestrawdatamedian[i,which(datafenzudf$class==classnames[2])]),paired = tpaired,alternative = talternative,var.equal = varequal)
      p.shapiro.test<-with(tdf, tapply(numval, name, function(x){
        if(min(x)==max(x)){
          return(0)
        }else{
          xx<-shapiro.test(x)
          return(xx$p.value)
        }
      }))
      p.bartlett.test<-bartlett.test(numval~name,data = tdf)
      p.normal.distribution[i]<-paste(as.numeric(p.shapiro.test),collapse = "_")
      p.t.test[i]<-ttest$p.value
      if(logdataif){
        times.t.test[i]<-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[2])]))-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[1])]))
      }else{
        times.t.test[i]<-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[2])]))/mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[1])]))
      }
    }
    if(adjust.method=="BH"){
      p.adjust<-p.adjust(p.t.test,method = "BH")
    }else{
      p.adjust<-qvalue(p.t.test)$qvalues
    }

    new_hytestrawdata<-data.frame(hytestrawdatamedian,p.normal.distribution=p.normal.distribution,p.t.test=p.t.test,p.adjust=p.adjust,Fold.Change=times.t.test)

  }
  else if(method=="aovtest"){
    p.aov.test<-vector()
    p.normal.distribution<-vector()
    p.variance.homo<-vector()
    times.aov.test<-vector()
    for(i in 1:n_hytestrawdata){
      aovdf<-data.frame(name=as.factor(rep(paste("A",1:length(classnames),sep = ""),times=classnamesnum)),numval=as.numeric(hytestrawdatamedian[i,which(datafenzudf$class!="QC")]))
      aovtest<-summary(aov(numval~name,data=aovdf))
      p.shapiro.test<-with(aovdf, tapply(numval, name, function(x){
        if(min(x)==max(x)){
          return(0)
        }else{
          xx<-shapiro.test(x)
          return(xx$p.value)
        }
      }))
      p.bartlett.test<-bartlett.test(numval~name,data = aovdf)
      p.normal.distribution[i]<-paste(round(as.numeric(p.shapiro.test),7),collapse = "_")
      p.variance.homo[i]<-round(p.bartlett.test$p.value,7)
      p.aov.test[i]<-round(aovtest[[1]][["Pr(>F)"]][1],7)
      if(logdataif){
        times.aov.test[i]<-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[length(classnames)])]))-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[1])]))
      }else{
        times.aov.test[i]<-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[length(classnames)])]))/mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[1])]))
      }

    }
    if(adjust.method=="BH"){
      p.adjust<-p.adjust(p.aov.test,method = "BH")
    }else{
      p.adjust<-qvalue(p.aov.test)$qvalues
    }

    new_hytestrawdata<-data.frame(hytestrawdatamedian,p.normal.distribution=p.normal.distribution,p.variance.homo=p.variance.homo,p.aov.test=p.aov.test,p.adjust=p.adjust,Fold.Change=times.aov.test)

  }
  else if(method=="wilcoxtest"){
    if(strhytestclass<=2){
      p.wilcox.test<-vector()
      times.wilcox.test<-vector()
      for(i in 1:n_hytestrawdata){
        wilcoxtest<-wilcox.test(as.numeric(hytestrawdatamedian[i,which(datafenzudf$class==classnames[1])]),as.numeric(hytestrawdatamedian[i,which(datafenzudf$class==classnames[2])]),paired = wilcoxpaired,alternative = wilcoxalternative,exact=wilcoxexact,correct=wilcoxcorrect)
        p.wilcox.test[i]<-wilcoxtest$p.value
        if(logdataif){
          times.wilcox.test[i]<-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[2])]))-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[1])]))
        }else{
          times.wilcox.test[i]<-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[2])]))/mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[1])]))
        }

      }
      if(adjust.method=="BH"){
        p.adjust<-p.adjust(p.wilcox.test,method = "BH")
      }else{
        p.adjust<-qvalue(p.wilcox.test)$qvalues
      }
      new_hytestrawdata<-data.frame(hytestrawdatamedian,p.wilcox.test=p.wilcox.test,p.adjust=p.adjust,Fold.Change=times.wilcox.test)

    }else{
      p.kruskal.test<-vector()
      times.kruskal.test<-vector()
      for(i in 1:n_hytestrawdata){
        aovdf<-data.frame(name=as.factor(rep(paste("A",1:length(classnames),sep = ""),times=classnamesnum)),numval=as.numeric(hytestrawdatamedian[i,which(datafenzudf$class!="QC")]))
        kruskaltest<-kruskal.test(numval~name,data=aovdf)
        p.kruskal.test[i]<-kruskaltest$p.value
        if(logdataif){
          times.kruskal.test[i]<-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[length(classnames)])]))-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[1])]))
        }else{
          times.kruskal.test[i]<-mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[length(classnames)])]))/mean(as.numeric(originaldf[i,which(datafenzudf$class==classnames[1])]))
        }

      }
      if(adjust.method=="BH"){
        p.adjust<-p.adjust(p.kruskal.test,method = "BH")
      }else{
        p.adjust<-qvalue(p.kruskal.test)$qvalues
      }
      new_hytestrawdata<-data.frame(hytestrawdatamedian,p.kruskal.test=p.kruskal.test,p.adjust=p.adjust,Fold.Change=times.kruskal.test)
    }
  }
  else if(method=="limma"){
    eb.fit <- function(dat, design){
      n <- dim(dat)[1]
      fit <- lmFit(dat, design)
      fit.eb <- eBayes(fit)
      log2FC <- fit.eb$coefficients[, 2]
      df.r <- fit.eb$df.residual
      df.0 <- rep(fit.eb$df.prior, n)
      s2.0 <- rep(fit.eb$s2.prior, n)
      s2 <- (fit.eb$sigma)^2
      s2.post <- fit.eb$s2.post
      t.ord <- fit.eb$coefficients[, 2]/fit.eb$sigma/fit.eb$stdev.unscaled[, 2]
      t.mod <- fit.eb$t[, 2]
      p.ord <- 2*pt(-abs(t.ord), fit.eb$df.residual)
      p.mod <- fit.eb$p.value[, 2]
      q.ord <- qvalue(p.ord)$q
      q.mod <- qvalue(p.mod)$q
      results.eb <- data.frame(log2FC, t.ord, t.mod, p.ord, p.mod, q.ord, q.mod, df.r, df.0, s2.0, s2, s2.post)
      results.eb <- results.eb[order(results.eb$p.mod), ]
      return(results.eb)
    }
    hytestlimmarawdata<-hytestrawdatamedian
    strhytestlimmaclass<-strhytestclass
    strhytestlimmaclassnum<-classnamesnum
    designx <- model.matrix(~factor(rep(1:strhytestlimmaclass,strhytestlimmaclassnum)))
    colnames(designx)<-c("Intercept",paste0("FC",1:(ncol(designx)-1)))
    wtFit <- lmFit(hytestlimmarawdata, designx)
    wtEbFit <- eBayes(wtFit)
    wtEbFittable<-as.data.frame(topTable(wtEbFit,coef=2,number = Inf,sort.by="none"))
    if(adjust.method=="BH"){
      p.adjust<-p.adjust(wtEbFittable$P.Value,method = "BH")
    }else{
      p.adjust<-qvalue(wtEbFittable$P.Value)$qvalues
    }
    strhytestlimmaclassnum1<-cumsum(strhytestlimmaclassnum)
    wtEbFittabledf1<-hytestlimmarawdata[,1:strhytestlimmaclassnum[1]]
    wtEbFittabledf2<-hytestlimmarawdata[,(strhytestlimmaclassnum1[length(strhytestlimmaclassnum1)-1]+1):strhytestlimmaclassnum1[length(strhytestlimmaclassnum1)]]
    if(logdataif){
      FCx<-rowMeans(wtEbFittabledf2,na.rm=T)-rowMeans(wtEbFittabledf1,na.rm=T)
      #wtEbFittable[,c("P.Value","adj.P.Val")]
      wtEbFittabledf<-cbind(hytestlimmarawdata,Fold.Change=FCx,p.Value=wtEbFittable$P.Value,p.adjust=p.adjust)
    }else{
      FCx<-rowMeans(wtEbFittabledf2,na.rm=T)/rowMeans(wtEbFittabledf1,na.rm=T)
      wtEbFittabledf<-cbind(hytestlimmarawdata,Fold.Change=FCx,p.Value=wtEbFittable$P.Value,p.adjust=p.adjust)#wtEbFittable$adj.P.Val
    }
    new_hytestrawdata<-as.data.frame(wtEbFittabledf)
  }
  else{
    new_hytestrawdata<-hytestrawdata
  }
  #new_hytestrawdata[is.na(new_hytestrawdata)]<- Inf
  if(multigrif){
    new_hytestrawdata_index<-which(new_hytestrawdata$p.adjust<=qvaluethresh)
  }else{
    new_hytestrawdata_index<-which(new_hytestrawdata$p.adjust<=qvaluethresh & (new_hytestrawdata$Fold.Change<=FCthreshlow | new_hytestrawdata$Fold.Change>=FCthreshbig))
  }
  new_hytestrawdata1<-new_hytestrawdata[new_hytestrawdata_index,]
  return(list(hytestdf_orginal=new_hytestrawdata,hytestdf_after=new_hytestrawdata1))
}
