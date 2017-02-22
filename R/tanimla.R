#' Tanimla function
#' Summarizes the data
#' DescribesBy data for each factor
#' @param x data frame to analyse
#' @param resp response variable
#' @param fac list of factors
#' @export
#' @examples
#' tanimla(x, resp, fac)

tanimla <- function(x, resp, fac){

  control = is.data.frame(x)
  if(!control){
    print("Not an object")
  } else {
    result <- list()
    uda.summary <- summary(x[[resp]])
    result[[1]] <- uda.summary
    print(length(result))
    for(i in fac){
      result[[length(result) + 1]] <- psych::describeBy(x[[resp]], factor(x[[i]]), mat=TRUE, digits=3)
      ylab=i
      main="Veri Box Plot"
      xlab="Faktor Duzeyleri"
      col=rainbow(7)
      font.main=3
      font.lab=6
      cex.main=1
      cex.lab=1
      cex=1
      boxplot(x[[resp]] ~ x[[i]],
              main=main,
              xlab=xlab,
              ylab=ylab,
              col=col,
              cex=cex,
              cex.lab=cex.lab,
              cex.main=cex.main,
              data=x)

      az <- dotplot(x[[resp]] ~ x[[i]],
              main="Veri Dot Plot",
              xlab=xlab,
              ylab=ylab,
              col=col,
              cex=cex,
              cex.lab=cex.lab,
              cex.main=cex.main,
              data=x)
      print(az)

      lwd=1
     xlab=resp
     ylab="Yuzde"
     type.1="percent"
     type.2="density"
     type.3 = "count"
     breaks="Sturges"
     col="gold"
     border="grey100"
     kz <- lattice::histogram(~x[[resp]]|x[[i]], data=x,
               type=type.1,
               xlab=xlab,
               ylab=ylab,
               border=border,
               col=col,
               breaks=breaks)
    print(kz)
    }
    return(result);
  }
}


