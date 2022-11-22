##' seal.increm
##' @param year vector with annual values
##' @param age vector with ages
##' @param weight vector with weights
##' @param sample.id vector with sample ids
##' @import dplyr
##' @details get waa
get.waa <- function(year,age,weight,sample.id=rep(1,length(year))){
    df <- data.frame(year=year,age=age,weight=weight,sample.id=sample.id)
    ddply(df,c('year','age'),summarise,
          w=mean(weight),
          wsd=sd(weight),
          n=length(weight),
          N=length(unique(sample.id)))
}
