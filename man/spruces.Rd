% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{spruces}
\alias{spruces}
\title{Effect of ozone-enriched atmosphere on growth of sitka spruces}
\format{
A data frame with 1027 rows and 4 variables:
\describe{
  \item{tree}{a factor giving an unique identifier for each tree.}
  \item{days}{a numeric vector giving the number of days since the beginning of the experiment.}
  \item{size}{a numeric vector giving an estimate of the volume of the tree trunk.}
  \item{treat}{a factor giving the treatment received for each tree: "normal" and "ozone-enriched".}
}
}
\usage{
data(spruces)
}
\description{
These data are analyzed primarily to determine how ozone
pollution affects tree growth. As ozone pollution is common in urban
areas, the impact of increased ozone concentrations on tree growth is
of considerable interest. The response variable is tree size, where
size is conventionally measured by the product of tree height and
stem diameter squared. In the first group, 54 trees were grown under
an ozone-enriched atmosphere, ozone exposure at 70 parts per billion.
In the second group, 25 trees were grown under normal conditions. The
size of each tree was observed 13 times across time, that is, 152,
174, 201, 227, 258, 469, 496, 528, 556, 579, 613, 639 and 674 days
since the beginning of the experiment. Hence, the objective is to
compare the trees' growth patterns under the two conditions.
}
\examples{
data(spruces)
dev.new()
boxplot(size ~ days, data=subset(spruces,treat=="normal"), at=c(1:13) - 0.2,
        col="yellow", boxwex=0.3, xaxt="n", xlim=c(0.9,13.1))
boxplot(size ~ days, data=subset(spruces,treat=="ozone-enriched"), add=TRUE,
        at=c(1:13) + 0.2, col="blue", boxwex=0.3, xaxt="n")
axis(1, at=c(1:13), labels=unique(spruces$days))
axis(2, at=seq(0,2000,250), labels=seq(0,2000,250))
legend("topleft", legend=c("normal","ozone-enriched"), fill=c("yellow","blue"),
       title="Atmosphere", bty="n")

}
\references{
Diggle P.J., Heagarty P., Liang K.-Y., Zeger S.L. (2002) \emph{Analysis of Longitudinal Data}. Oxford University Press, Oxford.

Crainiceanu C.M., Ruppert D., Wand M.P. (2005). Bayesian Analysis for Penalized Spline Regression Using WinBUGS. \emph{Journal of Statistical Software} 14(14):1-24.
}
\keyword{datasets}
