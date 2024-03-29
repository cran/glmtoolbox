% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{orobanche}
\alias{orobanche}
\title{Germination of Orobanche Seeds}
\format{
A data frame with 21 rows and 4 variables:
\describe{
  \item{specie}{a factor indicating the specie of Orobanche seed: O. aegyptiaca 75 ("Aegyptiaca 75") and O. aegyptiaca 73 ("Aegyptiaca 73").}
  \item{extract}{a factor indicating the root extract: cucumber ("Cucumber") and bean ("Bean").}
  \item{seeds}{a numeric vector indicating the total number of seeds.}
  \item{germinated}{a numeric vector indicating the number of germinated seeds.}
}
}
\usage{
data(orobanche)
}
\description{
These data arose from a study of the germination of two
species of Orobanche seeds (O. aegyptiaca 75 and O. aegyptiaca 73)
grown on 1/125 dilutions of two different root extract media (cucumber
and bean) in a 2×2 factorial layout with replicates. The data consist
of the number of seeds and germinating seeds for each replicate.
Interest is focused on the possible differences in germination rates
for the two types of seed and root extract and whether there is any
interaction.
}
\examples{
data(orobanche)
out <- aggregate(cbind(germinated,seeds) ~ extract + specie, data=orobanche, sum)
dev.new()
barplot(100*germinated/seeds ~ extract + specie, beside=TRUE, data=out, width=0.3,
        col=c("yellow","blue"), xlab="Specie", ylab="\% of germinated seeds")
legend("topleft",c("Bean","Cucumber"),fill=c("yellow","blue"),title="Extract",bty="n")

}
\references{
Crowder M.J. (1978) Beta-binomial anova for proportions. \emph{Journal of the Royal Statistical Society.
Series C (Applied Statistics)} 27:34-37.

Hinde J., Demetrio C.G.B. (1998) Overdispersion: Models and estimation. \emph{Computational Statistics
& Data Analysis} 27:151-170.
}
\keyword{datasets}
