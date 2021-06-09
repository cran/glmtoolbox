#'
#' @title Effect of ozone-enriched atmosphere on growth of sitka spruce
#'
#' @description The main objective of the analysis of these data is to assess the effect of the ozone pollution on the tree growth. As ozone pollution
#' is common in urban areas, the impact of increased ozone concentrations on tree growth is of considerable interest. The
#' response variable is tree size, where size is conventionally measured by the product of tree height and stem diameter squared.
#' In a first group, a total of 54 trees were grown under an ozone-enriched atmosphere, that is, ozone exposure at 70 parts per billion,
#' whereas in a second group, 25 were grown under a normal atmosphere. The size of each tree was observed 13 times across the time, that is,
#' 152, 174, 201, 227, 258, 469, 496, 528, 556, 579, 613, 639 and 674 days since the beginning of the experiment. Hence,
#' the objective is to compare the growth patterns of the trees under the two conditions. See Diggle et al. (2002, page 4).
#'
#' @docType data
#'
#' @usage data(spruce)
#'
#' @format A data frame with 1027 rows and 4 variables:
#' \describe{
#'   \item{tree}{a factor giving an unique identifier for each tree.}
#'   \item{days}{a numeric vector giving the number of days since the beginning of the experiment.}
#'   \item{size}{a numeric vector giving an estimate of the volume of the tree trunk.}
#'   \item{treat}{a factor giving the treatment received for each tree: "normal" and "ozone-enriched".}
#' }
#' @keywords datasets
#' @examples
#' boxplot(size ~ days, data=subset(spruce,treat=="normal"), at=c(1:13)-0.2,
#'         col="yellow", boxwex=0.3, outline=FALSE, xaxt="n", xlim=c(0.9,13.1))
#' boxplot(size ~ days, data=subset(spruce,treat=="ozone-enriched"), add=TRUE,
#'         at=c(1:13)+0.2, col="blue", boxwex=0.3, outline=FALSE, xaxt="n")
#' axis(1, at=1:13, labels=unique(spruce$days))
#' axis(2, at=seq(0,1500,250), labels=seq(0,1500,250))
#' legend(0.5, 1500, legend=c("normal","ozone-enriched"), title="Atmosphere",
#'        fill=c("yellow","blue"), bty="n")
#' @references Diggle P.J., Heagarty P., Liang K.-Y. and Zeger S.L. (2002) \emph{Analysis of Longitudinal Data}. Oxford University Press, Oxford.
#'
"spruce"

#' @title Agents to stimulate cellular differentiation

#' @description In a biomedical study of the immuno-activating ability of two agents, TNF (tumor necrosis factor)
#' and IFN (interferon), to induce cell differentiation, the number of cells that exhibited markers of
#' differentiation after exposure to TNF and IFN was recorded. At each of the 16 dose combinations of
#' TNF/INF, 200 cells were examined. The main question is whether the two
#' agents stimulate cell differentiation synergistically or independently.
#'
#' @docType data
#'
#' @usage data(cellular)
#'
#' @format A data frame with 16 rows and 3 variables:
#' \describe{
#'   \item{cells}{a numeric vector giving the number of cells that exhibited markers of
#' differentiation after exposure to the dose of the two agents}
#'   \item{tnf}{a numeric vector giving the dose (U/ml) of TNF}
#'   \item{ifn}{a numeric vector giving the dose (U/ml) of IFN}
#' }
#' @keywords datasets
#' @examples
#' barplot(100*cells/200 ~ ifn + tnf, beside=TRUE, data=cellular, col=terrain.colors(4),
#'         xlab="Dose of TNF", ylab="% of cells with markers of differentiation")
#' legend(1, 90, c("0","4","20","100"), fill=terrain.colors(4), bty="n", cex=0.9,
#'        title="Dose of IFN")
#' @references Piegorsch W.W., Weinberg C.R. and Margolin B.H. (1988) Exploring
#' simple independent action in multifactor tables of proportions. \emph{Biometrics} 44, 595-603.
#'
#' Vanegas L.H. and Rondon L.M. (2020) A data transformation to deal with
#' constant under/over-dispersion in binomial and poisson regression models. \emph{Journal
#' of Statistical Computation and Simulation} 90, 1811-1833.
"cellular"

#' @title Teratogenic effects of phenytoin and trichloropropene oxide

#' @description The data come from a 2x2 factorial design with 81 pregnant mice. In the experiment each
#' pregnant mouse was randomly allocated to an control group and three treated groups, which
#' received daily, by gastric gavages, 60 mg/kg of phenytoin, 100 mg/kg of
#' trichloropropene oxide, or 60 mg/kg phenytoin and 100 mg/kg of trichloropropene
#' oxide. On day 18 of gestation, fetuses were recovered, stained, and cleared. Then, by
#' visual inspection, the presence or absence of ossification was determined for the different
#' joints of the right and left forepaws. The purpose of the experiment was to investigate the
#' synergy of phenytoin  and trichloropropene oxide to produce ossification at the phalanges,
#' that is, teratogenic effects. See Morel and Nagaraj (2012, page 103).
#'
#' @docType data
#'
#' @usage data(ossification)
#'
#' @format A data frame with 81 rows and 4 variables:
#' \describe{
#'   \item{fetuses}{a numeric vector giving the number of fetuses showing ossification on the left middle third phalanx.}
#'   \item{litter}{a numeric vector giving the litter size.}
#'   \item{pht}{a factor giving the dose (mg/kg) of phenytoin: "0 mg/kg" or "60 mg/kg".}
#'   \item{tcpo}{a factor giving the dose (mg/kg) of trichloropropene oxide: "0 mg/kg" or "100 mg/kg".}
#' }
#' @keywords datasets
#' @examples
#' boxplot(100*fetuses/litter ~ pht, data=subset(ossification, tcpo=="0 mg/kg"),
#'         at=c(1:2)-0.2, col="yellow", boxwex=0.25, outline=FALSE, xaxt="n",
#'         xlab="Dose of PHT", ylab="% of fetuses showing ossification")
#' boxplot(100*fetuses/litter ~ pht, data=subset(ossification, tcpo=="100 mg/kg"),
#'         add=TRUE, at=c(1:2)+0.2, col="blue", boxwex=0.25, outline=FALSE, xaxt="n")
#' axis(1, at=1:2, labels=levels(ossification$pht))
#' legend(0.25, 20, legend=c("0 mg/kg","100 mg/kg"), fill=c("yellow","blue"),
#'        bty="n", cex=0.9, title="Dose of TCPO")
#' @references Morel J.G. and Neerchal N.K. (1997) Clustered binary logistic regression in teratology data
#' using a finite mixture distribution. \emph{Statistics in Medicine} 16, 2843-2853.
#'
#' Morel J.G. and Nagaraj N.K. (2012) \emph{Overdispersion Models in SAS}. SAS Institute Inc., Cary, North Carolina, USA.
"ossification"

#' @title Self diagnozed ear infections in swimmers
#'
#' @description The data come from the Pilot Surf/Health
#' Study of NSW Water Board performed in 1990 on 287
#' recruits. The objective of the study was to determine,
#' in particular, whether beach swimmers run a greater
#' risk of contracting ear infections than non-beach swimmers.
#' See Hand et al. (1994. page 266).
#'
#' @docType data
#'
#' @usage data(swimmers)
#'
#' @format A data frame with 287 rows and 5 variables:
#' \describe{
#'   \item{frequency}{a factor giving the recruit's perception of whether he or she is a frequent swimmer: "frequent" and "occasional".}
#'   \item{location}{a factor giving the recruit's usually chosen swimming location: "beach" and "non-beach".}
#'   \item{age}{a factor giving the recruit's age range: "15-19", "20-24" and "25-29".}
#'   \item{gender}{a factor giving the recruit's gender: "male" and "female".}
#'   \item{infections}{a numeric vector giving the number of self diagnozed
#'         ear infections that were reported by the recruit.}
#' }
#' @keywords datasets
#' @examples
#' boxplot(infections ~ frequency, data=subset(swimmers,location=="non-beach"),
#'         at=c(1:2)-0.2, col="yellow", boxwex=0.25, outline=FALSE, xaxt="n")
#' boxplot(infections ~ frequency, data=subset(swimmers,location=="beach"), add=TRUE,
#'         at=c(1:2)+0.2, col="blue", boxwex=0.25, outline=FALSE, xaxt="n")
#' axis(1, at=1:2, labels=levels(swimmers$frequency))
#' legend(0.3, 6, legend=c("non-beach","beach"), title="Location",
#'        fill=c("yellow","blue"), bty="n", cex=0.9)
#' @references Hand D.J., Daly F., Lunn A.D., McConway K.J. and Ostrowsky E. (1994)
#' \emph{A Handbook of Small Data Sets}, Chapman and Hall, London.
#'
#' Vanegas L.H. and Rondon L.M. (2020) A data transformation to deal with
#' constant under/over-dispersion in binomial and poisson regression models. \emph{Journal
#' of Statistical Computation and Simulation} 90, 1811-1833.
"swimmers"


#' @title Ability of retinyl acetate to prevent mammary cancer in rats
#'
#' @description A total of 76 female rats were injected with a carcinogen for
#' mammary cancer. Then, all animals were given retinyl acetate (retinoid) to
#' prevent mammary cancer for 60 days. After this phase, the 48 animals that
#' remained tumor-free were randomly assigned to continue the retinoid prophylaxis
#' or control. Rats were then palpated for tumors twice weekly, and observations
#' ended 182 days after the initial carcinogen injections began. The main objective
#' of analysis was to assess the difference in the development of tumors between
#' the treated and control groups. See Morel and Nagaraj (2012, page 63).
#'
#' @docType data
#'
#' @usage data(mammary)
#'
#' @format A data frame with 48 rows and 2 variables:
#' \describe{
#'   \item{group}{a factor giving the group to which the rat was assigned: "retinoid" or "control".}
#'   \item{tumors}{a numeric vector giving the number of tumors identified on the rat.}
#' }
#' @keywords datasets
#' @examples
#' boxplot(tumors ~ group, data=mammary, outline=FALSE, xlab="Group",
#'         ylab="Number of tumors", col=c("yellow","blue"))
#' @references Lawless J.F. (1987) Regression Methods for Poisson Process Data. \emph{Journal of the American
#' Statistical Association}, 82, 808-815.
#'
#' Morel J.G. and Nagaraj N.K. (2012) \emph{Overdispersion Models in SAS}. SAS Institute Inc., Cary, North Carolina, USA.
"mammary"

#' @title Species richness
#'
#' @description In these data the response is the species richness represented by a
#' count of the number of plant species on plots that have different biomass and
#' three different soil pH levels: low, mid, and high. See Crawley (2007, page 534).
#'
#' @docType data
#'
#' @usage data(richness)
#'
#' @format A data frame with 90 rows and 3 variables:
#' \describe{
#'   \item{Biomass}{a numeric vector giving the value of the biomass in the plots.}
#'   \item{pH}{a factor giving the soil pH level in the plots: "low", "mid", and "high".}
#'   \item{Species}{a numeric vector giving the number of plant species in the plots.}
#' }
#' @keywords datasets
#' @examples
#' with(richness,{
#'   plot(Biomass, Species,
#'        col=apply(as.matrix(pH),1,function(x) switch(x,"low"="red","mid"="black","high"="blue")),
#'        pch=apply(as.matrix(pH),1,function(x) switch(x,"low"=15,"mid"=16,"high"=17)))
#'   legend(8.5, 42, legend=c("low","mid","high"), col=c("red","black","blue"),
#'          pch=c(15,16,17), bty="n", cex=0.95, title="Soil pH level")
#' })
#' @references Crawley M.J. (2007) \emph{The R Book}. John Wiley & Sons, Chichester.
"richness"



#' @title Hill races in Scotland
#'
#' @description Each year the Scottish Hill Runners Association publishes a list of
#' hill races in Scotland for the year. These data consist of record time,
#' distance, and cumulative climb of 35 of those races. The aim of the statistical
#' analysis of these data is to explain the differences between the record time of
#' the races using their differences on distance and cumulative climb. See Agresti (2015, page 62).
#'
#' @docType data
#'
#' @usage data(races)
#'
#' @format A data frame with 35 rows and 4 variables:
#' \describe{
#'   \item{race}{a character vector giving the names of the races.}
#'   \item{distance}{a numeric vector giving the distance, in miles, of the races.}
#'   \item{cclimb}{a numeric vector giving the cumulative climb, in thousands of feet, of the races.}
#'   \item{rtime}{a numeric vector giving the record time, in minutes, of the races.}
#' }
#' @keywords datasets
#' @examples
#' races2 <- within(races,cli <- cut(cclimb, include.lowest=TRUE,
#'                                   breaks=quantile(cclimb, probs=c(0:2)/2),
#'                                   labels=c("low","high")))
#' with(races2,{
#'     plot(log(distance), log(rtime),
#'          col=apply(as.matrix(cli),1,function(x) switch(x,"low"="red","high"="blue")),
#'          pch=apply(as.matrix(cli),1,function(x) switch(x,"low"=15,"high"=16)))
#'     legend(0.7, 5.4, legend=c("low","high"), title="Cumulative climb",
#'            col=c("red","blue"), pch=c(15,16), bty="n")
#' })
#' @source \url{http://users.stat.ufl.edu/~aa/glm/data/}
#' @references Agresti A. (2015) \emph{Foundations of Linear and Generalized Linear Models}.
#' John Wiley & Sons, New Jersey.
"races"


#' @title Bladder cancer in mice
#'
#' @description Female mice were continuously fed dietary concentrations of
#' 2-Acetylaminofluorene (2-AAF), a carcinogenic and mutagenic derivative of fluorene.
#' Serially sacrificed, dead or moribund mice were examined for tumors and dates
#' of deaths were recorded. These data consist of the incidences of bladder
#' neoplasms in the mice observed during 33 months.
#'
#' @docType data
#'
#' @usage data(bladder)
#'
#' @format A data frame with 8 rows and 3 variables:
#' \describe{
#'   \item{dose}{a numeric vector giving the dose, in parts per \eqn{10^4}, of 2-AAF.}
#'   \item{exposed}{a numeric vector giving the number of mice exposed to each dose of 2-AAF.}
#'   \item{cancer}{a numeric vector giving the number of mice with bladder cancer for each dose of 2-AAF.}
#' }
#' @keywords datasets
#' @seealso \link{liver}
#' @examples
#' barplot(100*cancer/exposed ~ dose, beside=TRUE, data=bladder, col="red",
#'         xlab="Dose of 2-AAF", ylab="% of mice with bladder cancer")
#' @references Zhang H. and Zelterman D. (1999) Binary Regression for Risks in Excess of
#' Subject-Specific Thresholds. \emph{Biometrics} 55, 1247-1251.
"bladder"


#' @title Liver cancer in mice
#'
#' @description Female mice were continuously fed dietary concentrations of
#' 2-Acetylaminofluorene (2-AAF), a carcinogenic and mutagenic derivative of fluorene.
#' Serially sacrificed, dead or moribund mice were examined for tumors and dates
#' of deaths were recorded. These data consist of the incidences of liver
#' neoplasms in the mice observed during 18 months.
#'
#' @docType data
#'
#' @usage data(liver)
#'
#' @format A data frame with 8 rows and 3 variables:
#' \describe{
#'   \item{dose}{a numeric vector giving the dose, in parts per \eqn{10^4}, of 2-AAF.}
#'   \item{exposed}{a numeric vector giving the number of mice exposed to each dose of 2-AAF.}
#'   \item{cancer}{a numeric vector giving the number of mice with liver cancer for each dose of 2-AAF.}
#' }
#' @keywords datasets
#' @seealso \link{bladder}
#' @examples
#' barplot(100*cancer/exposed ~ dose, beside=TRUE, data=liver, col="red",
#'         xlab="Dose of 2-AAF", ylab="% of mice with liver cancer")
#' @references Zhang H. and Zelterman D. (1999) Binary Regression for Risks in Excess of Subject-Specific Thresholds. \emph{Biometrics} 55, 1247-1251.
"liver"


#' @title Skin cancer in women
#'
#' @description The data describe the incidence of nonmelanoma
#' skin cancer for women stratified by age in Minneapolis
#' (St. Paul) and Dallas (Fort Worth). See Kleinbaum et al. (2013, page 751).
#'
#' @docType data
#'
#' @usage data(skincancer)
#'
#' @format A data frame with 16 rows and 4 variables:
#' \describe{
#'   \item{cases}{a numeric vector giving the nonmelanoma skin cancer counts.}
#'   \item{city}{a factor giving the city to which correspond the skin cancer counts: "St.Paul" and "Ft.Worth".}
#'   \item{age}{a factor giving the age range to which correspond the skin cancer counts: "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84" and "85+".}
#'   \item{population}{a numeric vector giving the population of women.}
#' }
#' @keywords datasets
#' @examples
#' barplot(1000*cases/population ~ city + age, beside=TRUE, col=c("yellow","blue"), data=skincancer)
#' legend(1, 11, legend=c("St.Paul","Ft.Worth"), title="City",
#'        fill=c("yellow","blue"), bty="n", cex=0.9)

#' @references Kleinbaum D., Kupper L., Nizam A. and Rosenberg E.S. (2013) \emph{Applied Regression Analysis and
#' other Multivariable Methods, Fifth Edition}, Cengage Learning, Boston.
"skincancer"

#' @title Lesions of Aucuba mosaic virus
#'
#' @description The investigators counted the number of lesions
#' of \emph{Aucuba mosaic} virus developing after exposure to X
#' rays for various times. See Snedecor and Cochran (1980, page 404).
#'
#' @docType data
#'
#' @usage data(aucuba)
#'
#' @format A data frame with 7 rows and 2 variables:
#' \describe{
#'   \item{time}{ a numeric vector giving the minutes of exposure.}
#'   \item{lesions}{ a numeric vector giving the counts of lesions, in hundreds.}
#' }
#' @keywords datasets
#' @examples
#' barplot(lesions ~ time, col="red", data=aucuba)
#'
#' @references Snedecor G.W. and Cochran W.G. (1989) \emph{Statistical Methods, Eight Edition}, Iowa State University Press, Ames.
"aucuba"

#' @title Treatment for severe postnatal depression
#'
#' @description These data arose from a study on the efficacy of
#' oestrogen give transdermally for treatment of severe postnatal
#' depression. Women with major depression were randomly assigned
#' to either a placebo control group or estrogen patch group.
#' Prior to the treatment all women were assessed by
#' self-ratings of depressive symptoms on the Edinburgh
#' Postnatal Depression Scale (EPDS). The data on EPDS were
#' collected monthly for six months once the treatment began.
#' Higher scores on the EDPS are indicative of higher levels of
#' depression.

#' @docType data
#'
#' @usage data(depression)
#'
#' @format A data frame with 427 rows and 5 variables:
#' \describe{
#'   \item{subj}{ a numeric vector giving the identifier of each woman .}
#'   \item{group}{ a factor giving the received treatment: "placebo" or "estrogen".}
#'   \item{visit}{ a numeric vector giving the number of months since the treatment began, where -1 indicates the pretreatment assessment of the EDPS.}
#'   \item{dep}{ a numeric vector giving the value of the EDPS.}
#'   \item{depressd}{ a numeric vector coded as 1 when the value of the EDPS is greater than or equal to 11 and coded as 0 in other cases.}
#' }
#' @keywords datasets
#' @examples
#'  boxplot(dep ~ visit, data=subset(depression,group=="placebo"), at=c(0:6)-0.2,
#'          col="yellow", boxwex=0.25, outline=FALSE, xaxt="n", ylab="EDPS",
#'          xlab="Months since the treatment began", ylim=range(na.omit(depression$dep)))
#'  boxplot(dep ~ visit, data=subset(depression,group=="estrogen"), add=TRUE,
#'          at=c(0:6)+0.2, col="blue", boxwex=0.25, outline=FALSE, xaxt="n")
#'  axis(1, at=0:6, labels=c(-1,1:6))
#'  legend(-1, 8, legend=c("placebo","estrogen"), fill=c("yellow","blue"),
#'         bty="n", title="Group")
#'
#' @source \url{https://stats.idre.ucla.edu/spss/library/spss-librarypanel-data-analysis-using-gee/}
#' @references Gregoire A.J.P., Kumar R., Everitt B., Henderson A.F. and Studd J.W.W. (1996) Transdermal oestrogen for treatment of severe postnatal depression,
#' \emph{The Lancet} 347, 930-933.
"depression"
