#' @title amenorrhea
#'
#' @description A total of 1151 women completed menstrual diaries and
#' the diary data were used to generate binary sequence for each woman,
#' indicating whether or not she had experienced amenorrhea (the
#' absence of menstrual bleeding for a specified number of days) on
#' the day of randomization and three additional 90-day intervals. The
#' goal of this trial was to compare the two treatments (100 mg or 150
#' mg of depot-medroxyprogesterone acetate (DMPA)) in terms of how the
#' rates of amenorrhea change over time with continued use of the
#' contraceptive method.
#'
#' @docType data
#'
#' @usage data(amenorrhea)
#'
#' @format A data frame with 4604 rows and 4 variables:
#' \describe{
#'   \item{ID}{a numeric vector indicating the woman's ID.}
#'   \item{Dose}{a factor with two levels: "100mg" for treatment with 100 mg injection; and "150mg" for treatment with 150 mg injection.}
#'   \item{Time}{a numeric vector indicating the number of 90-day intervals since the trial beagn.}
#'   \item{amenorrhea}{a numeric vector indicating the amenorrhea status (1 for amenorrhea; 0 otherwise).}
#' }
#' @keywords datasets
#' @references Fitzmaurice G.M., Laird N.M., Ware J.H. (2011, page 397). \emph{Applied Longitudinal Analysis. 2nd ed.} John Wiley & Sons.
#' @references Machin D., Farley T.M., Busca B., Campbell M.J., d'Arcangues C. (1988) Assessing
#' changes in vaginal bleeding patterns in contracepting women. \emph{Contraception}, 38, 165-79.
#' @examples
#' data(amenorrhea)
#' dev.new()
#' amenorrhea2 <- aggregate(amenorrhea ~ Time + Dose,mean,data=amenorrhea,na.rm=TRUE)
#' barplot(100*amenorrhea ~ Dose+Time,data=amenorrhea2,beside=TRUE,col=c("blue","yellow"),ylab="%")
#' legend("topleft",legend=c("100 mg","150 mg"),fill=c("blue","yellow"),title="Dose",bty="n")
"amenorrhea"
#'
#' @title ldh
#'
#' @description The data consists of the proportion of lactic dehydrogenase
#' enzyme leakage obtained as a response of hepatocyte cell toxicity to the
#' effects of different combinations of carbon tetrachloride (CCl4) and
#' chloroform (CHCl3). Thus, the main objective of the data analysis is to
#' evaluate the effects of CCl4, CHCl3 and their interactions on the
#' response.
#'
#' @docType data
#'
#' @usage data(ldh)
#'
#' @format A data frame with 448 rows and 5 variables:
#' \describe{
#'   \item{LDH}{a numeric vector indicating the proportion of lactic dehydrogenase enzyme leakage, a surrogate for cell toxicity.}
#'   \item{CCl4}{a numeric vector indicating the carbon tetrachloride at 0, 1, 2.5 and 5 mM.}
#'   \item{CHCl3}{a numeric vector indicating the chloroform at 0, 5, 10 and 25 mM.}
#'   \item{Flask}{a numeric vector indicating the flask of isolated hepatocyte suspensions.}
#'   \item{Time}{a numeric vector indicating the time at 0, 0.01, 0.25, 0.50, 1, 2 and 3 hours.}
#' }
#' @keywords datasets
#' @examples
#' data(ldh)
#' opt <- unique(ldh$CCl4)
#' dev.new()
#' par(mfrow=c(1,length(opt)))
#' for(i in 1:length(opt))
#' boxplot(LDH ~ Time, data=subset(ldh,CCl4==opt[i]), ylim=c(0,0.8), main=paste("CCl4=",opt[i]))
#'
#' dev.new()
#' opt <- unique(ldh$CHCl3)
#' par(mfrow=c(1,length(opt)))
#' for(i in 1:length(opt))
#' boxplot(LDH ~ Time, data=subset(ldh,CHCl3==opt[i]), ylim=c(0,0.8), main=paste("CHCl3=",opt[i]))
#'
#' @source Gennings, C., Chinchilli, V.M., Carter, W.H. (1989). Response
#' Surface Analysis with Correlated Data: A Nonlinear Model Approach.
#' \emph{Journal of the American Statistical Association}, 84, 805–809.
#' @references Vonesh, E.F. (2012) Generalized Linear and Nonlinear Models
#' for Correlated Data: Theory and Applications Using SAS.
#' \emph{Cary, NC: SAS Institute Inc}.
"ldh"
#'
#' @title Oranges
#'
#' @description The data arose from five orange trees grown at Riverside,
#' California, during the period 1969-1973. The response is the trunk
#' circumference, in millimeters, and the predictor variable is the time,
#' in days, with an arbitrary origin taken on December 31, 1968.
#'
#' @docType data
#'
#' @usage data(Oranges)
#'
#' @format A data frame with 35 rows and 3 variables:
#' \describe{
#'   \item{Trunk}{a numeric vector indicating the trunk circumference, in millimeters.}
#'   \item{Days}{a numeric vector indicating the time, in days, since December 31, 1968.}
#'   \item{Tree}{a numeric vector with the identifier of each orange tree.}
#' }
#' @keywords datasets
#' @examples
#' dev.new()
#' data(Oranges)
#' with(Oranges,plot(Days, Trunk, pch=16, col="blue"))
#' @references Draper N., Smith H. (1998) Applied Regression Analysis, Third Edition. \emph{John Wiley & Sons}.
"Oranges"
#'
#'
#' @title Seizures
#'
#' @description The dataset reports the number of epileptic seizures in each
#' of four two-week intervals, and in a baseline eight-week inverval, for
#' Progabide treatment and placebo groups with a total of 59 individuals.
#'
#' @docType data
#'
#' @usage data(Seizures)
#'
#' @format A data frame with 236 rows and 6 variables:
#' \describe{
#'   \item{seizures}{a numeric vector indicating the number of epileptic seizures.}
#'   \item{treatment}{a factor indicating the applied treatment: "Progabide" and "Placebo".}
#'   \item{base}{a numeric vector indicating the number of epileptic seizures in the baseline eight-week inverval.}
#'   \item{age}{a numeric vector indicating the age of the individuals.}
#'   \item{time}{a numeric vector indicating which the two-week interval corresponds to the reported number of epileptic seizures.}
#'   \item{id}{a numeric vector indicating the identifier of each individual.}
#' }
#' @keywords datasets
#' @examples
#' dev.new()
#' data(Seizures)
#' boxplot(seizures ~ treatment:time, data=Seizures, ylim=c(0,25), col=c("blue","yellow"))
#' @source Thall P.F., Vail S.C. (1990) Some covariance models for longitudinal count data with overdispersion. \emph{Biometrics} 46:657–671.
#' @references Carey V.J., Wang Y.-G. (2011) Working covariance model selection for generalized estimating equations. \emph{Statistics in Medicine} 30:3117–3124.
#' @references Fu L., Hao Y., Wang Y.-G. (2018) Working correlation structure selection in generalized estimating equations. \emph{Computational Statistics & Data Analysis} 33:983–996.
#' @references Diggle P.J., Liang K.Y., Zeger S.L. (1994, page 166) \emph{Analysis of Longitudinal Data}. Clarendon Press.
"Seizures"
#'
#' @title Fabric faults
#'
#' @description The main objective of the analysis of this dataset is to assess
#' if there is an association between the number of faults in rolls of fabric and
#' their length.
#'
#' @docType data
#'
#' @usage data(fabric)
#'
#' @format A data frame with 32 rows and 2 variables:
#' \describe{
#'   \item{roll}{a numeric vector indicating the length of the rolls.}
#'   \item{faults}{a numeric vector indicating the number of faults.}
#' }
#' @keywords datasets
#' @examples
#' dev.new()
#' data(fabric)
#' with(fabric,plot(roll, faults, pch=16, xlab="Length of roll", ylab="Number of faults"))
#' @references Hinde J., Demetrio C.G.B. (1998) Over-dispersion: models and estimation. \emph{Computational Statistics & Data Analysis} 27:151–170.
"fabric"
#'
#' @title Discount coupons
#'
#' @description The market research department of a soft drink manufacturer is
#' investigating the effectiveness of a price discount coupon on the purchase
#' of a two-litre beverage product. A sample of 5500 costumers received coupons
#' for varying price discounts between 5 and 25 cents. The main objective of
#' the analysis is to determine if there is an effect of the price discount on
#' the proportion of redeemed coupons after one month.
#'
#' @docType data
#'
#' @usage data(coupons)
#'
#' @format A data frame with 11 rows and 3 variables:
#' \describe{
#'   \item{discounts}{a numeric vector indicating the price discount, in cents.}
#'   \item{costumers}{a numeric vector indicating the number of customers who received coupons.}
#'   \item{redeemed}{a numeric vector indicating the number of redeemed coupons.}
#' }
#' @keywords datasets
#' @examples
#' dev.new()
#' data(coupons)
#' barplot(100*redeemed/costumers ~ discounts, data=coupons, xlab="Discount price",
#'         ylab="(%) Redeemed coupons", col="blue")
#' @references Montgomery D.C., Peck E.A., Vining G. (2012, page 464) \emph{Introduction to linear regression analysis. 5th ed.} Berlin, Wiley.
"coupons"
#'
#' @title Hardened Steel
#'
#' @description This dataset consists of the failure times for hardened steel
#' specimens in a rolling contact fatigue test. Ten independent observations
#' were taken at each of the four values of contact stress. The response is
#' the length of the time until each specimen of the hardened steel failed.
#'
#' @docType data
#'
#' @usage data(Steel)
#'
#' @format A data frame with 40 rows and 2 variables:
#' \describe{
#'   \item{stress}{a numeric vector indicating the values of contact stress, in pounds per square inch x \eqn{10^{-6}}.}
#'   \item{life}{a numeric vector indicating the length of the time until the specimen of the hardened steel failed.}
#' }
#' @keywords datasets
#' @examples
#' dev.new()
#' data(Steel)
#' with(Steel,plot(log(stress), log(life), pch=16, xlab="Log(Stress)", ylab="log(Life)"))
#' @references McCool J. (1980) Confidence limits for Weibull regression with censored data. \emph{ Transactions on Reliability} 29:145-150.
"Steel"
#'
#' @title Advertising
#'
#' @description The Advertising data set consists of the sales of that product in
#' 200 different markets, along with advertising budgets for the product in each
#' of those markets for three different media: TV, radio, and newspaper.
#'
#' @docType data
#'
#' @usage data(advertising)
#'
#' @format A data frame with 200 rows and 4 variables:
#' \describe{
#'   \item{TV}{a numeric vector indicating the advertising budget on TV.}
#'   \item{radio}{a numeric vector indicating the advertising budget on radio.}
#'   \item{newspaper}{a numeric vector indicating the advertising budget on newspaper.}
#'   \item{sales}{a numeric vector indicating the sales of the interest product.}
#' }
#' @keywords datasets
#' @source \url{https://www.statlearning.com/s/Advertising.csv}
#' @examples
#' data(advertising)
#' breaks <- with(advertising,quantile(radio,probs=c(0:3)/3))
#' labels <- c("low","mid","high")
#' advertising2 <- within(advertising,radioC <- cut(radio,breaks,labels,include.lowest=TRUE))
#' dev.new()
#' with(advertising2,plot(TV,sales,pch=16,col=as.numeric(radioC)))
#' legend("topleft",legend=c("low","mid","high"),fill=c(1:3),title="Radio",bty="n")
#' @references James G., Witten D., Hastie T., Tibshirani R. (2013, page 15) \emph{An Introduction to Statistical Learning with Applications in R}, Springer, New York.
"advertising"
#'
#'
#' @title Alaska pipeline
#'
#' @description The Alaska pipeline data consists of in-field ultrasonic measurements of
#' the depths of defects in the Alaska pipeline. The depth of the defects were then
#' re-measured in the laboratory. These measurements were performed in six different
#' batches. The data were analyzed to calibrate the bias of the field measurements
#' relative to the laboratory measurements. In this analysis, the field measurement
#' is the response variable and the laboratory measurement is the predictor variable.
#'
#' @docType data
#'
#' @usage data(pipeline)
#'
#' @format A data frame with 107 rows and 2 variables:
#' \describe{
#'   \item{Field}{a numeric vector indicating the number of defects measured in the field.}
#'   \item{Lab}{a numeric vector indicating the number of defects measured in the laboratory.}
#' }
#' @keywords datasets
#' @source \url{https://www.itl.nist.gov/div898/handbook/pmd/section6/pmd621.htm}
#' @examples
#' data(pipeline)
#' dev.new()
#' xlab <- "In-laboratory measurements"
#' ylab <- "In-field measurements"
#' with(pipeline,plot(Lab,Field,pch=20,xlab=xlab,ylab=ylab))
#' @references Weisberg S. (2005). \emph{Applied Linear Regression}, 3rd edition. Wiley, New York.
"pipeline"
#'
#'
#' @title Dilution Assay
#'
#' @description These data are counts of virus
#' particles at 5 different dilutions. There are 4
#' replicate counts at each dilution except the last
#' for which there are 5 counts. The aim is to
#' estimate the number of virus particles per unit volume.
#'
#' @docType data
#'
#' @usage data(dilution)
#'
#' @format A data frame with 21 rows and 2 variables:
#' \describe{
#'   \item{Count}{a numeric vector indicating the count of virus particles.}
#'   \item{Dilution}{a numeric vector indicating the dilution volume.}
#' }
#' @keywords datasets
#' @source https://sada2013.sciencesconf.org/16138/glmSession4_Cotonou.pdf
#' @examples
#' data(dilution)
#' xlab <- "Dilution volume"
#' ylab <- "Count of virus particles"
#' dev.new()
#' with(dilution,plot(Dilution,Count,pch=20,xlab=xlab,ylab=ylab))
"dilution"
#'
#' @title Mammal brain and body weights
#'
#' @description These data corresponds to the (average) body weight and the
#' (average) brain weight for sixty-two species of mammals.
#'
#' @docType data
#'
#' @usage data(brains)
#'
#' @format A data frame with 62 rows and 3 variables:
#' \describe{
#'   \item{Specie}{a character string giving the species name.}
#'   \item{BrainWt}{a numeric vector indicating the average brain weight, in grams.}
#'   \item{BodyWt}{a numeric vector indicating the average body weight, in kilograms.}
#' }
#' @keywords datasets
#' @examples
#' data(brains)
#' xlab <- "log(Body Weight)"
#' ylab <- "log(Brain Weight)"
#' dev.new()
#' with(brains,plot(log(BodyWt),log(BrainWt),pch=20,xlab=xlab,ylab=ylab))
#' @references Allison T., Cicchetti D. (1976). Sleep in mammals: Ecology and constitutional correlates. \emph{Science} 194:732-734.
#' @references Weisberg S. (2005). \emph{Applied Linear Regression}, 3rd edition. Wiley, New York.
"brains"
#'
#' @title Dental Clinical Trial
#'
#' @description These data arose from a dental clinical study. In this trial, subjects were generally healthy adult
#' male and female volunteers, ages 18–55, with pre-existing plaque but without advanced periodontal
#' disease. Prior to entry, subjects were screened for a minimum of 20 sound, natural teeth and a minimum mean plaque index of 2.0. Subjects with gross oral pathology or on antibiotic, antibacterial,
#' or anti-inflammatory therapy were excluded from the study. One hundred nine volunteers were randomized in a double-blinded way to one of two new mouth rinses (A and B) or to a control mouth
#' rinse. Plaque was scored at baseline, at 3 months, and at 6 months by the Turesky modification of
#' the Quigley-Hein index, a continuous measure. Four subjects had missing plaque scores. The main
#' objective of the analysis is to measure the effectiveness of the three mouth rinses in inhibiting the
#' development of dental plaque.
#'
#' @docType data
#'
#' @usage data(rinse)
#'
#' @format A data frame with 315 rows and 7 variables:
#' \describe{
#'   \item{subject}{a character string giving the identifier of the volunteer.}
#'   \item{gender}{a factor indicating the gender of the volunteer: "Female" and "Male".}
#'   \item{age}{a numeric vector indicating the age of the volunteer.}
#'   \item{rinse}{a factor indicating the type of rinse used by the volunteer: "Placebo", "A" and "B".}
#'   \item{smoke}{a factor indicating if the volunteer smoke: "Yes" and "No".}
#'   \item{time}{a numeric vector indicating the time (in months) since the treatment began.}
#'   \item{score}{a numeric vector giving the subject’s score of plaque.}
#' }
#' @keywords datasets
#' @examples
#' data(rinse)
#' dev.new()
#' boxplot(score ~ time, data=subset(rinse,rinse=="Placebo"),ylim=c(0,3.5),
#'         at=c(1:3)-0.2, col="yellow", xaxt="n", boxwex=0.15)
#' boxplot(score ~ time, data=subset(rinse,rinse=="A"), add=TRUE,
#'         at=c(1:3), col="gray", xaxt="n", boxwex=0.15)
#' boxplot(score ~ time, data=subset(rinse,rinse=="B"), add=TRUE,
#'         at=c(1:3) + 0.2, col="blue", xaxt="n", boxwex=0.15)
#' axis(1, at=c(1:3), labels=unique(rinse$time))
#' legend("bottomleft",legend=c("placebo","rinse A","rinse B"),
#'        title="Treatment",fill=c("yellow","gray","blue"),bty="n")
#' @references Hadgu A., Koch G. (1999) Application of generalized estimating equations
#' to a dental randomized clinical trial. \emph{Journal of Biopharmaceutical Statistics} 9:161-178.
"rinse"
#'
#' @title Roots Produced by the Columnar Apple Cultivar Trajan.
#'
#' @description The data arose from a horticultural experiment to study the number of roots produced by 270
#' micropropagated shoots of the columnar apple cultivar Trajan.
#' During the rooting period, all shoots were maintained under identical conditions, but the shoots
#' themselves were cultured on media containing different concentrations of the cytokinin
#' 6-benzylaminopurine (BAP), in growth cabinets with an 8 or 16 hour photoperiod. The objective
#' is to assess the effect of both the photoperiod and the concentration levels of BAP on the
#' number of roots produced.
#'
#' @docType data
#'
#' @usage data(Trajan)
#'
#' @format A data frame with 270 rows and 4 variables:
#' \describe{
#'   \item{roots}{a numeric vector indicating the number of roots produced.}
#'   \item{shoot}{a numeric vector indicating the number of micropropogated shoots.}
#'   \item{photoperiod}{a factor indicating the photoperiod, in hours: 8 or 16.}
#'   \item{bap}{a numeric vector indicating the concentrations of the cytokinin 6-benzylaminopurine: 2.2, 4.4, 8.8 or 17.6.}
#' }
#' @source \url{https://support.sas.com/rnd/app/stat/examples/GENMODZIP/sas.html}
#' @keywords datasets
#' @examples
#' data(Trajan)
#' dev.new()
#' boxplot(roots ~ bap, data=subset(Trajan,photoperiod=="8"), at=c(1:4) - 0.15,
#'         col="blue", boxwex=0.2, xaxt="n", ylim=c(-0.5,17))
#' boxplot(roots ~ bap, data=subset(Trajan,photoperiod=="16"), add=TRUE,
#'         at=c(1:4) + 0.15, col="yellow", boxwex=0.2, xaxt="n")
#' axis(1, at=c(1:4), labels=levels(Trajan$bap))
#' legend("topright", legend=c("8","16"), title="Photoperiod", bty="n",
#'        fill=c("blue","yellow"))
#'
#' @references Ridout M., Demétrio C.G., Hinde J. (1998). Models for count data with many zeros. In
#' \emph{Proceedings of the XIXth international biometric conference}, 179–192.
#' @references Ridout M., Hinde J., Demétrio C.G. (2001). A score test for testing a zero-inflated
#' Poisson regression model against zero-inflated negative binomial alternatives. \emph{Biometrics}
#' 57:219-223.
#' @references Garay A.M., Hashimoto E.M., Ortega E.M.M., Lachos V. (2011). On estimation and
#' influence diagnostics for zero-inflated negative binomial regression models. \emph{Computational
#' Statistics & Data Analysis} 55:1304-1318.
"Trajan"

#'
#' @title Urinary Tract Infections in HIV-infected Men
#'
#' @description These data arose from a study conducted in the Department of
#' Internal Medicine at the Utrecht University Hospital, the Netherlands, where
#' 98 human immunodeficiency virus (HIV)-infected men were followed up to two
#' years. Urinary cultures were obtained during the first visit and every six
#' months thereafter. Also, cultures were obtained between regular scheduled
#' visits when signs and symptoms of urinary tract infections (UTI) occurred,
#' or when patients had fever of unknown origin. CD4+ cell counts were also
#' measured. A CD4+ count is a blood test to determine how well the immune
#' system is working in people who have been diagnosed with HIV. In general,
#' a decreasing CD4+ count is an indication of the progression of HIV. See
#' Hoepelman et al. (1992), van den Broek (1995), Morel and Nagaraj (2012, page 175).
#'
#' @docType data
#'
#' @usage data(uti)
#'
#' @format A data frame with 98 rows and 3 variables:
#' \describe{
#'   \item{episodes}{a numeric vector indicating the number of episodes, that is, the number of times each patient had urinary tract infections (UTI).}
#'   \item{time}{a numeric vector indicating the time to follow up, in months.}
#'   \item{cd4}{a numeric vector indicating the immune status of the patient as measured by the CD4+ cell counts.}
#' }
#' @keywords datasets
#' @examples
#' data(uti)
#' dev.new()
#' uti2 <- within(uti,cd4C <- cut(log(cd4),4,labels=c("low","mid-low","mid-high","high")))
#' out <- aggregate(cbind(episodes,time) ~ cd4C, sum, data=uti2)
#' barplot(12*episodes/time ~ cd4C, beside=TRUE, data=out, col="red",
#'         xlab="CD4+ cell count", ylab="Number of UTIs per year")
#'
#' @references Hoepelman A.I.M., Van Buren M., Van den Broek J., Borleffs J.C.C. (1992) Bacteriuria
#' in men infected with HIV-1 is related to their immune status (CD4+ cell count). \emph{AIDS} 6:179-184.
#' @references Morel J.G., Nagaraj N.K. (2012) \emph{Overdispersion Models in SAS}. SAS Institute Inc.,
#' Cary, North Carolina, USA.
#' @references van den Broek J. (1995) A Score Test for Zero Inflation in a Poisson Distribution.
#' \emph{Biometrics} 51:738–743.
"uti"
#'
#'
#' @title Shoulder Pain after Laparoscopic Cholecystectomy
#'
#' @description Inflation of the abdomen during laparoscopic cholecystectomy
#' (removal of the gallbladder) separates the liver from the diaphragm and
#' places strain on the attachments that connect both. This strain is felt as
#' referred pain in the shoulder. Suction to remove residual gas may reduce
#' shoulder pain. There were 22 subjects randomized in the active group (with
#' abdominal suction) and 19 subjects randomized in the control group (without
#' abdominal suction). After laparoscopic surgery, patients were asked to rate
#' their shoulder pain on a visual analog scale morning and afternoon for three
#' days after the operation (a total of six different times). The scale was
#' coded into five ordered categories where a pain score of 1 indicated "low pain"
#' and a score of 5 reflected "high pain". See Jorgensen et al. (1995),
#' Lumley (1996), Morel and Nagaraj (2012, page 319).
#'
#' @docType data
#'
#' @usage data(cholecystectomy)
#'
#' @format A data frame with 246 rows and 7 variables:
#' \describe{
#'   \item{id}{a numeric vector with the identifier of the patient.}
#'   \item{treatment}{a factor indicating the treatment received by the patient: abdominal suction ("A") and placebo ("P").}
#'   \item{gender}{a factor indicating the gender of the patient: female ("F") and male ("M").}
#'   \item{age}{a numeric vector indicating the age of the patient, in years.}
#'   \item{time}{a numeric vector indicating the occasion the patient was asked to rate their shoulder pain after the laparoscopic surgery: integers from 1 to 6.}
#'   \item{pain}{a numeric vector indicating the shoulder pain rated by the patient on a scale coded into five ordered categories, where
#'               1 indicated "low pain" and 5 reflected "high pain".}
#'   \item{pain2}{a numeric vector indicating the shoulder pain rated by the patient and coded as 1 for the two first categories of
#'                pain and 0 for other cases.}
#' }
#' @keywords datasets
#' @examples
#' data(cholecystectomy)
#' out <- aggregate(pain2 ~ treatment + time, data=cholecystectomy, mean)
#' dev.new()
#' barplot(100*pain2 ~ treatment + time, beside=TRUE, data=out, xlab="Time",
#'         col=c("yellow","blue"), ylab="% of patients with low pain")
#' legend("topleft", c("Placebo","Abdominal suction"), fill=c("yellow","blue"),
#'        title="Treatment", cex=0.9, bty="n")
#'
#' @references Jorgensen J.O., Gillies R.B., Hunt D.R., Caplehorn J.R.M., Lumley T. (1995)
#' A simple and effective way to reduce postoperative pain after laparoscopic cholecystectomy.
#' \emph{Australian and New Zealand Journal of Surgery} 65:466–469.
#' @references Lumley T. (1996) Generalized Estimating Equations for Ordinal Data: A Note on Working Correlation Structures.
#' \emph{Biometrics} 52:354–361.
#' @references Morel J.G., Nagaraj N.K. (2012) \emph{Overdispersion Models in SAS}. SAS Institute Inc.,
#' Cary, North Carolina, USA.
"cholecystectomy"
#'
#'
#' @title Germination of Orobanche Seeds
#'
#' @description These data arose from a study of the germination of two species
#' of Orobanche seeds (O. aegyptiaca 75 and O. aegyptiaca 73) grown on 1/125
#' dilutions of two different root extract media (cucumber and bean) in a 2×2
#' factorial layout with replicates. The data consist of the number of seeds
#' and the number germinating for each replicate. Interest focusses on the
#' possible differences in germination rates for the two types of seed and
#' root extract and whether there is any interaction. See Crowder (1978),
#' Hinde and Demetrio (1998).
#'
#' @docType data
#'
#' @usage data(orobanche)
#'
#' @format A data frame with 21 rows and 4 variables:
#' \describe{
#'   \item{specie}{a factor indicating the specie of Orobanche seed: O. aegyptiaca 75 ("Aegyptiaca 75") and O. aegyptiaca 73 ("Aegyptiaca 73").}
#'   \item{extract}{a factor indicating the root extract: cucumber ("Cucumber") and bean ("Bean").}
#'   \item{seeds}{a numeric vector indicating the total number of seeds.}
#'   \item{germinated}{a numeric vector indicating the number of germinated seeds.}
#' }
#' @keywords datasets
#' @examples
#' data(orobanche)
#' out <- aggregate(cbind(germinated,seeds) ~ extract + specie, data=orobanche, sum)
#' dev.new()
#' barplot(100*germinated/seeds ~ extract + specie, beside=TRUE, data=out, width=0.3,
#'         col=c("yellow","blue"), xlab="Specie", ylab="% of germinated seeds")
#' legend("topleft",c("Bean","Cucumber"),fill=c("yellow","blue"),title="Extract",bty="n")
#'
#' @references Crowder M.J. (1978) Beta-binomial anova for proportions. \emph{Journal of the Royal Statistical Society.
#' Series C (Applied Statistics)} 27:34-37.
#' @references Hinde J., Demetrio C.G.B. (1998) Overdispersion: Models and estimation. \emph{Computational Statistics
#' & Data Analysis} 27:151-170.
"orobanche"
#'
#'
#' @title Guidelines for Urinary Incontinence Discussion and Evaluation
#'
#' @description These data arose from a randomized controlled trial that
#' assessed if provider adherence to a set of guidelines for treatment of
#' patients with urinary incontinence (UI) affected patient outcomes. Data
#' were collected on 137 elderly patients from 38 medical practices. The
#' number of patients per practice ranged from 1 to 8 and the median was
#' 4 patients. The interest of the present analysis is to determine what
#' predicts whether or not a patient considers their UI a problem that
#' interferes with him/her daily life.
#'
#' @docType data
#'
#' @usage data(GUIDE)
#'
#' @format A data frame with 137 rows and 7 variables:
#' \describe{
#'   \item{bothered}{a numeric vector giving the answer to the following: Do you consider this accidental loss of urine a problem that interferes with your day to day activities or bothers you in other ways? 1 for "Yes" and 0 for "No".}
#'   \item{gender}{a factor giving the patient's gender: "Male" or "Female".}
#'   \item{age}{a numeric vector giving the standardized age: (age in years - 76)/10.}
#'   \item{dayacc}{a numeric vector giving the patient's report of the number of leaking accidents they experience in an average day (derived from number of accidents reported per week).}
#'   \item{severe}{a factor giving the severity of the loss of urine: "1" if there is only some moisture; "2" if the patient wet the underwear; "3" if the urine trickled down the thigh; and "4" if the patient wet the floor.}
#'   \item{toilet}{a numeric vector giving the patient's report on the number of times during the day he (or she) usually go to the toilet to urinate.}
#'   \item{practice}{a character string giving the identifier of the medical practice.}
#' }
#' @keywords datasets
#' @examples
#' data(GUIDE)
#' mod <- bothered ~ gender + age + dayacc + severe + toilet
#' fit <- glmgee(mod, family=binomial(logit), id=practice, corstr="Exchangeable", data=GUIDE)
#' summary(fit)
#' @references Hammill B.G., Preisser J.S. (2006) A SAS/IML software program for GEE and regression diagnostics. \emph{Computational Statistics & Data Analysis} 51:1197-1212.
#' @references Jung K.-M. (2008) Local Influence in Generalized Estimating Equations. \emph{Scandinavian Journal of Statistics} 35:286-294.
#' @source \url{http://www.bios.unc.edu/~preisser/personal/uidata/preqaq99.dat}
"GUIDE"

#'
#' @title Effect of ozone-enriched atmosphere on growth of sitka spruces
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
#' @usage data(spruces)
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
#' data(spruces)
#' dev.new()
#' boxplot(size ~ days, data=subset(spruces,treat=="normal"), at=c(1:13) - 0.2,
#'         col="yellow", boxwex=0.3, xaxt="n", xlim=c(0.9,13.1))
#' boxplot(size ~ days, data=subset(spruces,treat=="ozone-enriched"), add=TRUE,
#'         at=c(1:13) + 0.2, col="blue", boxwex=0.3, xaxt="n")
#' axis(1, at=c(1:13), labels=unique(spruces$days))
#' axis(2, at=seq(0,2000,250), labels=seq(0,2000,250))
#' legend("topleft", legend=c("normal","ozone-enriched"), fill=c("yellow","blue"),
#'        title="Atmosphere", bty="n")
#'
#' @references Diggle P.J., Heagarty P., Liang K.-Y., Zeger S.L. (2002) \emph{Analysis of Longitudinal Data}. Oxford University Press, Oxford.
#' @references Crainiceanu C.M., Ruppert D., Wand M.P. (2005). Bayesian Analysis for Penalized Spline Regression Using WinBUGS. \emph{Journal of Statistical Software} 14(14):1-24.
"spruces"

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
#' data(cellular)
#' dev.new()
#' barplot(100*cells/200 ~ ifn + tnf, beside=TRUE, data=cellular, col=terrain.colors(4),
#'         xlab="Dose of TNF", ylab="% of cells with markers of differentiation")
#' legend("topleft", legend=c("0","4","20","100"), fill=terrain.colors(4),
#'        title="Dose of IFN", bty="n")
#' @references Piegorsch W.W., Weinberg C.R., Margolin B.H. (1988) Exploring
#' simple independent action in multifactor tables of proportions. \emph{Biometrics} 44:595-603.
#'
#' Vanegas, L.H. and Rondon, L.M. (2020) A data transformation to deal with
#' constant under/over-dispersion in binomial and poisson regression models. \emph{Journal
#' of Statistical Computation and Simulation} 90:1811-1833.
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
#' data(ossification)
#' dev.new()
#' boxplot(100*fetuses/litter ~ pht, data=subset(ossification,tcpo=="0 mg/kg"),
#'         at=c(1:2) - 0.2, col="yellow", boxwex=0.25, xaxt="n",
#'         xlab="Dose of PHT", ylab="% of fetuses showing ossification")
#' boxplot(100*fetuses/litter ~ pht, data=subset(ossification,tcpo=="100 mg/kg"),
#'         add=TRUE, at=c(1:2) + 0.2, col="blue", boxwex=0.25, xaxt="n")
#' axis(1, at=c(1:2), labels=levels(ossification$pht))
#' legend("bottomleft", legend=c("0 mg/kg","100 mg/kg"), fill=c("yellow","blue"),
#'        title="Dose of TCPO", bty="n", cex=0.9)
#' @references Morel J.G., Neerchal N.K. (1997) Clustered binary logistic regression in teratology data
#' using a finite mixture distribution. \emph{Statistics in Medicine} 16:2843-2853.
#'
#' Morel J.G., Nagaraj N.K. (2012) \emph{Overdispersion Models in SAS}. SAS Institute Inc., Cary, North Carolina, USA.
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
#' data(swimmers)
#' dev.new()
#' boxplot(infections ~ frequency, data=subset(swimmers,location=="non-beach"),
#'         at=c(1:2) - 0.2, col="yellow", boxwex=0.25, xaxt="n")
#' boxplot(infections ~ frequency, data=subset(swimmers,location=="beach"), add=TRUE,
#'         at=c(1:2) + 0.2, col="blue", boxwex=0.25, xaxt="n")
#' axis(1, at=c(1:2), labels=levels(swimmers$frequency))
#' legend("topleft", title="Location",legend=c("non-beach","beach"),
#'        fill=c("yellow","blue"),bty="n")
#' @references Hand D.J., Daly F., Lunn A.D., McConway K.J., Ostrowsky E. (1994)
#' \emph{A Handbook of Small Data Sets}, Chapman and Hall, London.
#'
#' Vanegas L.H., Rondon L.M. (2020) A data transformation to deal with
#' constant under/over-dispersion in binomial and poisson regression models. \emph{Journal
#' of Statistical Computation and Simulation} 90:1811-1833.
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
#' data(mammary)
#' dev.new()
#' boxplot(tumors ~ group, data=mammary, outline=FALSE, xlab="Group",
#'         ylab="Number of tumors", col=c("yellow","blue"))
#' @references Lawless J.F. (1987) Regression Methods for Poisson Process Data. \emph{Journal of the American
#' Statistical Association} 82:808-815.
#'
#' Morel J.G., Nagaraj N.K. (2012) \emph{Overdispersion Models in SAS}. SAS Institute Inc., Cary, North Carolina, USA.
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
#' data(richness)
#' dev.new()
#' with(richness,plot(Biomass,Species,col=as.numeric(pH),pch=16))
#' legend("topright", legend=c("low","mid","high"), col=c(1:3), pch=16,
#'        title="pH level", bty="n")
#' @references Crawley M.J. (2007) \emph{The R Book}. John Wiley & Sons, Chichester.
"richness"
#'
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
#' data(races)
#' breaks <- with(races,quantile(cclimb,probs=c(0:2)/2))
#' labels <- c("low","high")
#' races2 <- within(races,cli <- cut(cclimb,include.lowest=TRUE,breaks,labels))
#' dev.new()
#' with(races2,plot(log(distance),log(rtime),pch=16,col=as.numeric(cli)))
#' legend("topleft", legend=c("low","high"), title="Cumulative climb",
#'        col=c(1:2), pch=16, bty="n")
#' @references Agresti A. (2015) \emph{Foundations of Linear and Generalized Linear Models}.
#' John Wiley & Sons, New Jersey.
"races"
#'
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
#' data(bladder)
#' dev.new()
#' barplot(100*cancer/exposed ~ dose, beside=TRUE, data=bladder, col="red",
#'         xlab="Dose of 2-AAF", ylab="% of mice with bladder cancer")
#' @references Zhang H., Zelterman D. (1999) Binary Regression for Risks in Excess of
#' Subject-Specific Thresholds. \emph{Biometrics} 55:1247-1251.
"bladder"
#'
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
#' data(liver)
#' dev.new()
#' barplot(100*cancer/exposed ~ dose, beside=TRUE, data=liver, col="red",
#'         xlab="Dose of 2-AAF", ylab="% of mice with liver cancer")
#' @references Zhang H., Zelterman D. (1999) Binary Regression for Risks in Excess of Subject-Specific Thresholds. \emph{Biometrics} 55:1247-1251.
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
#' data(skincancer)
#' dev.new()
#' barplot(1000*cases/population ~ city + age, beside=TRUE, col=c("yellow","blue"),
#'         data=skincancer)
#' legend("topleft", legend=c("St.Paul","Ft.Worth"), title="City",
#'        fill=c("yellow","blue"), bty="n")
#'
#'
#' @references Kleinbaum D., Kupper L., Nizam A., Rosenberg E.S. (2013) \emph{Applied Regression Analysis and
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
#' data(aucuba)
#' dev.new()
#' barplot(lesions ~ time, col="red", data=aucuba)
#'
#' @references Snedecor G.W., Cochran W.G. (1989) \emph{Statistical Methods, Eight Edition}, Iowa State University Press, Ames.
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
#'   \item{subj}{ a numeric vector giving the identifier of each woman.}
#'   \item{group}{ a factor giving the received treatment: "placebo" or "oestrogen".}
#'   \item{visit}{ a numeric vector giving the number of months since the treatment began, where -1 indicates the pretreatment assessment of the EDPS.}
#'   \item{dep}{ a numeric vector giving the value of the EDPS.}
#'   \item{depressd}{ a numeric vector coded as 1 when the value of the EDPS is greater than or equal to 11 and coded as 0 in other cases.}
#' }
#' @keywords datasets
#' @examples
#' data(depression)
#' dev.new()
#' boxplot(dep ~ visit, data=subset(depression,group=="placebo"), at=c(0:6) - 0.2,
#'         col="yellow", boxwex=0.3, xaxt="n", ylim=range(na.omit(depression$dep)),
#'         xlab="Months since the treatment began", ylab="EDPS")
#' boxplot(dep ~ visit, data=subset(depression,group=="oestrogen"), add=TRUE,
#'         at=c(0:6) + 0.2, col="blue", boxwex=0.3, xaxt="n")
#' axis(1, at=c(0:6), labels=c(-1,1:6))
#' legend("bottomleft", legend=c("placebo","oestrogen"), fill=c("yellow","blue"),
#'        title="Treatment", bty="n")
#'
#' @source \url{https://stats.oarc.ucla.edu/spss/library/spss-librarypanel-data-analysis-using-gee/}
#' @references Gregoire A.J.P., Kumar R., Everitt B., Henderson A.F., Studd, J.W.W. (1996) Transdermal oestrogen for treatment of severe postnatal depression,
#' \emph{The Lancet} 347:930-933.
"depression"

#' @title Shelf life of a photographic developer
#'
#' @description These data arise from an experiment using accelerated
#' life testing to determine the estimated shelf life of a photographic
#' developer. The maximum density as well as the temperature seem to
#' be good indicators of overall developer/film performance.
#'
#' @docType data
#'
#' @usage data(shelflife)
#'
#' @format A data frame with 21 rows and 3 variables:
#' \describe{
#'   \item{Time}{ a numeric vector giving the shelf life, in hours.}
#'   \item{Temp}{ a factor giving the temperature, in degrees celsius.}
#'   \item{Dmax}{ a numeric vector giving the maximum density.}
#' }
#' @keywords datasets
#' @examples
#' data(shelflife)
#' dev.new()
#' with(shelflife,plot(Dmax, Time, pch=16, col=as.numeric(Temp)))
#' legend("topright", legend=c("72C","82C","92C"), col=c(1:3), pch=16,
#'        title="Temperature", bty="n")
#'
#' @references Chapman, R.E. (1997) Degradation study of a photographic developer to determine shelf life,
#' \emph{Quality Engineering} 10:1, 137-140.
"shelflife"
