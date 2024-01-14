#'
#' @title Calls to a technical support help line
#'
#' @description Data on technical support calls after a product release. Using
#' this information, new products could be allocated technical support
#' resources.
#'
#' @docType data
#'
#' @usage data(calls)
#'
#' @format A data frame with 16 rows and 2 variables:
#' \describe{
#'   \item{week}{a numeric vector indicating number of weeks that have elapsed since the product’s release.}
#'   \item{calls}{a numeric vector indicating the number of technical support calls.}
#' }
#' @keywords datasets
#' @source \url{https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.3/statug/statug_mcmc_examples12.htm}
#' @examples
#' data(calls)
#' dev.new()
#' with(calls,plot(week,calls,xlab="The number of weeks since the release of the product",
#'      pch=16,col="blue",ylab="Technical support calls"))
"calls"
#'
#'
#' @title Age and Eye Lens Weight of Rabbits in Australia
#'
#' @description The dry weight of the eye lens was measured for 71 free-living wild rabbits of known age. Eye
#' lens weight tends to vary much less with environmental conditions than does total body weight, and
#' therefore may be a much better indicator of age.
#'
#' @docType data
#'
#' @usage data(rabbits)
#'
#' @format A data frame with 71 rows and 2 variables:
#' \describe{
#'   \item{age}{a numeric vector indicating the rabbit age, in days.}
#'   \item{wlens}{a numeric vector indicating the dry weight of eye lens, in milligrams.}
#' }
#' @keywords datasets
#' @references Dudzinski M.L., Mykytowycz R. (1961) The eye lens as an indicator of age in
#'  the wild rabbit in Australia. \emph{CSIRO Wildlife Research}, 6, 156-159.
#' @references Ratkowsky D.A. (1983). \emph{Nonlinear Regression Modelling}. Marcel Dekker, New York.
#' @references Wei B.C. (1998). \emph{Exponential Family Nonlinear Models}. Springer, Singapore.
#' @examples
#' data(rabbits)
#' dev.new()
#' with(rabbits,plot(age,wlens,xlab="Age (in days)",pch=16,col="blue",
#'                   ylab="Dry weight of eye lens (in milligrams)"))
"rabbits"
#'
#'
#' @title amenorrhea
#'
#' @description A total of 1151 women completed menstrual diaries. The
#' diary data were used to generate a binary sequence for each woman,
#' indicating whether or not she had experienced amenorrhea (the absence
#' of menstrual bleeding for a specified number of days) on the day of
#' randomization and three additional 90-day intervals. The goal of
#' this trial was to compare the two treatments (100 mg or 150 mg of
#' depot-medroxyprogesterone acetate (DMPA)) in terms of how the rates
#' of amenorrhea change over time with continued use of the contraceptive
#' method.
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
#' @references Vonesh E.F. (2012) Generalized Linear and Nonlinear Models
#' for Correlated Data: Theory and Applications Using SAS.
#' \emph{Cary, NC: SAS Institute Inc}.
"ldh"
#'
#' @title Oranges
#'
#' @description The data arose from five orange trees grown in Riverside,
#' California, during 1969-1973. The response is the trunk circumference,
#' in millimeters, and the predictor variable is time, in days. The
#' predictor variable has an arbitrary origin and was taken on December
#' 31, 1968.
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
#' @description The dataset reports the number of epileptic seizures in
#' each of four two-week intervals, and in a baseline eight-week interval,
#' for Progabide treatment and placebo groups with a total of 59
#' individuals.
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
#' @description The main objective of the analysis of this dataset is to
#' assess if there is an association between the number of faults in
#' fabric rolls and their length.
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
#' @description The market research department of a soft drink
#' manufacturer is investigating the effectiveness of a price discount
#' coupon on the purchase of a two-litre beverage product. A sample of
#' 5500 costumers received coupons for varying price discounts between
#' 5 and 25 cents. The main objective of the analysis is to determine
#' if the price discount affects the proportion of redeemed coupons
#' after one month.
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
#' @description This dataset consists of failure times for hardened steel
#'  specimens in a rolling contact fatigue test. Ten independent
#'  observations were taken at each of the four contact stress values.
#'  Response is the time that each specimen of hardened steel failed.
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
#' @description The advertising data set consists of sales of that product
#' in 200 different markets. It also includes advertising budgets for the
#' product in each of those markets for three different media: TV, radio,
#' and newspapers.
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
#' @description The Alaska pipeline data consists of in-field ultrasonic
#' measurements of defects depths in the Alaska pipeline. The depth of
#' the defects was measured again in the laboratory. These measurements
#' were performed in six batches. The data were analyzed to calibrate
#' the bias of field measurements relative to laboratory measurements.
#' In this analysis, the field measurement is the response variable and
#' the laboratory measurement is the predictor variable.
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
#' @description These data are counts of virus particles at 5 different
#' dilutions. There are 4 replicate counts at each dilution except the
#' last for which there are 5 counts. The aim is to estimate the expected
#' number of virus particles per unit volume.
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
#' @description These data correspond to the (average) body weight and
#' the (average) brain weight for sixty-two species of mammals.
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
#' @description These data arose from a study in dentistry. In this trial,
#' subjects were generally healthy adult male and female volunteers, ages
#' 18–55, with pre-existing plaque but without advanced periodontal
#' disease. Prior to entry, subjects were screened for a minimum of 20
#' sound, natural teeth and a minimum mean plaque index of 2.0. Subjects
#' with gross oral pathology or on antibiotic, antibacterial, or
#' anti-inflammatory therapy were excluded from the study. One hundred
#' nine volunteers were randomized in a double-blinded way to one of two
#' novel mouth rinses (A and B) or to a control mouth rinse. Plaque was
#' scored at baseline, at 3 months, and at 6 months by the Turesky
#' modification of the Quigley-Hein index, a continuous measure. Four
#' subjects had missing plaque scores. The main objective of the
#' analysis is to measure the effectiveness of three mouth rinses at
#' inhibiting dental plaque.
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
#' @description The data arose from a horticultural experiment to study
#' the number of roots produced by 270 micropropagated shoots of the
#' columnar apple cultivar Trajan. During the rooting period, all shoots
#' were maintained under identical conditions. However, the shoots
#' themselves were cultured on media containing different concentrations
#' of the cytokinin 6-benzylaminopurine (BAP), in growth cabinets with
#' an 8 or 16 hour photoperiod. The objective is to assess the effect
#' of both the photoperiod and BAP concentration levels on the number
#' of roots produced.
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
#' @description These data arose from a study conducted in the Department
#' of Internal Medicine at Utrecht University Hospital, in the
#' Netherlands. In this study, 98 HIV-infected men were followed for
#' up to two years. Urinary cultures were obtained during the first
#' visit and every six months thereafter. Also, cultures were obtained
#' between regular scheduled visits when signs and symptoms of urinary
#' tract infections (UTI) occurred, or when patients had a fever of
#' unknown origin. CD4+ cell counts were also measured. A CD4+ count
#' is a blood test to determine how well the immune system works in
#' people diagnosed with HIV. In general, a decreasing CD4+ count
#' indicates HIV progression.
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
#' @description Inflation of the abdomen during laparoscopic
#' cholecystectomy (removal of the gallbladder) separates the liver from
#' the diaphragm and strains the attachments that connect both. This
#' strain is felt as a referred shoulder pain. Suction to remove residual
#' gas may reduce shoulder pain. There were 22 subjects randomized in the
#' active group (with abdominal suction) and 19 subjects randomized in
#' the control group (without abdominal suction). After laparoscopic
#' surgery, patients were asked to rate their shoulder pain on a visual
#' analog scale morning and afternoon for three days after the operation
#' (a total of six different times). The scale was coded into five
#' ordered categories where a pain score of 1 indicated "low pain" and
#' a score of 5 reflected "high pain".
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
#' @description These data arose from a study of the germination of two
#' species of Orobanche seeds (O. aegyptiaca 75 and O. aegyptiaca 73)
#' grown on 1/125 dilutions of two different root extract media (cucumber
#' and bean) in a 2×2 factorial layout with replicates. The data consist
#' of the number of seeds and germinating seeds for each replicate.
#' Interest is focused on the possible differences in germination rates
#' for the two types of seed and root extract and whether there is any
#' interaction.
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
#' 4 patients. The statistical analysis aims to determine what predicts
#' whether or not a patient considers their UI a problem that interferes
#' with him/her daily life.
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
#' @description These data are analyzed primarily to determine how ozone
#' pollution affects tree growth. As ozone pollution is common in urban
#' areas, the impact of increased ozone concentrations on tree growth is
#' of considerable interest. The response variable is tree size, where
#' size is conventionally measured by the product of tree height and
#' stem diameter squared. In the first group, 54 trees were grown under
#' an ozone-enriched atmosphere, ozone exposure at 70 parts per billion.
#' In the second group, 25 trees were grown under normal conditions. The
#' size of each tree was observed 13 times across time, that is, 152,
#' 174, 201, 227, 258, 469, 496, 528, 556, 579, 613, 639 and 674 days
#' since the beginning of the experiment. Hence, the objective is to
#' compare the trees' growth patterns under the two conditions.
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

#' @description In a biomedical study of the immuno-activating ability of
#' two agents, TNF (tumor necrosis factor) and IFN (interferon), to induce
#' cell differentiation, the number of cells that exhibited markers of
#' differentiation after exposure to TNF and IFN was recorded. At each of
#' the 16 TNF/INF dose combinations, 200 cells were examined. The main
#' question is whether the two agents stimulate cell differentiation
#' synergistically or independently.
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
#' Vanegas L.H., Rondon L.M. (2020) A data transformation to deal with
#' constant under/over-dispersion in binomial and poisson regression models. \emph{Journal
#' of Statistical Computation and Simulation} 90:1811-1833.
"cellular"

#' @title Teratogenic effects of phenytoin and trichloropropene oxide

#' @description The data come from a 2x2 factorial design with 81 pregnant
#' mice. In the experiment each pregnant mouse was randomly allocated to a
#' control group and three treated groups. These groups received daily, by
#' gastric gavage, 60 mg/kg of phenytoin, 100 mg/kg of trichloropropene
#' oxide, or 60 mg/kg of phenytoin and 100 mg/kg of trichloropropene oxide. On day 18 of gestation, the fetuses were recovered, stained, and cleared. Then, by visual inspection, the presence or absence of ossification was determined for the different joints of the right and left forepaws. The experiment investigated the synergy of phenytoin and trichloropropene oxide to produce ossification at the phalanges, teratogenic effects.
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
#' @description A pilot surf/health study was conducted by NSW Water
#' Board in 1990 on 287 recruits. The objective of the study was to
#' determine whether beach swimmers run an increased risk of contracting
#' ear infections than non-beach swimmers.
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
#' @description A total of 76 female rats were injected with a carcinogen
#' for mammary cancer. Then, all animals were given retinyl acetate
#' (retinoid) to prevent mammary cancer for 60 days. After this phase,
#' the 48 animals that remained tumor-free were randomly assigned to
#' continue retinoid prophylaxis or control. Rats were then palpated for
#' tumors twice weekly, and observations ended 182 days after initial
#' carcinogen injections began. The main objective of the analysis was
#' to assess the difference in tumor development between the treated
#' and control groups.
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
#' @description In these data the response is species richness represented
#' by a count of the number of plant species on plots with different
#' biomass and three different soil pH levels: low, mid, and high.
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
#' @description Each year the Scottish Hill Runners Association publishes
#' a list of hill races in Scotland for the year. These data consist of
#' the record time, distance, and cumulative climb of 35 of those races.
#' The statistical analysis of these data aims to explain the differences
#' between the record time of the races. This is done using their
#' differences in distance and cumulative climb.
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
#' @description Female mice were continuously fed dietary concentrations
#' of 2-Acetylaminofluorene (2-AAF), a carcinogenic and mutagenic
#' derivative of fluorene. Serially sacrificed, dead or moribund mice
#' were examined for tumors and deaths dates recorded. These data consist
#' of the incidences of bladder neoplasms in mice observed during 33
#' months.
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
#' @description Female mice were continuously fed dietary concentrations
#' of 2-Acetylaminofluorene (2-AAF), a carcinogenic and mutagenic
#' derivative of fluorene. Serially sacrificed, dead or moribund mice
#' were examined for tumors and deaths dates recorded. These data consist
#' of the incidences of liver neoplasms in mice observed during 18 months.
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
#' @description The data describe the incidence of nonmelanoma skin
#' cancer among women stratified by age in Minneapolis (St. Paul) and
#'  Dallas (Fort Worth).
#'
#' @docType data
#'
#' @usage data(skincancer)
#'
#' @format A data frame with 16 rows and 4 variables:
#' \describe{
#'   \item{cases}{a numeric vector giving the nonmelanoma skin cancer counts.}
#'   \item{city}{a factor giving the city to which correspond the skin cancer counts: "St.Paul" and "Ft.Worth".}
#'   \item{ageC}{a factor giving the age range to which correspond the skin cancer counts: "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84" and "85+".}
#'   \item{population}{a numeric vector giving the population of women.}
#'   \item{age}{a numeric vector giving the midpoint of age range.}
#' }
#' @keywords datasets
#' @examples
#' data(skincancer)
#' dev.new()
#' barplot(1000*cases/population ~ city + ageC, beside=TRUE, col=c("yellow","blue"),
#'         data=skincancer)
#' legend("topleft", legend=c("St.Paul","Ft.Worth"), title="City",
#'        fill=c("yellow","blue"), bty="n")
#'
#' @references Kleinbaum D., Kupper L., Nizam A., Rosenberg E.S. (2013) \emph{Applied Regression Analysis and
#' other Multivariable Methods, Fifth Edition}, Cengage Learning, Boston.
"skincancer"

#' @title Lesions of Aucuba mosaic virus
#'
#' @description The investigators counted the number of lesions of
#' \emph{Aucuba mosaic} virus developing after exposure to X rays for
#'  various times.
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
#' @description These data arose from a study on the efficacy of oestrogen
#' given transdermally for the treatment of severe postnatal depression.
#' Women with major depression were randomly assigned to a placebo control
#' group or an oestrogen patch group. Prior to the treatment all women
#' were assessed by self-rated depressive symptoms on the Edinburgh
#' Postnatal Depression Scale (EPDS). EPDS data were collected monthly
#' for six months once treatment began. Higher EDPS scores are indicative
#' of higher depression levels.
#'
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
#' @references Gregoire A.J.P., Kumar R., Everitt B., Henderson A.F., Studd J.W.W. (1996) Transdermal oestrogen for treatment of severe postnatal depression,
#' \emph{The Lancet} 347:930-933.
"depression"

#' @title Shelf life of a photographic developer
#'
#' @description These data arise from an experiment using accelerated
#' life testing to determine the estimated shelf life of a photographic
#' developer. Maximum density and temperature seem to be reliable
#' indicators of overall developer/film performance.
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
#' @references Chapman R.E. (1997) Degradation study of a photographic developer to determine shelf life,
#' \emph{Quality Engineering} 10:1, 137-140.
"shelflife"
#'
#' @title The effects of fertilizers on coastal Bermuda grass
#'
#' @description These data arose from a \eqn{4^3} factorial experiment with the three major plant nutrients, nitrogen (N), phosphorus (P), and potassium (K), on the
#' yield of coastal Bermuda grass. The experiment was performed to produce a response surface for the effects of the three nutrients, so that an optimal
#' dressing could be predicted. The grass was cut about every five weeks and oven-dried.
#'
#' @docType data
#'
#' @usage data(Grass)
#'
#' @format A data frame with 64 rows and 4 variables:
#' \describe{
#'   \item{Nitrogen}{a numeric vector indicating the Nitrogen level, in lb/acre.}
#'   \item{Phosphorus}{a numeric vector indicating the Phosphorus level, in lb/acre.}
#'   \item{Potassium}{a numeric vector indicating the Potassium level, in lb/acre.}
#'   \item{Yield}{a numeric vector indicating the yields, in tons/acre.}
#' }
#' @keywords datasets
#' @references Welch L.F., Adams W.E., Carmon J.L. (1963) Yield Response Surfaces, Isoquants, and Economic Fertilizer Optima for Coastal Bermuda grass.
#'             \emph{Agronomy Journal}, 55, 63-67.
#' @references McCullagh P., Nelder J.A. (1989). \emph{Generalized Linear Models. 2nd Edition}. Chapman and Hall, London.
#' @references Wei B.C. (1998). \emph{Exponential Family Nonlinear Models}. Springer, Singapore.
"Grass"
#'
#'
#' @title Assay of an Insecticide with a Synergist
#'
#' @description These data are concerned with the estimation of the lowest-cost mixtures of insecticides and synergists. They
#' relate to assays on a grasshopper \emph{Melanopus sanguinipes} with carbofuran and piperonyl butoxide, which enhances
#' carbofuran's toxicity.
#'
#' @docType data
#'
#' @usage data(Melanopus)
#'
#' @format A data frame with 15 rows and 4 variables:
#' \describe{
#'   \item{Killed}{a numeric vector indicating how many grasshoppers were killed.}
#'   \item{Exposed}{a numeric vector indicating how many grasshoppers were exposed.}
#'   \item{Insecticide}{a numeric vector indicating the dose of insecticide.}
#'   \item{Synergist}{a numeric vector indicating the dose of synergist.}
#' }
#' @keywords datasets
#' @references McCullagh P., Nelder J.A. (1989). \emph{Generalized Linear Models. 2nd Edition}. Chapman and Hall, London.
#' @references Wei B.C. (1998). \emph{Exponential Family Nonlinear Models}. Springer, Singapore.
"Melanopus"
#'
#'
#' @title Developmental rate of Drosophila melanogaster
#'
#' @description \emph{Drosophila melanogaster} developmental stages were monitored as part of an experiment to determine the effect of temperature
#' on their duration. The eggs were laid at approximately 25 degrees Celsius and remained at that temperature for 20-30 minutes. The eggs were then
#' brought to the experimental temperature, which remained constant throughout the experiment.
#'
#' @docType data
#'
#' @usage data(Drosophila)
#'
#' @format A data frame with 23 rows and 3 variables:
#' \describe{
#'   \item{Temp}{a numeric vector indicating the temperature, in degrees Celsius.}
#'   \item{Duration}{a numeric vector indicating the average duration of the embryonic period, in hours, measured from the time at which the eggs were laid.}
#'   \item{Size}{a numeric vector indicating how many eggs each batch contained.}
#' }
#' @keywords datasets
#' @references Powsner L. (1935) The effects of temperature on the durations of the developmental stages of \emph{Drosophila melanogaster}.
#'             \emph{Physiological Zoology}, 8, 474-520.
#' @references McCullagh P., Nelder J.A. (1989). \emph{Generalized Linear Models. 2nd Edition}. Chapman and Hall, London.
#' @references Wei B.C. (1998). \emph{Exponential Family Nonlinear Models}. Springer, Singapore.
#' @examples
#' data(Drosophila)
#' dev.new()
#' with(Drosophila, plot(Temp,Duration,xlab="Temperature, in degrees Celsius",pch=16,col="blue",
#'                       ylab="Average duration of the embryonic period"))
"Drosophila"
#'
#'
#' @title Radioimmunological Assay of Cortisol
#'
#' @description The amount of hormone contained in a preparation
#' cannot be measured directly, so estimating an unknown dose of
#' hormone involves a two-step process. A calibration curve must
#' first be established, then the curve must be inverted to
#' determine the hormone dose. The calibration curve is estimated
#' using a radioimmunological assay.
#'
#' @docType data
#'
#' @usage data(Cortisol)
#'
#' @format A data frame with 64 rows and 2 variables:
#' \describe{
#'   \item{lDose}{a numeric vector indicating the logarithm in base 10 of the dose.}
#'   \item{Y}{a numeric vector indicating the response, in counts per minute.}
#' }
#' @keywords datasets
#' @references Huet S., Bouvier A., Poursat M.-A., Jolivet E. (2004). \emph{Statistical tools for nonlinear regression : a practical guide with S-PLUS and R
#' examples. 2nd Edition}. Springer,  New York.
#' @examples
#' data(Cortisol)
#' dev.new()
#' with(Cortisol, plot(lDose,Y,xlab="Log10(Dose, in ng/0.1 ml)",pch=16,col="blue",
#'                     ylab="Response, in counts per minute"))
"Cortisol"
#'

#'
#' @title Generalized Nonlinear Models.
#' @description \code{gnm} is used to fit generalized nonlinear models, specified by giving a symbolic description of the "linear" predictor
#'              and a description of the error distribution.
#' @param formula a \code{formula} expression which is a symbolic description of the "linear" predictor of the model to be fitted to the data.
#' @param family a description of the error distribution and link function to be used in the model. For \code{gnm} this can be a character string naming a family function,
#'        a family function or the result of a call to a family function. As default, \code{family} is set to \code{gaussian(identity)}.
#' @param weights an (optional) vector of "prior weights" to be used in the fitting process. The length of \code{weights} should be the same as the number of observations.
#' @param offset this can be used to specify an \emph{a priori} known component to be included in the linear predictor during fitting. This should be \code{NULL} or a numeric vector of length equal to the number of cases.
#' @param data an (optional) data frame, list or environment (or object coercible by \link{as.data.frame} to a data frame) containing the variables in the model.
#'        If not found in data, the variables are taken from \code{environment(formula)}, typically the environment from which \code{gnm} is called.
#' @param subset an (optional) vector specifying a subset of observations to be used in the fitting process.
#' @param start an (optional) vector of starting values for the parameters in the "linear" predictor.
#' @param toler an (optional) positive value which represents the \emph{convergence tolerance}. The convergence is reached when the maximum of the absolute relative
#'        differences between the values of the parameters in the "linear" predictor in consecutive iterations of the fitting algorithm is lower than \code{toler}. As
#'        default, \code{toler} is set to 0.00001.
#' @param maxit an (optional) integer value which represents the maximum number of iterations allowed for the fitting algorithm. As default, \code{maxit} is set to 50.
#' @param trace an (optional) logical variable. If \code{TRUE}, output is produced for each iteration of the estimating algorithm.
#' @param ... further arguments passed to or from other methods.
#'
#' @return an object of class \emph{gnm} in which the main results of the model fitted to the data are stored, i.e., a
#' list with components including
#' \tabular{ll}{
#' \code{coefficients} \tab a vector containing the parameter estimates,\cr
#' \tab \cr
#' \code{fitted.values}\tab a vector containing the estimates of \eqn{\mu_1,\ldots,\mu_n},\cr
#' \tab \cr
#' \code{start}        \tab a vector containing the starting values used,\cr
#' \tab \cr
#' \code{prior.weights}\tab a vector containing the case weights used,\cr
#' \tab \cr
#' \code{offset}       \tab a vector containing the offset used, \cr
#' \tab \cr
#' \code{terms}        \tab an object containing the terms objects,\cr
#' \tab \cr
#' \code{loglik}       \tab the value of the log-likelihood function avaliated at the parameter estimates,\cr
#' \tab \cr
#' \code{estfun}       \tab a vector containing the estimating functions evaluated at the parameter estimates\cr
#'                     \tab and the observed data,\cr
#' \tab \cr
#' \code{formula}      \tab the formula,\cr
#' \tab \cr
#' \code{converged}    \tab a logical indicating successful convergence,\cr
#' \tab \cr
#' \code{model}        \tab the full model frame,\cr
#' \tab \cr
#' \code{y}            \tab the response vector,\cr
#' \tab \cr
#' \code{family}       \tab an object containing the \link{family} object used,\cr
#' \tab \cr
#' \code{linear.predictors} \tab a vector containing the estimates of \eqn{g(\mu_1),\ldots,g(\mu_n)},\cr
#' \tab \cr
#' \code{R}            \tab a matrix with unscaled estimate of the variance-covariance\cr
#'                     \tab matrix of model parameters,\cr
#' \tab \cr
#' \code{call}         \tab the original function call.\cr
#' }
#' @seealso \link{glm}, \link{glmgee}, \link{gnmgee}
#' @details
#' A set of standard extractor functions for fitted model objects is available for objects of class \emph{gnm},
#' including methods to the generic functions such as \code{summary}, \code{model.matrix}, \code{estequa},
#' \code{coef}, \code{vcov}, \code{logLik}, \code{fitted}, \code{confint}, \code{AIC}, \code{BIC} and \code{predict}.
#' In addition, the model fitted to the	data may be assessed using functions such as \code{adjR2.gnm}, \link{anova.gnm},
#' \link{residuals.gnm}, \link{dfbeta.gnm}, \link{cooks.distance.gnm} and \link{envelope.gnm}.
#' @export gnm
#' @examples
#' ###### Example 1: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#' summary(fit1)
#'
#' ###### Example 2: Assay of an Insecticide with a Synergist
#' data(Melanopus)
#' fit2 <- gnm(Killed/Exposed ~ b0 + b1*log(Insecticide-a1) + b2*Synergist/(a2 + Synergist),
#'             family=binomial(logit), weights=Exposed, start=c(b0=-3,b1=1.2,a1=1.7,b2=1.7,a2=2),
#'			   data=Melanopus)
#' summary(fit2)
#'
#' ###### Example 3: Developmental rate of Drosophila melanogaster
#' data(Drosophila)
#' fit3 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
#'             start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)
#' summary(fit3)
#'
#' ###### Example 4: Radioimmunological Assay of Cortisol
#' data(Cortisol)
#' fit4 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
#'             start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)
#' summary(fit4)
#'
#' ###### Example 5: Age and Eye Lens Weight of Rabbits in Australia
#' data(rabbits)
#' fit5 <- gnm(wlens ~ b1 - b2/(age + b3), family=Gamma(log),
#'             start=c(b1=5.5,b2=130,b3=35), data=rabbits)
#' summary(fit5)
#'
#' ###### Example 6: Calls to a technical support help line
#' data(calls)
#' fit6 <- gnm(calls ~ SSlogis(week, Asym, xmid, scal), family=poisson(identity), data=calls)
#' summary(fit6)
#'
gnm <- function(formula,family=gaussian(),offset=NULL,weights=NULL,data,subset=NULL,start=NULL,toler=0.00001,maxit=50,trace=FALSE,...){
  if(missingArg(data)) data <- environment(eval(formula))
  if(is(family,"function")) family <- family()
  .Theta <- function() return(.Theta)
  environment(.Theta) <- environment(family$variance)
  if(is.null(start)){
    defaultW <- getOption("warn")
    options(warn = -1)
    form <- paste0("y2 <- family$linkfun(",formula[[2]],")")
    if(family$family=="binomial"){
      form <- paste0("ifelse(",formula[[2]],"*(1-",formula[[2]],")==0,abs(",formula[[2]],"-0.01),",formula[[2]],")")
      form <- paste0("y2 <- family$linkfun(",form,")")
    }
    if(family$family=="poisson") form <- paste0("y2 <- family$linkfun(ifelse(",formula[[2]],"==0,0.01,",formula[[2]],"))")
    eval(parse(text=paste0("data <- within(data,",form,")")))
    mmf <- match.call(expand.dots = FALSE)
    m <- match(c("subset", "weights", "offset"), names(mmf), 0)
    mmf <- mmf[c(1L,m)]
    if(!is.null(mmf$offset)) eval(parse(text=paste0("data <- within(data,y2 <- y2 - ",deparse(mmf$offset),")") ))
    conls <- try(do.call(nls,list(formula=as.formula(paste0("y2 ~ ",as.character(formula[3]))),data=data,subset=mmf$subset,weights=mmf$weights)),silent=TRUE)
    options(warn = defaultW)
    if(is.list(conls)) start <- coef(conls)
  }
  if(is.null(start)) stop("Starting values are required. Please specify some!!",call.=FALSE)
  formula2 <- formula
  avs <- all.vars(formula)[-1]
  if(is.null(names(start))) pars <- rownames(start) else pars <- names(start)
  vnames <- avs[is.na(match(avs,pars))]
  formula <- paste(formula[2],"~ 1 + ",paste(vnames,collapse="+"))
  formula <- as.formula(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "weights", "offset", "data", "subset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.omit
  mf[[1L]] <- as.name("model.frame")
  mf$formula <- formula
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- as.matrix(model.response(mf, "any"))
  offset <- as.vector(model.offset(mf))
  weights <- as.vector(model.weights(mf))
  if(ncol(y)==2 & family$family=="binomial"){
    weights <- as.matrix(y[,1] + y[,2])
    y <- as.matrix(y[,1]/weights)
  }
  y <- as.matrix(as.numeric(y))
  X <- model.matrix(mt, mf)
  p <- length(start)
  n <- nrow(X)
  if(is.null(weights)) weights <- matrix(1,n,1) else weights <- as.matrix(weights)
  if(any(weights <= 0)) stop("Only positive weights are allowed!!",call.=FALSE)
  if(is.null(offset)) offs <- matrix(0,n,1) else offs <- as.matrix(offset)
  if(family$family=="poisson" & (any(abs(y - round(y)) > 0.001) | any(y<0)))
    stop("negative or non-integer values are not allowed for the 'Poisson' family",call.=FALSE)
  if(grepl("Negative Binomial",family$family) & (any(abs(y - round(y)) > 0.001) | any(y<0)))
    stop("negative or non-integer values are not allowed for the 'Negative Binomial' family",call.=FALSE)
  if(family$family=="binomial" & any(abs(weights - round(weights)) > 0.001))
    stop("non-integer prior.weights are not allowed for the 'binomial' family",call.=FALSE)
  if(family$family=="binomial" & (any(abs(y*weights - round(y*weights)) > 0.001) | any(y<0)))
    stop("negative or non-integer values are not allowed for the 'binomial' family",call.=FALSE)
  if(family$family %in% c("Gamma","inverse.gaussian") & any(y<=0))
    stop(paste("non-positive values are not allowed for the",family$family,"family"),call.=FALSE)
  etaf <- function(beta){
    temp <- deparse(formula2[[3]])
    for(i in 1:p) temp <- gsub(pars[i],beta[i],temp)
    matrix(eval(parse(text=temp),envir=data.frame(X)),ncol=1)
  }
  Der <- function(beta) matrix(jacobian(etaf,beta),ncol=p)
  beta_new <- start
  tol <- 1
  niter <- 0
  if(trace) message("\nIteration  Convergence Criterion")
  while(tol > toler & niter < maxit){
    beta_old <- beta_new
    etas <- etaf(beta_old) + offs
    Ders <- Der(beta_old)
    mus <- family$linkinv(etas)
    w <- family$mu.eta(etas)*sqrt(weights/family$variance(mus))
    Dersw <- Ders*matrix(w,n,p)
    U <- crossprod(Dersw,sqrt(weights/family$variance(mus))*(y-mus))
    I <- crossprod(Dersw,Dersw)
    kchol <- try(chol(I),silent=TRUE)
    if(is.matrix(kchol)) kchol <- chol2inv(kchol) else kchol <- solve(I)
    beta_new <- beta_old + kchol%*%U
    tol <- max(abs((beta_new-beta_old)/beta_old))
    niter <- niter + 1
    if(trace) message("    ",niter,"            ",signif(tol,digits=5))
  }
  if(niter >= maxit) warning("algorithm did not converge", call. = FALSE)
  rownames(beta_new) <- pars
  colnames(beta_new) <- ""
  etas <- etaf(beta_new) + offs
  mus <- family$linkinv(etas)
  Ders <- Der(beta_new)
  if(is.null(family$dispersion)){
    if(family$family %in% c("poisson","binomial")) family$dispersion <- 1 else family$dispersion <- NA
    if(grepl("Negative Binomial",family$family)) family$dispersion <- 1
  }
  phi <- ifelse(is.na(family$dispersion),sum((y-mus)^2*weights/family$variance(mus))/(n-p),family$dispersion)
  w <- family$mu.eta(etas)*sqrt(weights/family$variance(mus))
  Dersw <- Ders*matrix(w,n,p)
  I <- crossprod(Dersw,Dersw)
  kchol <- try(chol(I),silent=TRUE)
  if(is.matrix(kchol)) vc <- chol2inv(kchol) else vc <- solve(kchol)
  estfun <- matrix(crossprod(Dersw,sqrt(weights/family$variance(mus))*(y-mus))/phi,length(beta_new),1)
  colnames(vc) <- rownames(vc) <- rownames(estfun) <- pars
  colnames(estfun) <- ""
  if(var(offs)==0) null.deviance <- sum(family$dev.resids(y,sum(y*weights)/sum(weights),weights))
  else{
    fit0 <- glm(y ~ 1, offset=offs, family=family, weights=weights)
    null.deviance <- fit0$deviance
  }
  out_ <- list(coefficients=beta_new,fitted.values=mus,linear.predictors=etas,null.deviance=null.deviance,df.null=n-1,
               prior.weights=weights,y=y,formula=formula2,call=match.call(),offset=offs,model=mf,data=data,
               converged=ifelse(niter<maxit,TRUE,FALSE),estfun=estfun,R=vc,terms=mt,family=family,weights=w**2,
               phi=phi,logLik=-family$aic(y,rep(1,n),mus,weights,sum(family$dev.resids(y,mus,weights)))/2 + is.na(family$dispersion),
               deviance=sum(family$dev.resids(y,mus,weights)),df.residual=n-p,levels=.getXlevels(attr(mf,"terms"),mf),
               contrasts=attr(X,"contrasts"),start=start,iter=niter,etaf=etaf,linear=FALSE)
  class(out_) <- "gnm"
  return(out_)
}

#' @method vcov gnm
#' @export
vcov.gnm <- function(object, ...,digits=max(3, getOption("digits") - 2), dispersion=NULL){
  disp <- ifelse(is.null(dispersion),object$phi,dispersion)
  return(object$R*disp)
}
#' @method coef gnm
#' @export
coef.gnm <- function(object, ...,digits=max(3, getOption("digits") - 2)){
  out_ <- round(object$coefficients,digits=digits)
  colnames(out_) <- "Estimates"
  return(out_)
}
#' @method model.matrix gnm
#' @export

model.matrix.gnm <-	function(object,...){
  X <- model.matrix(object$terms, object$model, contrasts=object$contrasts)
  p <- length(object$coefficients)
  pars <- rownames(object$coefficients)
  etaf <- function(beta){
    temp <- deparse(object$formula[[3]])
    for(i in 1:p) temp <- gsub(pars[i],beta[i],temp)
    matrix(eval(parse(text=temp),data.frame(X)),ncol=1)
  }
  Der <- function(beta) matrix(jacobian(etaf,beta),ncol=p)
  out_ <- Der(object$coefficients)
  colnames(out_) <- pars
  return(out_)
}
#' @method logLik gnm
#' @export
logLik.gnm <- function(object, ...){
  out_ <- object$logLik
  attr(out_,"df") <- length(object$coefficients) + ifelse(is.null(object$family$dispersion),1,ifelse(is.na(object$family$dispersion),1,0))
  attr(out_,"nobs") <- length(object$prior.weights)
  class(out_) <- "logLik"
  return(out_)
}

#' @method fitted gnm
#' @export
fitted.gnm <- function(object, ...){
  out_ <- object$fitted.values
  colnames(out_) <- "Fitted values"
  return(out_)
}

#' @method estequa gnm
#' @export
estequa.gnm <- function(object, ...){
  out_ <- object$estfun
  colnames(out_) <- " "
  return(out_)
}
#' @title Confidence Intervals for Generalized Nonlinear Models
#' @description Computes confidence intervals based on Wald test for a generalized nonlinear model.
#' @param object an object of the class \emph{gnm}.
#' @param parm a specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param digits an (optional) integer value indicating the number of decimal places to be used. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @param level an (optional) value indicating the required confidence level. As default, \code{level} is set to 0.95.
#' @param contrast an (optional) matrix indicating the linear combinations of parameters for which confidence intervals are required. The number of rows in this matrix corresponds to the number of linear combinations required.
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param dispersion an (optional) value indicating the estimate of the dispersion parameter. As default, \code{dispersion} is set to \code{summary(object)$dispersion}.
#' @param ...	further arguments passed to or from other methods.
#' @details The approximate 100(\code{level})\% confidence interval for \eqn{\beta} based on the Wald test.
#' @return A matrix with so many rows as parameters in the "linear" predictor and two columns: "Lower limit" and "Upper limit".
#' @examples
#' ###### Example 1: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#'
#' confint(fit1, level=0.95)
#'
#' ###### Example 2: Assay of an Insecticide with a Synergist
#' data(Melanopus)
#' fit2 <- gnm(Killed/Exposed ~ b0 + b1*log(Insecticide-a1) + b2*Synergist/(a2 + Synergist),
#'             family=binomial(logit), weights=Exposed, start=c(b0=-3,b1=1.2,a1=1.7,b2=1.7,a2=2),
#'			   data=Melanopus)
#'
#' confint(fit2, level=0.95)
#'
#' ###### Example 3: Developmental rate of Drosophila melanogaster
#' data(Drosophila)
#' fit3 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
#'             start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)
#'
#' confint(fit3, level=0.95)
#'
#' ###### Example 4: Radioimmunological Assay of Cortisol
#' data(Cortisol)
#' fit4 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
#'             start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)
#'
#' ### Confidence Interval for 'b1-b0'
#' confint(fit4, level=0.95, contrast=matrix(c(-1,1,0,0,0),1,5))
#'
#' ### Confidence Intervals for 'b0', 'b1', 'b2', 'b3', 'b4'
#' confint(fit4, level=0.95, contrast=diag(5))
#'
#' @method confint gnm
#' @export
confint.gnm <- function(object,parm,level=0.95,contrast,digits=max(3, getOption("digits") - 2),dispersion=NULL,verbose=TRUE,...){
  name.s <- rownames(object$coefficients)
  if(missingArg(contrast)){
    bs <- coef(object)
    ee <- sqrt(diag(vcov(object,dispersion=dispersion)))
  }else{
    contrast <- as.matrix(contrast)
    if(ncol(contrast)!=length(name.s)) stop(paste("Number of columns of contrast matrix must to be",length(name.s)),call.=FALSE)
    bs <- contrast%*%coef(object)
    ee <- sqrt(diag(contrast%*%vcov(object,dispersion=dispersion)%*%t(contrast)))
    name.s <- apply(contrast,1,function(x) paste0(x[x!=0],"*",name.s[x!=0],collapse=" + "))
  }
  results <- matrix(0,length(ee),2)
  results[,1] <- bs - qnorm((1+level)/2)*ee
  results[,2] <- bs + qnorm((1+level)/2)*ee
  rownames(results) <- name.s
  colnames(results) <- c("Lower limit","Upper limit")
  if(verbose){
    cat("\n Approximate",round(100*level,digits=1),"percent confidence intervals based on the Wald test \n\n")
    print(round(results,digits=digits))
  }
  return(invisible((round(results,digits=digits))))
}

#' @method predict gnm
#' @export
predict.gnm <- function(object, ...,newdata, se.fit=FALSE, type=c("link","response"), dispersion=NULL){
  type <- match.arg(type)
  if(missingArg(newdata)) predicts <- object$linear.predictors
  else{
    newdata <- data.frame(newdata)
    mf <- model.frame(delete.response(object$terms),newdata,xlev=object$levels)
    X <- model.matrix(delete.response(object$terms),mf,contrasts=object$contrasts)
    p <- length(object$coefficients)
    pars <- rownames(object$coefficients)
    etaf <- function(beta){
      temp <- deparse(object$formula[[3]])
      for(i in 1:p) temp <- gsub(pars[i],beta[i],temp)
      matrix(eval(parse(text=temp),data.frame(X)),ncol=1)
    }
    predicts <- etaf(object$coefficients)
    offs <- model.offset(mf)
    if(!is.null(offs)) predicts <- predicts + offs
  }
  if(type=="response") predicts <- object$family$linkinv(predicts)
  if(se.fit){
    X <- matrix(jacobian(etaf,object$coefficients),nrow=nrow(X))
    varhat <- vcov(object,dispersion=dispersion)
    se <- matrix(sqrt(apply(tcrossprod(X,varhat)*X,1,sum)),ncol=1)
    if(type=="response") se <- se*abs(object$family$mu.eta(object$family$linkfun(predicts)))
    predicts <- cbind(predicts,se)
    colnames(predicts) <- c("fit","se.fit")
  }else colnames(predicts) <- c("fit")
  return(predicts)
}

#' @method summary gnm
#' @export
summary.gnm <- function(object, ...,digits=max(3, getOption("digits") - 2),dispersion=NULL){
  disp <- ifelse(is.null(dispersion),object$phi,dispersion)
  aic <- round(object$family$aic(object$y,rep(1,length(object$y)),object$fitted.values,object$prior.weights,object$deviance) + 2*length(object$coefficients),digits=digits)
  cat("\nSample size")
  cat("\n   Number of observations: ",length(object$y))
  cat("\n*************************************************************")
  cat("\nModel")
  cat("\n      Family distribution: ",object$family$family)
  cat("\n            Link function: ",object$family$link)
  cat("\n*************************************************************\n")
  cat("Coefficients\n")
  TAB	<- rbind(cbind(Estimate <- object$coefficients,
                     StdErr <- sqrt(diag(vcov(object,dispersion=disp))),
                     tval <- Estimate/StdErr,
                     p.value <- 2*pnorm(-abs(tval))))
  colnames(TAB) <- c("Estimate", "Std.Error", "z-value", "Pr(>|z|)")
  rownames(TAB) <- rownames(object$coefficients)
  printCoefmat(TAB, P.values=TRUE, signif.stars=FALSE, has.Pvalue=TRUE, digits=digits, dig.tst=digits, signif.legend=FALSE, tst.ind=c(1,2,3), na.print="")
  cat("\n(Dispersion parameter for",object$family$family,"family taken to be",paste0(round(disp,digits=digits),")"),"\n")
  cat("*************************************************************\n")
  nd <-	round(object$null.deviance,digits=digits)
  rd <- round(object$deviance,digits=digits)
  r2 <- round(1 - rd/nd,digits=digits)
  adjr2 <- round(1 - (object$deviance/object$df.residual)/(object$null.deviance/object$df.null),digits=digits)
  if(is.na(aic)){
    m <- max(nchar(nd),nchar(rd),nchar(r2),nchar(adjr2))
    aic <- NA
  }
  else{m <- max(nchar(nd),nchar(rd),nchar(r2),nchar(adjr2),nchar(aic))
  if(nchar(aic) < m) aic <- paste(paste(rep(" ",m-nchar(aic)-1),collapse=""),aic)
  }
  if(nchar(rd) < m) rd <- paste(paste(rep(" ",m-nchar(rd)-1),collapse=""),rd)
  if(nchar(nd) < m) nd <- paste(paste(rep(" ",m-nchar(nd)-1),collapse=""),nd)
  if(nchar(r2) < m) r2 <- paste(paste(rep(" ",m-nchar(r2)-1),collapse=""),r2)
  if(nchar(adjr2) < m) adjr2 <- paste(paste(rep(" ",m-nchar(adjr2)-1),collapse=""),adjr2)
  cat("     Null deviance:",as.character(nd)," on ",object$df.null,"degrees of freedom\n")
  cat(" Residual deviance:",as.character(rd)," on ",object$df.residual,"degrees of freedom\n")
  cat("         R-squared:",as.character(r2),"\n")
  cat("Adjusted R-squared:",as.character(adjr2),"\n")
  cat("               AIC:",as.character(aic),"\n\n")
  cat("Number of Fisher Scoring iterations:",object$iter,"\n")
  return(invisible(list(coefficients=round(TAB,digits=digits),r.squared=r2,adj.r.squared=adjr2,aic=aic,dispersion=disp)))
}
#'
#' @title Residuals for Generalized Nonlinear Models
#' @description Computes residuals for a fitted generalized nonlinear model.
#' @param object a object of the class \emph{gnm}.
#' @param type an (optional) character string giving the type of residuals which should be returned. The available options are: (1) "quantile", (2) "deviance", and (3) "pearson". As default, \code{type} is set to "quantile".
#' @param standardized an (optional) logical switch indicating if the residuals should be standardized by dividing by the square root of \eqn{(1-h)}, where \eqn{h} is a measure of leverage. As default, \code{standardized} is set to FALSE.
#' @param plot.it an (optional) logical switch indicating if a plot of the residuals versus the fitted values is required. As default, \code{plot.it} is set to FALSE.
#' @param identify an (optional) integer value indicating the number of individuals to identify on the plot of residuals. This is only appropriate when \code{plot.it=TRUE}.
#' @param dispersion an (optional) value indicating the dispersion parameter estimate that must be used to calculate residuals.
#' @param ... further arguments passed to or from other methods
#' @return A vector with the observed residuals type \code{type}.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Davison A.C., Gigli A. (1989) Deviance Residuals and Normal Scores Plots. \emph{Biometrika} 76, 211-221.
#' @references Dunn P.K., Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics} 5, 236-244.
#' @references Pierce D.A., Schafer D.W. (1986) Residuals in Generalized Linear Models. \emph{Journal of the American Statistical Association} 81, 977-986.
#' @examples
#' ###### Example 1: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#' residuals(fit1, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'                 col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Assay of an Insecticide with a Synergist
#' data(Melanopus)
#' fit2 <- gnm(Killed/Exposed ~ b0 + b1*log(Insecticide-a1) + b2*Synergist/(a2 + Synergist),
#'             family=binomial(logit), weights=Exposed, start=c(b0=-3,b1=1.2,a1=1.7,b2=1.7,a2=2),
#'			   data=Melanopus)
#' residuals(fit2, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'                 col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Developmental rate of Drosophila melanogaster
#' data(Drosophila)
#' fit3 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
#'             start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)
#' residuals(fit3, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'                 col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 4: Radioimmunological Assay of Cortisol
#' data(Cortisol)
#' fit4 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
#'             start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)
#' residuals(fit4, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'                 col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 5: Age and Eye Lens Weight of Rabbits in Australia
#' data(rabbits)
#' fit5 <- gnm(wlens ~ b1 - b2/(age + b3), family=Gamma(log),
#'             start=c(b1=5.5,b2=130,b3=35), data=rabbits)
#' residuals(fit5, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'                 col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 6: Calls to a technical support help line
#' data(calls)
#' fit6 <- gnm(calls ~ SSlogis(week, Asym, xmid, scal), family=poisson(identity), data=calls)
#' residuals(fit6, type="quantile", plot.it=TRUE, col="red", pch=20, col.lab="blue",
#'                 col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' @method residuals gnm
#' @export
residuals.gnm <- function(object,type=c("quantile","deviance","pearson"),standardized=FALSE,plot.it=FALSE,identify,dispersion=NULL,...){
  type <- match.arg(type)
  .Theta <- function() return(.Theta)
  environment(.Theta) <- environment(object$family$variance)
  family <- object$family
  if(grepl("Negative Binomial",family$family)) family$family <- "Negative.Binomial"
  if(type=="quantile" & !(family$family %in% c("Gamma","inverse.gaussian","gaussian","poisson","binomial","Negative.Binomial")))
    stop("Quantile-type residuals are not supported for this family of distributions !!",call.=FALSE)
  if(any(object$prior.weights == 0)) stop("Only positive weights are supported!!",call.=FALSE)
  quantileres <- function(family,y,mu,phi){
    resi <- switch(family,
                   Gamma = pgamma(y,shape=1/phi,scale=mu*phi),
                   inverse.gaussian = pnorm((y/mu-1)/sqrt(phi*y)) + exp(2/(mu*phi))*pnorm(-(y/mu+1)/sqrt(y*phi)),
                   gaussian = pnorm((y-mu)/sqrt(phi)),
                   poisson = ppois(y-1,lambda=mu) + dpois(y,lambda=mu)*runif(length(mu)),
                   binomial = pbinom(y/phi-1,size=1/phi,prob=mu) + dbinom(y/phi,size=1/phi,prob=mu)*runif(length(mu)),
                   Negative.Binomial = pnbinom(y-1,size=.Theta(),mu=mu) + dnbinom(y-1,size=.Theta(),mu=mu)*runif(length(mu)))
    resi2 <- ifelse(resi>=0.5,1-resi,resi)
    return(qnorm(ifelse(.Machine$double.xmin>=resi2,.Machine$double.xmin,resi2))*ifelse(resi>0.5,-1,1))
  }
  phi <- ifelse(is.null(dispersion),object$phi,dispersion)
  weights <- ifelse(!(family$family %in% c("poisson","Negative.Binomial")),object$prior.weights,1)
  if(type=="quantile")
    rd <- quantileres(family$family,object$y,object$fitted.values,phi/weights)
  if(type=="pearson")
    rd <- (object$y - object$fitted.values)/sqrt(phi*object$family$variance(object$fitted.values)/weights)
  if(type=="deviance")
    rd <- sqrt(family$dev.resids(object$y,object$fitted.values,weights)/phi)*ifelse(object$y>=object$fitted.values,1,-1)
  if(standardized){
    X <- model.matrix(object)
    Xw <- X*matrix(sqrt(object$weights),nrow(X),ncol(X))
    salida <- svd(Xw)
    h <- apply(salida$u^2,1,sum)
    rd <- rd/sqrt(1-h)
  }
  if(plot.it){
    nano <- list(...)
    nano$x <- object$fitted.values
    nano$y <- rd
    if(is.null(nano$ylim)) nano$ylim <- c(min(-3.5,min(rd)),max(+3.5,max(rd)))
    if(is.null(nano$xlab)) nano$xlab <- "Fitted values"
    if(is.null(nano$ylab)) nano$ylab <- paste(type," - type residuals",sep="")
    if(is.null(nano$pch))  nano$pch  <- 20
    if(is.null(nano$labels)) labels <- 1:length(rd)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    abline(h=-3,lty=3)
    abline(h=+3,lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  rd <- as.matrix(rd)
  colnames(rd) <- type
  return(invisible(rd))
}

#' @title Normal QQ-plot with simulated envelope of residuals in Generalized Nonlinear Models
#' @description Produces a normal QQ-plot with simulated envelope of residuals for generalized nonlinear models.
#' @param object an object of the class \emph{gnm}.
#' @param rep an (optional) positive integer which allows to specify the number of replicates which should be used to build the simulated envelope. As default, \code{rep} is set to 25.
#' @param conf an (optional) value in the interval (0,1) indicating the confidence level which should be used to build the pointwise confidence intervals, which form the envelope. As default, \code{conf} is set to 0.95.
#' @param type a character string indicating the type of residuals which should be used. The available options are: randomized quantile ("quantile"), deviance ("deviance") and pearson ("pearson") residuals. As default, \code{type} is set to "quantile".
#' @param standardized an (optional) logical switch indicating if the residuals should be standardized by dividing by the square root of \eqn{(1-h)}, where \eqn{h} is a measure of leverage. As default, \code{standardized} is set to FALSE.
#' @param plot.it an (optional) logical switch indicating if the normal QQ-plot with simulated envelope of residuals is required or just the data matrix in which it is based. As default, \code{plot.it} is set to TRUE.
#' @param identify an (optional) positive integer indicating the number of individuals to identify on the QQ-plot with simulated envelope of residuals. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with the following four columns:
#' \tabular{ll}{
#' \code{Lower limit} \tab the quantile (1 - \code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'                    \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Median} \tab the quantile 0.5 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'               \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Upper limit} \tab the quantile (1 + \code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'                    \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Residuals} \tab the observed \code{type}-type residuals,\cr
#' }
#' @details In order to construct the simulated envelope, \code{rep} independent realizations of the response variable for each individual are simulated, which is
#' done by considering (1) the model assumption about the distribution of the response variable; (2) the estimation of the "linear" predictor parameters; and (3)
#' the estimation of the dispersion parameter. Each time, the vector of observed responses is replaced with one of the simulated samples, re-fitting the interest
#' model \code{rep} times. For each \eqn{i=1,2,...,n}, where \eqn{n} is the number of individuals in the sample, the \eqn{i}-th order statistic of the
#' \code{type}-type residuals is computed and then sorted for each replicate, giving a random sample of size \code{rep} of the \eqn{i}-th order statistic. In
#' other words, the simulated envelope is comprised of the quantiles (1 - \code{conf})/2 and (1 + \code{conf})/2 of the random sample of size \code{rep} of the
#' \eqn{i}-th order statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n}.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Davison A.C., Gigli A. (1989) Deviance Residuals and Normal Scores Plots. \emph{Biometrika} 76, 211-221.
#' @references Dunn P.K., Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics} 5, 236-244.
#' @references Pierce D.A., Schafer D.W. (1986) Residuals in Generalized Linear Models. \emph{Journal of the American Statistical Association} 81, 977-986.
#' @references Wei B.C. (1998). \emph{Exponential Family Nonlinear Models}. Springer, Singapore.
#' @seealso \link{envelope.lm}, \link{envelope.overglm}
#' @examples
#' ###### Example 1: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#'
#' #envelope(fit1, rep=50, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#' #         col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Assay of an Insecticide with a Synergist
#' data(Melanopus)
#' fit2 <- gnm(Killed/Exposed ~ b0 + b1*log(Insecticide-a1) + b2*Synergist/(a2 + Synergist),
#'             family=binomial(logit), weights=Exposed, start=c(b0=-3,b1=1.2,a1=1.7,b2=1.7,a2=2),
#'			   data=Melanopus)
#'
#' #envelope(fit2, rep=50, conf=0.95, type="pearson", col="red", pch=20, col.lab="blue",
#' #         col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Developmental rate of Drosophila melanogaster
#' data(Drosophila)
#' fit3 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
#'             start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)
#'
#' #envelope(fit3, rep=50, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#' #         col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 4: Radioimmunological Assay of Cortisol
#' data(Cortisol)
#' fit4 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
#'             start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)
#'
#' #envelope(fit4, rep=50, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#' #         col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' @method envelope gnm
#' @export
envelope.gnm <- function(object, rep=25, conf=0.95, type=c("quantile","deviance","pearson"), standardized=FALSE, plot.it=TRUE, identify, ...){
  no.offset <- function(x, preserve = NULL) {
    k <- 0
    proc <- function(x) {
      if (length(x) == 1) return(x)
      if (x[[1]] == as.name("offset") && !((k<<-k+1) %in% preserve)) return(x[[1]])
      replace(x, -1, lapply(x[-1], proc))
    }
    update(proc(x), . ~ . - offset)
  }
  type <- match.arg(type)
  .Theta <- function() return(.Theta)
  defaultW <- getOption("warn")
  options(warn = -1)
  if(object$family$family=="gaussian")
    object$family$simulate <- function(object,nsim){
      mus <- fitted(object)
      return(rnorm(length(mus),mean=mus,sd=sqrt(object$phi/weights)))
    }
  if(is.null(object$family$simulate)) stop(paste("family",object$family$family,"is not implemented!!"),call.=FALSE)
  if(any(object$prior.weights == 0)) stop("Only positive weights are supported!!",call.=FALSE)
  bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
  i <- 1
  mu <- object$fitted.values
  n <- length(mu)
  formul <- no.offset(object$formula)
  formula2 <- as.formula(paste("resp_ ~ ",paste(deparse(formul[[3]]),collapse="")))
  e <- matrix(0,n,rep)
  weights=object$prior.weights
  offset=object$offset
  object2 <- object
  object2$model <- NULL
  suppressMessages({
    while(i <= rep){
      resp <- object$family$simulate(object2,1)
      fits <- try(gnm(formula2,weights=weights,offset=offset,start=object$coefficients,family=object$family,data=data.frame(object$model,resp_=resp)),silent=TRUE)
      if(is.list(fits)){
        if(fits$converged==TRUE){
          rs <- residuals(fits,type=type,standardized=standardized)
          e[,i] <- sort(rs)
          setTxtProgressBar(bar,i)
          i <- i + 1
        }
      }
    }})
  close(bar)
  alpha <- 1 - max(0,min(1,abs(conf)))
  e <- as.matrix(e[,1:(i-1)])
  es <- apply(e,1,function(x) return(quantile(x,probs=c(alpha/2,0.5,1-alpha/2))))
  rd <- residuals(object,type=type,standardized=standardized)
  out_ <- as.matrix(cbind(t(es),sort(rd)))
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  if(plot.it){
    nano <- list(...)
    nano$y <- rd
    nano$type <- "p"
    if(is.null(nano$ylim)) nano$ylim <- 1.1*range(out_)
    if(is.null(nano$pch)) nano$pch <- 20
    if(is.null(nano$col)) nano$col <- "black"
    if(is.null(nano$xlab)) nano$xlab <- "Expected quantiles"
    if(is.null(nano$ylab)) nano$ylab <- "Observed quantiles"
    if(is.null(nano$main)) nano$main <- paste0("Normal QQ plot with simulated envelope\n of ",type,"-type residuals")
    if(is.null(nano$labels)) labels <- 1:length(rd)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    outm <- do.call("qqnorm",nano)
    lines(sort(outm$x),es[2,],xlab="",ylab="",main="", type="l",lty=3)
    lines(sort(outm$x),es[1,],xlab="",ylab="",main="", type="l",lty=1)
    lines(sort(outm$x),es[3,],xlab="",ylab="",main="", type="l",lty=1)
    if(!missingArg(identify)) identify(outm$x,outm$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  options(warn = defaultW)
  return(invisible(out_))
}

#' @title Dfbeta statistic for Generalized Nonlinear Models
#' @description Calculates an approximation of the parameter estimates that would be produced by deleting each case in turn,
#' which is known as the \emph{one-step approximation}. Additionally, the function can produce an index plot of the Dfbeta statistic
#' for some parameter specified by the argument \code{coefs}.
#' @param model an object of class \emph{gnm}.
#' @param coefs	an (optional) character string which (partially) match with the names of some model parameters.
#' @param identify an (optional) integer indicating the number of individuals to identify on the plot of the Dfbeta statistic.
#' This is only appropriate if \code{coefs} is specified.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used
#' to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main},
#' \code{sub}, \code{xlab}, \code{ylab}.
#' @details The \emph{one-step approximation} of the parameters estimates when the \eqn{i}-th case
#' is excluded from the dataset consists of the vector obtained as a result of the first iteration of the Fisher Scoring
#' algorithm when it is performed using: (1) a dataset in which the \eqn{i}-th case is excluded; and (2)
#' a starting value that is the estimate of the same model but based on the dataset including all cases.
#' @return A matrix with as many rows as cases in the sample and as many columns as parameters in the linear predictor. The
#' \eqn{i}-th row in that matrix corresponds to the difference between the parameters estimates obtained using all cases
#' and the \emph{one-step approximation} of those estimates when excluding the \eqn{i}-th case from the dataset.
#' @references Pregibon D. (1981). Logistic regression diagnostics. \emph{The Annals of Statistics}, 9, 705-724.
#' @references Wei B.C. (1998). \emph{Exponential Family Nonlinear Models}. Springer, Singapore.
#' @method dfbeta gnm
#' @export
#' @examples
#' ###### Example 1: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#'
#' fit1a <- update(fit1, subset=-c(1), start=coef(fit1), maxit=1)
#' coef(fit1) - coef(fit1a)
#'
#' dfbetas <- dfbeta(fit1)
#' round(dfbetas[1,],5)
#'
#' ###### Example 2: Assay of an Insecticide with a Synergist
#' data(Melanopus)
#' fit2 <- gnm(Killed/Exposed ~ b0 + b1*log(Insecticide-a1) + b2*Synergist/(a2 + Synergist),
#'             family=binomial(logit), weights=Exposed, start=c(b0=-3,b1=1.2,a1=1.7,b2=1.7,a2=2),
#'			   data=Melanopus)
#'
#' fit2a <- update(fit2, subset=-c(2), start=coef(fit2), maxit=1)
#' coef(fit2) - coef(fit2a)
#'
#' dfbetas <- dfbeta(fit2)
#' round(dfbetas[2,],5)
#'
#' ###### Example 3: Developmental rate of Drosophila melanogaster
#' data(Drosophila)
#' fit3 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
#'             start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)
#'
#' fit3a <- update(fit3, subset=-c(3), start=coef(fit3), maxit=1)
#' coef(fit3) - coef(fit3a)
#'
#' dfbetas <- dfbeta(fit3)
#' round(dfbetas[3,],5)
#'
#' ###### Example 4: Radioimmunological Assay of Cortisol
#' data(Cortisol)
#' fit4 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
#'             start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)
#'
#' fit4a <- update(fit4, subset=-c(4), start=coef(fit4), maxit=1)
#' coef(fit4) - coef(fit4a)
#'
#' dfbetas <- dfbeta(fit4)
#' round(dfbetas[4,],5)
#'
dfbeta.gnm <- function(model, coefs, identify, ...){
  X <- model.matrix(model)
  n <- nrow(X)
  p <- ncol(X)
  y <- model$y
  mus <- model$fitted.values
  Xw <- X*matrix(sqrt(model$weights),nrow(X),ncol(X))
  salida <- svd(Xw)
  h <- apply(salida$u^2,1,sum)
  dfbetas <- Xw%*%chol2inv(chol(crossprod(Xw)))*matrix((y-mus)*sqrt(model$prior.weights)/(sqrt(model$family$variance(mus))*(1-h)),n,p)
  colnames(dfbetas) <- rownames(coef(model))
  if(!missingArg(coefs)){
    ids <- grep(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(length(ids) > 0){
      nano <- list(...)
      if(is.null(nano$labels)) labels <- 1:nrow(dfbetas)
      else{
        labels <- nano$labels
        nano$labels <- NULL
      }
      nano$x <- 1:nrow(dfbetas)
      if(is.null(nano$xlab)) nano$xlab <- "Observation (i)"
      if(is.null(nano$type)) nano$type <- "h"
      if(is.null(nano$ylab)) nano$ylab <- expression(hat(beta)-hat(beta)[("- i")])
      oldpar <- par(no.readonly=TRUE)
      on.exit(par(oldpar))
      par(mfrow=c(1,length(ids)))
      for(i in 1:length(ids)){
        nano$y <- dfbetas[,ids[i]]
        nano$main <- colnames(dfbetas)[ids[i]]
        do.call("plot",nano)
        if(any(nano$y > 0)) abline(h=3*mean(nano$y[nano$y > 0]),lty=3)
        if(any(nano$y < 0)) abline(h=3*mean(nano$y[nano$y < 0]),lty=3)
        if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
      }
    }else stop(paste("There are no variables with the name",coefs,collapse=""),call.=FALSE)
  }
  return(dfbetas)
}

#' @title Cook's Distance for Generalized Nonlinear Models
#' @description Produces an approximation of the Cook's distance, better known as the \emph{one-step approximation},
#' for measuring the effect of deleting each observation in turn on the estimates of the parameters in a linear
#' predictor. Additionally, this function can produce an index plot of Cook's distance for all or a subset of the
#' parameters in the linear predictor (via the argument \code{coefs}).
#' @param model an object of class \emph{gnm}.
#' @param dispersion an (optional) value indicating the estimate of the dispersion parameter. As default, \code{dispersion} is set to \code{summary(object)$dispersion}.
#' @param plot.it an (optional) logical indicating if the plot is required or just the data matrix in which that
#' plot is based. As default, \code{plot.it} is set to FALSE.
#' @param coefs	an (optional) character string that matches (partially) some of the model parameter names.
#' @param identify an (optional) integer indicating the number of individuals to identify on the plot of the Cook's
#' distance. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used
#' to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main},
#' \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as individuals in the sample and one column with the values of the Cook's distance.
#' @details The Cook's distance consists of the \emph{distance} between two estimates of the parameters in the linear
#' predictor using a metric based on the (estimate of the) variance-covariance matrix. The first one set of estimates
#' is computed from a dataset including all individuals, and the second one is computed from a dataset in which the
#' \emph{i}-th individual is excluded. To avoid computational burden, the second set of estimates is replaced by its
#' \emph{one-step approximation}. See the \link{dfbeta.overglm} documentation.
#' @examples
#' ###### Example 1: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#'
#' cooks.distance(fit1, plot.it=TRUE, col="red", lty=1, lwd=1,
#'   col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Assay of an Insecticide with a Synergist
#' data(Melanopus)
#' fit2 <- gnm(Killed/Exposed ~ b0 + b1*log(Insecticide-a1) + b2*Synergist/(a2 + Synergist),
#'             family=binomial(logit), weights=Exposed, start=c(b0=-3,b1=1.2,a1=1.7,b2=1.7,a2=2),
#'			   data=Melanopus)
#'
#' ### Cook's distance just for the parameter "b1"
#' cooks.distance(fit2, plot.it=TRUE, coef="b1", col="red", lty=1, lwd=1,
#'   col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Developmental rate of Drosophila melanogaster
#' data(Drosophila)
#' fit3 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
#'             start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)
#'
#' cooks.distance(fit3, plot.it=TRUE, col="red", lty=1, lwd=1,
#'   col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 4: Radioimmunological Assay of Cortisol
#' data(Cortisol)
#' fit4 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
#'             start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)
#'
#' cooks.distance(fit4, plot.it=TRUE, col="red", lty=1, lwd=1,
#'   col.lab="blue", col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' @method cooks.distance gnm
#' @export
cooks.distance.gnm <- function(model, plot.it=FALSE, dispersion=NULL, coefs, identify,...){
  dfbetas <- dfbeta(model)
  met <- vcov(model, dispersion=dispersion)
  subst <- NULL
  if(!missingArg(coefs)){
    ids <- grepl(coefs,colnames(dfbetas),ignore.case=TRUE)
    if(sum(ids) > 0){
      subst <- colnames(dfbetas)[ids]
      dfbetas <- as.matrix(dfbetas[,ids])
      met <- as.matrix(met[ids,ids])
    }else stop(paste("There are no coefficients with the name",coefs,collapse=""),call.=FALSE)
  }
  met2 <- try(chol(met),silent=TRUE)
  if(is.matrix(met2)) met2 <- chol2inv(met2) else met2 <- solve(met)
  CD <- as.matrix(apply((dfbetas%*%met2)*dfbetas,1,mean))
  colnames(CD) <- "Cook's distance"
  if(plot.it){
    nano <- list(...)
    if(is.null(nano$labels)) labels <- 1:nrow(dfbetas)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    nano$x <- 1:nrow(dfbetas)
    nano$y <- CD
    if(is.null(nano$xlab)) nano$xlab <- "Observation (i)"
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- expression((1/p)(hat(beta)-hat(beta)[{(-~~i)}])^{T}~(Var(hat(beta)))^{-1}~(hat(beta)-hat(beta)[{(-~~i)}]))
    do.call("plot",nano)
    abline(h=3*mean(CD),lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  if(!is.null(subst)){
    message("The coefficients included in the Cook's distance are:\n")
    message(subst)
  }
  return(CD)
}

#' @title Adjusted R-squared in Generalized Nonlinear Models
#' @description Computes the adjusted deviance-based R-squared in generalized nonlinear models.
#' @param ... one or several objects of the class \emph{gnm}, which are obtained from the fit of generalized nonlinear models.
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param digits an (optional) integer value indicating the number of decimal places to be used. As default, \code{digits} is set to \code{max(3, getOption("digits") - 2)}.
#' @details The deviance-based R-squared is computed as \eqn{R^2=1 - Deviance/Null.Deviance}. Then,
#' the adjusted deviance-based R-squared is computed as
#' \eqn{1 - \frac{n-1}{n-p}(1-R^2)}, where \eqn{p} is the
#' number of parameters in the "linear" predictor and \eqn{n} is the sample size.
#' @return a matrix with the following columns
#' \tabular{ll}{
#' \code{Deviance} \tab value of the residual deviance,\cr
#' \tab \cr
#' \code{R-squared} \tab value of the deviance-based R-squared,\cr
#' \tab \cr
#' \code{df}       \tab number of parameters in the "linear" predictor,\cr
#' \tab \cr
#' \code{adj.R-squared} \tab value of the adjusted deviance-based R-squared,\cr
#' }
#' @method adjR2 gnm
#' @export
#' @examples
#' ###### Example 1: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#' fit2 <- update(fit1, family=Gamma(inverse))
#' adjR2(fit1,fit2)
#'
#' ###### Example 2: Developmental rate of Drosophila melanogaster
#' data(Drosophila)
#' fit1 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
#'             start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)
#' fit2 <- update(fit1, family=inverse.gaussian(log))
#' adjR2(fit1,fit2)
#'
#' ###### Example 3: Radioimmunological Assay of Cortisol
#' data(Cortisol)
#' fit1 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
#'             start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)
#' fit2 <- update(fit1, family=gaussian(identity))
#' adjR2(fit1,fit2)
#'
#' ###### Example 4: Age and Eye Lens Weight of Rabbits in Australia
#' data(rabbits)
#' fit1 <- gnm(wlens ~ b1 - b2/(age + b3), family=Gamma(log),
#'             start=c(b1=5.5,b2=130,b3=35), data=rabbits)
#' fit2 <- update(fit1, family=gaussian(log))
#' adjR2(fit1,fit2)
#'
adjR2.gnm <- function(...,digits=max(3, getOption("digits") - 2),verbose=TRUE){
  x <- list(...)
  if(any(unlist(lapply(x,function(xx) !is(xx,"gnm")))))
    stop("Only gnm-type objects are supported!!",call.=FALSE)
  out_ <- matrix(NA,length(x),4)
  call. <- match.call()
  for(i in 1:length(x)){
    out_[i,1] <- x[[i]]$deviance
    out_[i,3] <- length(x[[i]]$coefficients)
    out_[i,2] <- round(1 - x[[i]]$deviance/x[[i]]$null.deviance,digits=digits)
    out_[i,4] <- round(1 - (x[[i]]$deviance/x[[i]]$df.residual)/(x[[i]]$null.deviance/x[[i]]$df.null),digits=digits)
  }
  rownames(out_) <- as.character(call.[2:(length(x) + 1)])
  colnames(out_) <- c("Deviance","R-squared","df","adj.R-squared")
  if(length(x)==1){
    out_ <- as.numeric(out_[1,4])
    return(out_)
  }
  if(verbose) print(out_)
  return(invisible(out_))
}

#' @title Comparison of nested models in Generalized Nonlinear Models.
#' @description Allows to use the likelihood-ratio test to compare nested models in generalized nonlinear models.
#' @param object an object of the class \emph{gnm}.
#' @param ... another objects of the class \emph{gnm}.
#' @param verbose an (optional) logical indicating if should the report of results be printed. As default, \code{verbose}
#' is set to TRUE.
#' @return A matrix with the following three columns:
#' \tabular{ll}{
#' \code{Chi} \tab The value of the statistic of the test,\cr
#' \tab \cr
#' \code{Df}\tab The number of degrees of freedom,\cr
#' \tab \cr
#' \code{Pr(>Chi)} \tab The \emph{p}-value of the \code{test}-type test computed using the Chi-square distribution.\cr
#' }
#' @examples
#' ###### Example: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#'
#' fit2 <- update(fit1,Yield ~ I(b0 + b2/(Phosphorus + a2) + b3/(Potassium + a3)),
#'                start=c(b0=0.1,b2=1,b3=1,a2=15,a3=30))
#'
#' anova(fit2,fit1)
#'
#' @method anova gnm
#' @export
anova.gnm <- function(object,...,verbose=TRUE){
  x <- list(object,...)
  hast <- length(x)
  if(hast<=1) stop("At least two gnm-type objects are required!!",call.=FALSE)
  out_ <- matrix(0,hast-1,3)
  for(i in 2:hast){
    vars0 <- rownames(coef(x[[i-1]]))
    vars1 <- rownames(coef(x[[i]]))
    ids <- is.na(match(vars1,vars0))
    phi <- x[[i]]$phi
    sc <- (x[[i-1]]$deviance - x[[i]]$deviance)/phi
    df <- sum(ids)
    out_[i-1,] <- cbind(sc,df,1-pchisq(sc,df))
  }
  colnames(out_) <- c(" Chi  ", " df", " Pr(Chisq>)")
  rownames(out_) <- paste(1:(hast-1),"vs",2:hast)
  if(verbose){
    cat("\nLikelihood-ratio test\n\n")
    for(i in 1:hast) cat(paste("Model", i,": ",x[[i]]$formula[2],x[[i]]$formula[1],x[[i]]$formula[3:length(x[[i]]$formula)],collapse=""),"\n")
    cat("\n")
    printCoefmat(out_, P.values=TRUE, has.Pvalue=TRUE, digits=5, signif.legend=TRUE, cs.ind=2)
  }
  return(invisible(out_))
}

#' @title Test for Varying Dispersion Parameter in Generalized Nonlinear Models
#' @description Performs Rao's score test for varying dispersion parameter in
#' weighted and unweighted generalized nonlinear models in which the response
#' distribution is assumed to be Gaussian, Gamma, or inverse Gaussian.
#' @param model an object of the class \emph{gnm} where the distribution of the response
#' variable is assumed to be \code{gaussian}, \code{Gamma} or \code{inverse.gaussian}.
#' @param varformula an (optional) \code{formula} expression of the form \code{~ z1 + z2 + ... + zq} describing only the potential explanatory variables for the dispersion. As default, the same explanatory variables are taken as in the model for the mean.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @details From the generalized nonlinear model with varying dispersion in which
#' \eqn{\log(\phi)=\gamma_0 + \gamma_1z_1 + \gamma_2z_2 + ... + \gamma_qz_q}, where
#' \eqn{\phi} is the dispersion parameter of the distribution used to describe the
#' response variable, the Rao's score test (denoted here as \eqn{S}) to assess the
#' hypothesis \eqn{H_0: \gamma=0} versus \eqn{H_1: \gamma\neq 0} is computed,
#' where \eqn{\gamma=(\gamma_1,\ldots,\gamma_q)}.  The corresponding \emph{p}-value is
#' computed from the chi-squared distribution with \eqn{q} degrees of freedom,
#' that is, \emph{p}-value = Prob\eqn{[\chi^2_{q} > S]}. If the object \code{model}
#' corresponds to an unweighted generalized linear model, this test assesses assumptions
#' of constant variance and constant coefficient of variation on models in which the
#' response distribution is assumed to be Gaussian and Gamma, respectively.
#' @return a list list with components including
#' \tabular{ll}{
#' \code{statistic} \tab value of the Rao's score test (\eqn{S}),\cr
#' \tab \cr
#' \code{df}        \tab number of degrees of freedom (\eqn{q}),\cr
#' \tab \cr
#' \code{p.value}   \tab \emph{p}-value of the test,\cr
#' \tab \cr
#' \code{vars}   \tab names of explanatory variables for the dispersion parameter,\cr
#' }
#' @examples
#' ###### Example 1: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#' vdtest(fit1)
#' vdtest(fit1,varformula = ~ Nitrogen + Phosphorus + Potassium)
#'
#' ###### Example 2: Developmental rate of Drosophila melanogaster
#' data(Drosophila)
#' fit2 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
#'             start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)
#' vdtest(fit2)
#' vdtest(fit2,varformula = ~ Temp)
#' vdtest(fit2,varformula = ~ log(Temp))
#'
#' ###### Example 3: Radioimmunological Assay of Cortisol
#' data(Cortisol)
#' fit3 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
#'             start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)
#' vdtest(fit3)
#' vdtest(fit3,varformula = ~ lDose)
#' vdtest(fit3,varformula = ~ exp(lDose))
#'
#' ###### Example 4: Age and Eye Lens Weight of Rabbits in Australia
#' data(rabbits)
#' fit4 <- gnm(wlens ~ b1 - b2/(age + b3), family=Gamma(log),
#'             start=c(b1=5.5,b2=130,b3=35), data=rabbits)
#' vdtest(fit4)
#' vdtest(fit4,varformula = ~ age)
#' vdtest(fit4,varformula = ~ log(age))
#' @method vdtest gnm
#' @export
#' @references Wei B.-C., Shi, J.-Q., Fung W.-K., Hu Y.-Q. (1998) Testing for Varying Dispersion in Exponential Family Nonlinear Models. \emph{Annals of the Institute of Statistical Mathematics} 50, 277–294.
#'
#' @seealso \link{vdtest.lm}, \link{vdtest.glm}

vdtest.gnm <- function(model,varformula,verbose=TRUE,...){
  if(!(model$family$family %in% c("gaussian","Gamma","inverse.gaussian")))
    stop("Only gaussian, Gamma and inverse.gaussian families are supported!!",call.=FALSE)
  if(missingArg(varformula)){
    varformula <- model$formula
    b <- c(as.character(model$formula[[2]]),rownames(model$coefficients))
    a <- all.vars(varformula)
    formula <- paste("~ 0 + ",paste(a[!(a %in% b)],collapse="+"))
    formula <- as.formula(formula)
  }else formula <- varformula
  if(is.null(model$call$data)) Z <- model.frame(formula)
  else Z <- model.frame(formula,eval(model$call$data))
  if(colnames(Z)[1]=="(Intercept)") Z <- Z[,-1]
  if(!is.null(model$call$subset)) Z <- Z[eval(model$call$subset,eval(model$call$data)),]
  Z <- as.matrix(Z)
  n <- nrow(Z)
  p <- ncol(Z)
  Z <- cbind(1,Z)
  Z2 <- Z
  y <- model$y
  mus <- fitted(model)
  w <- model$prior.weights
  if(model$family$family=="gaussian"){
    phies <- mean((y-mus)^2*w)
    tau <- (y-mus)^2*w/phies - 1
  }
  if(model$family$family=="inverse.gaussian"){
    phies <- mean((y-mus)^2*w/(mus^2*y))
    tau <- (y-mus)^2*w/(mus^2*y*phies) - 1
  }
  if(model$family$family=="Gamma"){
    phies <- model$phi
    phies <- uniroot(function(x) sum((y/mus + log(mus*x/(w*y)) + psigamma(w/x) - 1)*w), lower=phies*(0.1), upper=phies*(1.9))$root
    tau <- sqrt(2)*(y/mus + log(mus*phies/(w*y)) + psigamma(w/phies) - 1)*(w/phies)
    Z2 <- Z*matrix(sqrt(psigamma(w/phies,1)*(w/phies)^2 - w/phies),n,p+1)
  }
  Zstar <- chol2inv(chol(t(Z2)%*%Z2))[-1,-1]
  sc = as.numeric(0.5*(t(tau)%*%Z[,-1])%*%Zstar%*%(t(Z[,-1])%*%tau))
  if(verbose){
    cat("\n             Score test for varying dispersion parameter\n\n")
    cat("          Statistic = ",round(sc,digits=5),"\n degrees of freedom = ",p,"\n            p-value = ",format.pval(1-pchisq(sc,p)),"\n\n")
  }
  return(invisible(list(statistic=sc,df=p,p.value=format.pval(1-pchisq(sc,p)),vars=colnames(Z)[-1])))
}

#' @title Test for Varying Dispersion Parameter in Generalized Linear Models
#' @description Performs Rao's score test for varying dispersion parameter in
#' weighted and unweighted generalized linear models in which the response
#' distribution is assumed to be Gaussian, Gamma, or inverse Gaussian.
#' @param model an object of the class \emph{glm} where the distribution of the response
#' variable is assumed to be \code{gaussian}, \code{Gamma} or \code{inverse.gaussian}.
#' @param varformula an (optional) \code{formula} expression of the form \code{~ z1 + z2 + ... + zq} describing only the potential explanatory variables for the dispersion. As default, the same explanatory variables are taken as in the model for the mean.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As default, \code{verbose} is set to TRUE.
#' @param ...	further arguments passed to or from other methods.
#' @details From the generalized linear model with varying dispersion in which
#' \eqn{\log(\phi)=\gamma_0 + \gamma_1z_1 + \gamma_2z_2 + ... + \gamma_qz_q}, where
#' \eqn{\phi} is the dispersion parameter of the distribution used to describe the
#' response variable, the Rao's score test (denoted here as \eqn{S}) to assess the
#' hypothesis \eqn{H_0: \gamma=0} versus \eqn{H_1: \gamma\neq 0} is computed,
#' where \eqn{\gamma=(\gamma_1,\ldots,\gamma_q)}.  The corresponding \emph{p}-value is
#' computed from the chi-squared distribution with \eqn{q} degrees of freedom,
#' that is, \emph{p}-value = Prob\eqn{[\chi^2_{q} > S]}. If the object \code{model}
#' corresponds to an unweighted generalized linear model, this test assesses assumptions
#' of constant variance and constant coefficient of variation on models in which the
#' response distribution is assumed to be Gaussian and Gamma, respectively.
#' @return a list list with components including
#' \tabular{ll}{
#' \code{statistic} \tab value of the Rao's score test (\eqn{S}),\cr
#' \tab \cr
#' \code{df}        \tab number of degrees of freedom (\eqn{q}),\cr
#' \tab \cr
#' \code{p.value}   \tab \emph{p}-value of the test,\cr
#' \tab \cr
#' \code{vars}   \tab names of explanatory variables for the dispersion parameter,\cr
#' }
#' @examples
#' ###### Example 1: Fuel consumption of automobiles
#' Auto <- ISLR::Auto
#' fit1 <- glm(mpg ~ weight*horsepower, family=inverse.gaussian("log"), data=Auto)
#' vdtest(fit1)
#' vdtest(fit1,varformula= ~ weight + horsepower)
#' vdtest(fit1,varformula= ~ log(weight) + log(horsepower))
#'
#' ###### Example 2: Hill races in Scotland
#' data(races)
#' fit2 <- glm(rtime ~ log(distance) + cclimb, family=Gamma("log"), data=races)
#' vdtest(fit2)
#' vdtest(fit2,varformula= ~ distance + cclimb)
#' vdtest(fit2,varformula= ~ log(distance) + log(cclimb))
#'
#' ###### Example 3: Mammal brain and body weights
#' data(brains)
#' fit3 <- glm(BrainWt ~ log(BodyWt), family=Gamma("log"), data=brains)
#' vdtest(fit3)
#' vdtest(fit3,varformula= ~ BodyWt)
#' @method vdtest glm
#' @export
#' @references Wei B.-C., Shi, J.-Q., Fung W.-K., Hu Y.-Q. (1998) Testing for Varying Dispersion in Exponential Family Nonlinear Models. \emph{Annals of the Institute of Statistical Mathematics} 50, 277–294.
#'
#' @seealso \link{vdtest.lm}, \link{vdtest.gnm}




vdtest.glm <- function(model,varformula,verbose=TRUE,...){
  if(!(model$family$family %in% c("gaussian","Gamma","inverse.gaussian")))
    stop("Only gaussian, Gamma and inverse.gaussian families are supported!!",call.=FALSE)
  if(missingArg(varformula)) varformula <- model$formula
  if(is.null(model$call$data)) Z <- model.matrix(varformula)
  else Z <- model.matrix(varformula,eval(model$call$data))
  if(!is.null(model$call$subset)) Z <- Z[eval(model$call$subset,eval(model$call$data)),]
  n <- nrow(Z)
  if(colnames(Z)[1]!="(Intercept)"){
    out_ <- colnames(Z)
    Z <- cbind(1,Z)
  }else out_ <- colnames(Z)[-1]
  p <- ncol(Z) - 1
  Z2 <- Z
  y <- model$y
  mus <- fitted(model)
  w <- model$prior.weights
  if(model$family$family=="gaussian"){
    phies <- mean((y-mus)^2*w)
    tau <- (y-mus)^2*w/phies - 1
  }
  if(model$family$family=="inverse.gaussian"){
    phies <- mean((y-mus)^2*w/(mus^2*y))
    tau <- (y-mus)^2*w/(mus^2*y*phies) - 1
  }
  if(model$family$family=="Gamma"){
    phies <- summary(model)$dispersion
    phies <- uniroot(function(x) sum((y/mus + log(mus*x/(w*y)) + psigamma(w/x) - 1)*w), lower=phies*(0.1), upper=phies*(1.9))$root
    tau <- sqrt(2)*(y/mus + log(mus*phies/(w*y)) + psigamma(w/phies) - 1)*(w/phies)
    Z2 <- Z*matrix(sqrt(psigamma(w/phies,1)*(w/phies)^2 - w/phies),n,p+1)
  }
  Zstar <- chol2inv(chol(t(Z2)%*%Z2))[-1,-1]
  sc = as.numeric(0.5*(t(tau)%*%Z[,-1])%*%Zstar%*%(t(Z[,-1])%*%tau))
  if(verbose){
    cat("\n             Score test for varying dispersion parameter\n\n")
    cat("          Statistic = ",round(sc,digits=5),"\n degrees of freedom = ",p,"\n            p-value = ",format.pval(1-pchisq(sc,p)),"\n\n")
  }
  return(invisible(list(statistic=sc,df=p,p.value=format.pval(1-pchisq(sc,p)),vars=out_)))
}

#' @title Normal QQ-plot with simulated envelope of residuals in Generalized Linear Models
#' @description Produces a normal QQ-plot with simulated envelope of residuals for generalized linear models.
#' @param object an object of the class \emph{glm}.
#' @param rep an (optional) positive integer which allows to specify the number of replicates which should be used to build the simulated envelope. As default, \code{rep} is set to 25.
#' @param conf an (optional) value in the interval (0,1) indicating the confidence level which should be used to build the pointwise confidence intervals, which form the envelope. As default, \code{conf} is set to 0.95.
#' @param type an (optional) character string indicating the type of residuals which should be used. The available options are: randomized quantile ("quantile"), deviance ("deviance") and pearson ("pearson") residuals. As default, \code{type} is set to "quantile".
#' @param standardized an (optional) logical switch indicating if the residuals should be standardized by dividing by the square root of \eqn{(1-h)}, where \eqn{h} is a measure of leverage. As default, \code{standardized} is set to FALSE.
#' @param plot.it an (optional) logical switch indicating if the normal QQ-plot with simulated envelope of residuals is required or just the data matrix in which it is based. As default, \code{plot.it} is set to TRUE.
#' @param identify an (optional) positive integer indicating the number of individuals to identify on the QQ-plot with simulated envelope of residuals. This is only appropriate if \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE} then \code{...} may be used to include graphical parameters to customize the plot. For example,  \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix with the following four columns:
#' \tabular{ll}{
#' \code{Lower limit} \tab the quantile (1 - \code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'                    \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Median} \tab the quantile 0.5 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'               \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Upper limit} \tab the quantile (1 + \code{conf})/2 of the random sample of size \code{rep} of the \eqn{i}-th order\cr
#'                    \tab  statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n},\cr
#' \tab \cr
#' \code{Residuals} \tab the observed \code{type}-type residuals,\cr
#' }
#' @details In order to construct the simulated envelope, \code{rep} independent realizations of the response variable for each individual are simulated, which is
#' done by considering (1) the model assumption about the distribution of the response variable; (2) the estimation of the linear predictor parameters; and (3)
#' the estimation of the dispersion parameter. Each time, the vector of observed responses is replaced with one of the simulated samples, re-fitting the interest
#' model \code{rep} times. For each \eqn{i=1,2,...,n}, where \eqn{n} is the number of individuals in the sample, the \eqn{i}-th order statistic of the
#' \code{type}-type residuals is computed and then sorted for each replicate, giving a random sample of size \code{rep} of the \eqn{i}-th order statistic. In
#' other words, the simulated envelope is comprised of the quantiles (1 - \code{conf})/2 and (1 + \code{conf})/2 of the random sample of size \code{rep} of the
#' \eqn{i}-th order statistic of the \code{type}-type residuals for \eqn{i=1,2,...,n}.
#' @references Atkinson A.C. (1985) \emph{Plots, Transformations and Regression}. Oxford University Press, Oxford.
#' @references Davison A.C., Gigli A. (1989) Deviance Residuals and Normal Scores Plots. \emph{Biometrika} 76, 211-221.
#' @references Dunn P.K., Smyth G.K. (1996) Randomized Quantile Residuals. \emph{Journal of Computational and Graphical Statistics} 5, 236-244.
#' @references Pierce D.A., Schafer D.W. (1986) Residuals in Generalized Linear Models. \emph{Journal of the American Statistical Association} 81, 977-986.
#' @references Wei B.C. (1998). \emph{Exponential Family Nonlinear Models}. Springer, Singapore.
#' @seealso \link{envelope.lm}, \link{envelope.gnm}, \link{envelope.overglm}
#' @examples
#' ###### Example 1:
#' burn1000 <- aplore3::burn1000
#' burn1000 <- within(burn1000, death <- factor(death, levels=c("Dead","Alive")))
#' fit1 <- glm(death ~ age*inh_inj + tbsa*inh_inj, family=binomial("logit"), data=burn1000)
#' envelope(fit1, rep=50, conf=0.95, type="pearson", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 2: Fuel consumption of automobiles
#' Auto <- ISLR::Auto
#' fit2 <- glm(mpg ~ horsepower*weight, family=inverse.gaussian("log"), data=Auto)
#' envelope(fit2, rep=50, conf=0.95, type="pearson", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 3: Skin cancer in women
#' data(skincancer)
#' fit3 <- glm(cases ~ city + ageC, offset=log(population), family=poisson, data=skincancer)
#' envelope(fit3, rep=100, conf=0.95, type="quantile", col="red", pch=20,col.lab="blue",
#'          col.axis="blue",col.main="black",family="mono",cex=0.8)
#'
#' ###### Example 4: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit4 <- glm(infections ~ frequency + location, family=poisson(log), data=swimmers)
#' envelope(fit4, rep=100, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' ###### Example 5: Agents to stimulate cellular differentiation
#' data(cellular)
#' fit5 <- glm(cbind(cells,200-cells) ~ tnf + ifn, family=binomial(logit), data=cellular)
#' envelope(fit5, rep=100, conf=0.95, type="quantile", col="red", pch=20, col.lab="blue",
#'          col.axis="blue", col.main="black", family="mono", cex=0.8)
#'
#' @method envelope glm
#' @export
envelope.glm <- function(object, rep=25, conf=0.95, type=c("quantile","deviance","pearson"), standardized=FALSE, plot.it=TRUE, identify, ...){
  type <- match.arg(type)
  .Theta <- function() return(.Theta)
  defaultW <- getOption("warn")
  options(warn = -1)
  if(object$family$family=="gaussian")
    object$family$simulate <- function(object,nsim){
      mus <- fitted(object)
      return(rnorm(length(mus),mean=mus,sd=sqrt(object$phi/weights)))
    }
  if(is.null(object$family$simulate)) stop(paste("family",object$family$family,"is not implemented!!"),call.=FALSE)
  if(any(object$prior.weights == 0)) stop("Only positive weights are supported!!",call.=FALSE)
  bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
  i <- 1
  mu <- object$fitted.values
  n <- length(mu)
  e <- matrix(0,n,rep)
  weights=object$prior.weights
  offset=object$offset
  X <- model.matrix(object)
  object2 <- object
  object2$model <- NULL
  suppressMessages({
    while(i <= rep){
      resp <- object$family$simulate(object2,1)
      fits <- try(glm(resp ~ 0+X,weights=weights,offset=offset,start=object$coefficients,family=object$family),silent=TRUE)
      if(is.list(fits)){
        if(fits$converged==TRUE){
          rs <- residuals2(fits,type=type,standardized=standardized)
          e[,i] <- sort(rs)
          setTxtProgressBar(bar,i)
          i <- i + 1
        }
      }
    }})
  close(bar)
  alpha <- 1 - max(0,min(1,abs(conf)))
  e <- as.matrix(e[,1:(i-1)])
  es <- apply(e,1,function(x) return(quantile(x,probs=c(alpha/2,0.5,1-alpha/2))))
  rd <- residuals2(object,type=type,standardized=standardized)
  out_ <- as.matrix(cbind(t(es),sort(rd)))
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  if(plot.it){
    nano <- list(...)
    nano$y <- rd
    nano$type <- "p"
    if(is.null(nano$ylim)) nano$ylim <- 1.1*range(out_)
    if(is.null(nano$pch)) nano$pch <- 20
    if(is.null(nano$col)) nano$col <- "black"
    if(is.null(nano$xlab)) nano$xlab <- "Expected quantiles"
    if(is.null(nano$ylab)) nano$ylab <- "Observed quantiles"
    if(is.null(nano$main)) nano$main <- paste0("Normal QQ plot with simulated envelope\n of ",type,"-type residuals")
    if(is.null(nano$labels)) labels <- 1:length(rd)
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    outm <- do.call("qqnorm",nano)
    lines(sort(outm$x),es[2,],xlab="",ylab="",main="", type="l",lty=3)
    lines(sort(outm$x),es[1,],xlab="",ylab="",main="", type="l",lty=1)
    lines(sort(outm$x),es[3,],xlab="",ylab="",main="", type="l",lty=1)
    if(!missingArg(identify)) identify(outm$x,outm$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  options(warn = defaultW)
  return(invisible(out_))
}

#' @title Test for zero-excess in Count Regression Models
#' @description Allows to assess if the observed number of zeros is significantly higher than expected according to the fitted count regression model (poisson or negative binomial).
#' @param object an object of the class \code{glm}, for poisson regression models, or an object of the class \code{overglm}, for negative binomial regression models.
#' @param alternative an (optional) character string indicating the alternative hypothesis. There are three options: excess of zeros ("excess"), lack of zeros ("lack"), and both ("both"). As a default, \code{type} is set to "excess".
#' @param method an (optional) character string indicating the method to calculate the mean and variance of the difference between observed and estimated expected number of zeros. There are two options: parametric bootstrapping ("boot") and naive ("naive"). As a default, \code{type} is set to "boot".
#' @param rep an (optional) positive integer which allows to specify the number of replicates which should be used by the parametric bootstrapping. As a default, \code{rep} is set to 100.
#' @param verbose an (optional) logical switch indicating if should the report of results be printed. As a default, \code{verbose} is set to TRUE.
#' @return A matrix with 1 row and the following columns:
#' \tabular{ll}{
#' \code{Observed} \tab the observed number of zeros,\cr
#' \tab \cr
#' \code{Expected}\tab the expected number of zeros,\cr
#' \tab \cr
#' \code{z-value}\tab the value of the statistical test,\cr
#' \tab \cr
#' \code{p.value}\tab the p-value of the statistical test.\cr
#' }
#' @details
#' According to the formulated count regression model, we have that \eqn{Y_i\sim P(y;\mu_i,\phi)}
#' for \eqn{i=1,\ldots,n} are independent variables. Consequently, the expected number of zeros can
#' be estimated by \eqn{P(0;\hat{\mu}_i,\hat{\phi})} for \eqn{i=1,\ldots,n}, where \eqn{\hat{\mu}_i}
#' and \eqn{\hat{\phi}} represent the estimates of \eqn{\mu_i} and \eqn{\phi}, respectively, obtained
#' from the fitted model. Thus, the statistical test can be defined as the standardized difference
#' between the observed and (estimated) expected number of zeros. The standard normal distribution
#' tends to be the distribution of that statistic when the sample size, \eqn{n}, tends to infinity.
#' In He, Zhang, Ye, and Tang (2019), the above approach is called a naive test since it ignores the
#' sampling variation associated with the estimated model parameters. To correct this, parametric
#' bootstrapping is used to estimate the mean and variance of the difference between the (estimated)
#' expected and observed number of zeros.
#' @references He Hua, Zhang Hui, Ye Peng, Tang Wan (2019) A test of inflated zeros
#' for Poisson regression models, \emph{Statistical Methods in Medical Research} 28,
#' 1157-1169.
#' @examples
#' ####### Example 1: Self diagnozed ear infections in swimmers
#' data(swimmers)
#' fit1 <- glm(infections ~ frequency + location, family=poisson, data=swimmers)
#' zero.excess(fit1,rep=50)
#' fit2 <- overglm(infections ~ frequency + location, family="nb1", data=swimmers)
#' zero.excess(fit2,rep=50)
#'
#' ####### Example 2: Article production by graduate students in biochemistry PhD programs
#' bioChemists <- pscl::bioChemists
#' fit1 <- glm(art ~ fem + kid5 + ment, family=poisson, data=bioChemists)
#' zero.excess(fit1,rep=50)
#' fit2 <- overglm(art ~ fem + kid5 + ment, family="nb1", data=bioChemists)
#' zero.excess(fit2,rep=50)

#' ####### Example 3: Roots Produced by the Columnar Apple Cultivar Trajan
#' data(Trajan)
#' fit1 <- glm(roots ~ photoperiod, family=poisson, data=Trajan)
#' zero.excess(fit1,rep=50)
#' fit2 <- overglm(roots ~ photoperiod, family="nbf", data=Trajan)
#' zero.excess(fit2,rep=50)
#'
#' @seealso \link{overglm}, \link{zeroinf}
#' @export zero.excess
zero.excess <- function(object, alternative=c("excess","lack","both"), method=c("boot","naive"), rep=100, verbose=TRUE){
  if(is(object,"overglm")){
    if(!(object$family$family %in% c("nbf","nb1","nb2"))) stop("Only 'nb1', 'nb2' and 'nbf' families of overglm-type objects are supported!!",call.=FALSE)
  }else{
    if(is(object,"glm")){
      if(object$family$family!="poisson") stop("Only 'poisson' family of glm-type objects are supported!!",call.=FALSE)
    }else{
      stop("Only 'nb1', 'nb2' and 'nbf' families of overglm-type objects and 'poisson' family of glm-type objects are supported!!",call.=FALSE)
    }
  }
  alternative <- match.arg(alternative)
  method <- match.arg(method)
  defaultW <- getOption("warn")
  options(warn = -1)
  mu <- object$fitted.values
  n <- length(mu)
  X <- model.matrix(object)
  p0 <- exp(-mu)
  if(is.null(object$offset)) offs <- rep(0,n) else offs <- object$offset
  if(object$family$family %in% c("nb1","nb2","nbf")){
    phi <- exp(object$coefficients[object$parms[1] + 1])
    tau <- switch(object$family$family,nb1=0,nb2=-1,nbf=object$coefficients[object$parms[1] + 2])
    familia <- paste0(object$family$family,"(",object$family$link,")")
    a <- 1/(phi*mu^tau)
    p0 <- (a/(mu + a))^a
  }
  if(method=="boot"){
    rep <- max(1,floor(abs(rep)))
    e <- matrix(0,rep,1)
    bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
    i <- 1
    while(i <= rep){
      if(object$family$family %in% c("nb1","nb2","nbf")){
        resp. <- rnbinom(n=n,mu=mu,size=1/(phi*mu^tau))
        fits <- try(overglm(resp. ~ -1 + X + offset(offs),start=coef(object),weights=object$prior.weights,family=familia),silent=TRUE)
        if(is.list(fits)){
          if(fits$converged==TRUE){
            mus <- fitted(fits)
            phis <- exp(fits$coefficients[object$parms[1] + 1])
            taus <- switch(object$family$family,nb1=0,nb2=-1,nbf=fits$coefficients[object$parms[1] + 2])
            as <- 1/(phis*mus^taus)
            p0s <- (as/(mus + as))^as
            e[i] <- sum(ifelse(fits$y==0,1,0) - p0s)
            setTxtProgressBar(bar,i)
            i <- i + 1
          }
        }
      }
      if(object$family$family=="poisson"){
        resp. <- rpois(n=n,lambda=mu)
        fits <- try(glm.fit(x=X,y=resp.,family=object$family,offset=offs,start=coef(object),weights=object$prior.weights),silent=TRUE)
        if(is.list(fits)){
          if(fits$converged==TRUE){
            e[i] <- sum(ifelse(fits$y==0,1,0) - exp(-fitted(fits)))
            setTxtProgressBar(bar,i)
            i <- i + 1
          }
        }
      }
    }
    options(warn = defaultW)
    z <- (sum(object$y==0) - sum(p0) - mean(e))/sd(e)
  }else z <- (sum(object$y==0)-sum(p0))/sqrt(sum(p0*(1-p0)))
  pv <- 1-pnorm(z)
  labp <- "Pr(>z)"
  if(alternative=="lack"){
    pv <- pnorm(z)
    labp <- "Pr(<z)"
  }
  if(alternative=="both"){
    pv <- 1-pchisq(z^2,1)
    labp <- "Pr(>|z|)"
  }
  out_ <- matrix(cbind(sum(object$y==0),sum(p0),z,pv),1,4)
  colnames(out_) <- c("Observed","Expected","z-value",labp)
  rownames(out_) <- ""
  if(verbose){
    cat("\n  Number of Zeros\n")
    printCoefmat(out_, P.values=TRUE, has.Pvalue=TRUE, digits=5, signif.legend=FALSE, cs.ind=2)
  }
  return(invisible(out_))
}

#' @title Local Influence for Generalized Nonlinear Models
#' @description Computes some measures and, optionally, display	graphs of them to perform
#' influence analysis based on the approaches described by Cook (1986).
#' @param object an object of class \emph{gnm}.
#' @param type an (optional) character string indicating the type of approach to study the
#' local influence. The options are: the absolute value of the elements of the eigenvector which corresponds to the maximum absolute eigenvalue ("local"); and the absolute value of the elements of the main diagonal ("total"). As default, \code{type} is set to "total".
#' @param perturbation an (optional) character string indicating the perturbation scheme
#' to apply. The options are: case weight perturbation of observations ("case-weight") and perturbation of response ("response"). As default, \code{perturbation} is set to "case-weight".
#' @param plot.it an (optional) logical indicating if the plot of the measures of local
#' influence is required or just the data matrix in which that plot is based. As default,
#' \code{plot.it} is set to FALSE.
#' @param coefs	an (optional) character string which (partially) match with the names of
#' some of the parameters in the 'linear' predictor.
#' @param identify an (optional) integer indicating the number of observations to identify
#' on the plot of the measures of local influence. This is only appropriate if
#' \code{plot.it=TRUE}.
#' @param ... further arguments passed to or from other methods. If \code{plot.it=TRUE}
#' then \code{...} may be used to include graphical parameters to customize the plot. For example, \code{col}, \code{pch}, \code{cex}, \code{main}, \code{sub}, \code{xlab}, \code{ylab}.
#' @return A matrix as many rows as observations in the sample and one column with the values of the measures of local influence.
#' @method localInfluence gnm
#' @export
#' @references Cook D. (1986) Assessment of Local Influence. \emph{Journal of the Royal Statistical Society: Series B (Methodological)} 48, 133-155.
#' @references Thomas W., Cook D. (1989) Assessing Influence on Regression Coefficients in Generalized Linear Models. \emph{Biometrika} 76, 741-749.
#' @examples
#' ###### Example 1: The effects of fertilizers on coastal Bermuda grass
#' data(Grass)
#' fit1 <- gnm(Yield ~ b0 + b1/(Nitrogen + a1) + b2/(Phosphorus + a2) + b3/(Potassium + a3),
#'             family=gaussian(inverse), start=c(b0=0.1,b1=13,b2=1,b3=1,a1=45,a2=15,a3=30), data=Grass)
#'
#' localInfluence(fit1, type="local", perturbation="case-weight", plot.it=TRUE, col="red",
#'                lty=1, lwd=1, col.lab="blue", col.axis="blue", col.main="black", family="mono")
#'
#' ###### Example 2: Assay of an Insecticide with a Synergist
#' data(Melanopus)
#' fit2 <- gnm(Killed/Exposed ~ b0 + b1*log(Insecticide-a1) + b2*Synergist/(a2 + Synergist),
#'             family=binomial(logit), weights=Exposed, start=c(b0=-3,b1=1.2,a1=1.7,b2=1.7,a2=2),
#'			   data=Melanopus)
#'
#' ### Local Influence just for the parameter "b1"
#' localInfluence(fit2, type="local", perturbation="case-weight", plot.it=TRUE, coefs="b1", col="red",
#'                lty=1, lwd=1, col.lab="blue", col.axis="blue", col.main="black", family="mono")
#'
#' ###### Example 3: Developmental rate of Drosophila melanogaster
#' data(Drosophila)
#' fit3 <- gnm(Duration ~ b0 + b1*Temp + b2/(Temp-a), family=Gamma(log),
#'             start=c(b0=3,b1=-0.25,b2=-210,a=55), weights=Size, data=Drosophila)
#'
#' localInfluence(fit3, type="total", perturbation="case-weight", plot.it=TRUE, col="red",
#'                lty=1, lwd=1, col.lab="blue", col.axis="blue", col.main="black", family="mono")
#'
#' ###### Example 4: Radioimmunological Assay of Cortisol
#' data(Cortisol)
#' fit4 <- gnm(Y ~ b0 + (b1-b0)/(1 + exp(b2+ b3*lDose))^b4, family=Gamma(identity),
#'             start=c(b0=130,b1=2800,b2=3,b3=3,b4=0.5), data=Cortisol)
#'
#' localInfluence(fit4, type="total", perturbation="case-weight", plot.it=TRUE, col="red",
#'                lty=1, lwd=1, col.lab="blue", col.axis="blue", col.main="black", family="mono")
#'
localInfluence.gnm <- function(object,type=c("total","local"),perturbation=c("case-weight","response"),coefs,plot.it=FALSE,identify,...){
  type <- match.arg(type)
  perturbation <- match.arg(perturbation)
  subst <- NULL
  if(!missingArg(coefs)){
    ids <- grepl(coefs,rownames(object$coefficients),ignore.case=TRUE)
    if(sum(ids) > 0) subst <- rownames(object$coefficients)[ids]
    else stop(paste("There are no coefficients with the name",coefs,collapse=""),call.=FALSE)
  }
  beta <- object$coefficients
  p <- length(beta)
  y <- object$y
  X <- model.matrix(object$terms, object$model, contrasts=object$contrasts)
  pars <- rownames(object$coefficients)
  etaf <- function(beta){
    temp <- deparse(object$formula[[3]])
    for(i in 1:p) temp <- gsub(pars[i],beta[i],temp)
    matrix(eval(parse(text=temp),data.frame(X)),ncol=1)
  }
  etas <- etaf(beta) + object$offset
  mus <- object$family$linkinv(etas)
  Der <- function(beta) matrix(jacobian(etaf,beta),ncol=p)
  Ders <- Der(beta)
  n <- nrow(Ders)
  S <- function(b){
    Ders <- Der(b)
    etas <- etaf(b) + object$offset
    mus <- object$family$linkinv(etas)
    matrix(crossprod(Ders,object$prior.weights*(y-mus)*object$family$mu.eta(etas)/object$family$variance(mus)),p,1)
  }
  Qpp <- -jacobian(S,beta)
  if(perturbation=="case-weight")
    Delta <- matrix(object$prior.weights*(y-mus)*object$family$mu.eta(etas)/object$family$variance(mus),n,p)*Ders
  else Delta <- matrix(object$family$mu.eta(etas)/sqrt(object$family$variance(mus)/object$prior.weights),n,p)*Ders
  Qpp2 <- try(chol(Qpp),silent=TRUE)
  if(is.matrix(Qpp2)) Qpp2 <- chol2inv(Qpp2) else Qpp2 <- solve(Qpp)
  if(!is.null(subst)) Qpp2[-ids,-ids] <- Qpp2[-ids,-ids] - solve(Qpp[-ids,-ids])
  li <- tcrossprod(Delta,t(Qpp2))
  if(type=="local"){
    tol <- 1
    bnew <- matrix(rnorm(nrow(li)),nrow(li),1)
    while(tol > 0.000001){
      bold <- bnew
      bnew <- tcrossprod(li,t(crossprod(Delta,bold)))
      bnew <- bnew/sqrt(sum(bnew^2))
      tol <- max(abs((bnew - bold)/bold))
    }
    out_ <- abs(bnew/sqrt(sum(bnew^2)))
  }else out_ <- apply(li*Delta,1,sum)
  out_ <- matrix(out_,nrow=length(out_))
  rownames(out_) <- 1:n
  colnames(out_) <- type
  if(plot.it){
    nano <- list(...)
    nano$x <- 1:length(out_)
    nano$y <- out_
    if(is.null(nano$xlab)) nano$xlab <- "Observation (i)"
    if(is.null(nano$type)) nano$type <- "h"
    if(is.null(nano$ylab)) nano$ylab <- ifelse(type=="local",expression(d[max]),expression(diag[i]))
    if(is.null(nano$labels)) labels <- 1:n
    else{
      labels <- nano$labels
      nano$labels <- NULL
    }
    do.call("plot",nano)
    if(any(out_>0)) abline(h=3*mean(out_[out_>0]),lty=3)
    if(any(out_<0)) abline(h=3*mean(out_[out_<0]),lty=3)
    if(!missingArg(identify)) identify(nano$x,nano$y,n=max(1,floor(abs(identify))),labels=labels)
  }
  if(!is.null(subst))
    message("The coefficients included in the measures of local influence are: ",paste(subst,sep=""),"\n")
  return(invisible(out_))
}
