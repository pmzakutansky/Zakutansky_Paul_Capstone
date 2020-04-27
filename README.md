## Task 1

Provide a brief background and significance about a specific research problem that interests you. It could be project you’re involved with now, or a rotation project, or something you’d like to work on. The reader will need to understand enough background to make sense of the experiment you propose below. Keep it brief. In one short paragraph.

**Proper development of oligodendroglia (OL) cells is essential for the myelination of the central nervous system (CNS). The most common CNS demyelination disease is Multiple Sclerosis which is due to an immune attack on myelinating OLs. One lncRNA involved in OL and myelin development is _nuclear enriched abundant transcript 1 (NEAT1)_. The human _NEAT1_ gene gives rise to two isoforms, _NEAT1_ Long and Short. A critical myelination factor is the selective RNA binding protein Quaking (QKI). Deficiency of QKI causes a loss of myelin in a mouse model. Recent discoveries have shown QKI interacts with long non-coding RNA (lncRNA). In addition, _NEAT1_ has been shown to be upregulated in Multiple Sclerosis patients, which raises the intriguing possibility that _NEAT1_ may play an important role in OL differentiation and myelination. My preliminary data revealed the human lncRNA NEAT1 contains a consensus quaking response element known to mediate interactions with QKI. I found that QKI regulates _NEAT1_ isoform expression in a human OL cell line, possibly through modulating _NEAT1_ poly-adenylation site usage. Moreover, the developmental regulation of QKI during OL differentiation may be an underlying factor for the preferential upregulation of the _NEAT1_ Long isoform to drive OL differentiation. I am interested in QKI’s regulation of _NEAT1_ biogenesis through usage of the polyadenylation signal sequence to facilitate cleavage and polyadenylation via an upstream quaking response element (QRE).  I will express dual firefly-renilla luciferase vector, under control of a CMV promoter with approximately a 500 base pair region of _NEAT1_ surrounding the polyadenylation signal sequence and polyadenylation site. I will perform site directed mutagenesis to replace four essential nucleotides in the QRE consensus binding sequence (mutant _NEAT1_-QRE). I will analyze the expression of firefly and renilla luciferase levels as well as the expression of the reporter RNA. I predict the mutation of the _NEAT1_ QRE prevents the cleavage of _NEAT1_ at the polyadenylation site compared to the WT reporter. This result would support my hypothesis that the QRE upstream of the polyadenylation signal sequence is necessary for the biogenesis of _NEAT1_ isoforms.**

## Task 2

Briefly state something that is unknown about this system that can be discovered through, and leads to, an experiment.  For example, "It is not known whether....."

**It is not known whether biogenesis of _NEAT1_ isoforms is dependent on the Quaking Response Element (QRE) upstream of the _NEAT1_ polyadenylation signal sequence and whether the binding event facilitates the cleavage and polyadenylation of _NEAT1_.**

## Task 3

Make an “if” “then” prediction that is related to item #2. It should be of the general form, “if X is true, then Y should happen”.

**If QKI binds to the QRE upstream of the NEAT1 polyadenylation signal sequence, then the WT _NEAT1_-QRE luciferase reporter should show an increase in the expression of firefly luciferase and a lack of Renilla luciferase expression compared to the mutant _NEAT1_-QRE.**

## Task 4

What dependent variable will be observed to test this prediction in item #3? What predictor variable will be used to manipulate the system experimentally? Define the inherent properties of these variables (eg, are they sorted, ordered or measured).

**The dependent variable will be the luciferase activity, measured as the ratio of Firefly luciferase to Renilla luciferase. The dependent variable is therefore a measured variable. The predictor variable is the form of QKI (wild-type or mutant) sequence used in the QRE construct. The predictor variable therefore is sorted.**

## Task 5

Write a statistical hypothesis.  There should be a null and alternate. These should be explicitly consistent with the prediction in item #3 and the response variable in #4. In other words, make sure the statistical hypotheses that you write here serves as a test of the prediction made in item #3.

**Ho: There is no different in the luciferase activity of the WT _NEAT1_-QRE, mutant _NEAT1_-QRE and control QRE luciferase reporter constructs.**
**Ha: There is an increase in the luciferase activity of the WT _NEAT1_-QRE reporter construct compared to mutant _NEAT1_-QRE reporter construct and control QRE constructs.** 

## Task 6

What is the statistical test you would use to test the hypothesis in item #5? Briefly defend what makes this appropriate for the hypothesis and the experimental variables. If there are alternatives, why is this approach chosen instead? Points will not be awarded if the justification involves something like "because everybody does it this way".

**To test this hypothesis, I will run a one-way ANOVA. The reason I would run a one-way ANOVA is because I am only testing the effect the quaking response element (QRE) has on the cleavage of NEAT1. The dependent variable, the ratio of luciferase activity (Firefly/_Renilla_ luciferase), is continuous and measured, which is needed to conduct an ANOVA. In addition, I am utilizing a wild-type, mutant, and control QRE luciferase construct, and therefore has three predictor variables. An ANOVA is best utilized for three or more groups to reduce the variability of a type 1 error.** 

## Task 7

List the procedures and decision rules you have for executing and interpreting the experiment. These procedures range from selection of experimental units, to randomization to primary endpoint to threshold decisions. Define (and defend) what you believe will be the independent replicate.

**To conduct this experiment, I will transfect human oligodendroglia (HOG) cells with a dual luciferase reporter construct containing Firefly and _Renilla_ luciferase. Twenty-four hours post transfection, cells are harvested and the luminescence of the Firefly luciferase and _Renilla_ luciferase are measured. The readings obtained by the luminometer were converted values of the ratio of Firefly to _Renilla_ luciferase. This protocol is conducted on five separate passages of HOG cells, each passage being one week apart. The collection of each of the five weeks of cells will serve as five biological replicates.**

## Task 8

Produce a graph of a simulation for the expected results. Create a dataMaker-like function in R to create and plot the data. Label and scale any axis. The graph should illustrate the magnitude of the expected response, or the level of response that you expect to see and would be minimally scientifically relevant. Be sure to illustrate any variation that is expected.

```{r}
library(ez)
library(tidyverse)
library(readxl)
```
```{r}
b=100 #expected basal QRE construct reporter
a=1.87 #expected fold-to-basal effect of WT NEAT1 QRE construct reporter
f=1.05 #expected fold-to-basal effect of Mutant NEAT1 construct reporter
sd=25 #expected standard deviation
n=5 #number of replicates for each group. 
reps = 100 #number of Monte Carlo simulations to run

dataMaker <- function(n, b, a, f, sd) {
  Control <-rnorm (n, b, sd)
  WT_QRE <- rnorm(n, (b*a), sd)
  Mutant_QRE <- rnorm(n, (b*f), sd)
  Response<- c(Control, WT_QRE, Mutant_QRE)
  Predictor <- c(rep(c("Control", "WT_QRE", "Mutant_QRE"), each=n))
  ID<- as.factor(c(1:length(Predictor)))
  NEAT1df1<- data.frame(ID, Predictor, Response)}
NEAT1df2<-dataMaker(n, b, a, f, sd)**

NEAT1df3 <- NEAT1df2 %>% group_by(Predictor) %>% summarise(mean_resp=mean(Response), sd_resp=sd(Response))
NEAT1df3

NEAT1_plot<- ggplot(NEAT1df2, aes(Predictor, Response))+ geom_jitter(width=0.1, size=4) +stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), geom="crossbar", width=0.2, color="blue") +labs(x="QRE Reporter Construct", y="Relative Luciferase Activity Level \n (Firefly Luciferase/Renilla Luciferase)") +ggtitle("Relative Luciferase Levels of NEAT1 QRE Constructs") +theme_classic()+ theme(plot.title=element_text(hjust=0.5))
NEAT1_plot
```

## Task 9

Write and perform a Monte Carlo analysis to calculate a sample size necessary to test the hypothesis. This Monte Carlo must test the primary endpoint.

```{r}
sims=100
b1=100#expected basal QRE construct reporter
a1=1.87 #expected fold-to-basal effect of WT NEAT1 QRE construct reporter
f1=1.05 #expected fold-to-basal effect of Mutant NEAT1 construct reporter
sd1=25 #expected standard deviation
n1=5 #number of replicates for each group. After testing less than 5 replicates, the minimum number required is 5 replicates.
```
```{r}
dataMaker2 <- function(n1, b1, a1, f1, sd1) {
  Control <-rnorm (n1, b1, sd1)
  WT_QRE <- rnorm(n1, (b1*a1), sd1)
  Mutant_QRE <- rnorm(n1, (b1*f1), sd1)
  Response<- c(Control, WT_QRE, Mutant_QRE)
  Predictor <- c(rep(c("Control", "WT_QRE", "Mutant_QRE"), each=n))
  ID<- as.factor(c(1:length(Predictor)))
  NEAT1df3<- data.frame(ID, Predictor, Response)}
pval <- replicate(sims, {
  sample.df <- dataMaker2(n1, sd1, b1, a1, f1)
  one_wayAnova <- ezANOVA(data=sample.df, dv=Response, wid=ID, between=Predictor, type=2, detailed=F)
  pval <- one_wayAnova$ANOVA[1,5]})
pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power")
plot<- ggplot(data.frame(pval)) + geom_histogram(aes(pval), color="blue", bins=30) + labs(x="p-value", y="Count") +theme_classic()+ggtitle("p-value distribution of NEAT1 QRE ANOVA Monte Carlo") + theme(plot.title=element_text(hjust=0.5))
plot
```
