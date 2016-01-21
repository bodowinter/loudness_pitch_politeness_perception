## Bodo Winter
## Created January 20, 2016
## Analysis of Pitch/Intensity perception experiment

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load in data:

setwd('/Users/teeniematlock/Desktop/research/politeness/intensity_pitch/')
loud <- read.csv('intensity_perception.csv')
f0 <- read.csv('pitch_perception.csv')

## Load in packages:

library(dplyr)
library(lme4)

## Reduce data frame:

loud <- select(loud,
	Subject, Age, Sex, Trial, SpeakerNum, IntLevel, IntLevelNum, 
	RespK.ACC.SubTrial., SoundFile.SubTrial., StimNum, StimDB, SpeakerSex)
f0 <- select(f0,
	Subject, Age, Sex, Trial, SpeakerNum, F0Level, F0LevelNum, 
	RespK.ACC.SubTrial., SoundFile.SubTrial., StimNum, StimF0, SpeakerSex)

## Rename columns:

loud <- rename(loud,
	Listener = Subject,
	ListenerSex = Sex, Speaker = SpeakerNum,
	Resp = RespK.ACC.SubTrial., File = SoundFile.SubTrial.,
	Item = StimNum, IntNum = IntLevelNum)
f0 <- rename(f0,
	Listener = Subject,
	ListenerSex = Sex, Speaker = SpeakerNum,
	Resp = RespK.ACC.SubTrial., File = SoundFile.SubTrial.,
	Item = StimNum, F0Num = F0LevelNum)

## Center predictors, transform to factors, calculate quadratic predictor:

loud <- mutate(loud,
	IntNum_c = IntNum - mean(IntNum),
	Speaker = as.factor(Speaker),
	Listener = as.factor(Listener),
	Item = as.factor(Item),	
	IntNum_c2 = IntNum_c ^ 2)
f0 <- mutate(f0,
	F0Num_c = F0Num - mean(F0Num),
	Speaker = as.factor(Speaker),
	Listener = as.factor(Listener),
	Item = as.factor(Item),
	F0Num_c2 = F0Num_c ^ 2)

## Create contrast coded predictors (deviation coding):

loud$SpeakerSex01 <- as.numeric(loud$SpeakerSex) - 1.5
loud$ListenerSex01 <- as.numeric(loud$ListenerSex) - 1.5

f0$SpeakerSex01 <- as.numeric(f0$SpeakerSex) - 1.5
f0$ListenerSex01 <- as.numeric(f0$ListenerSex) - 1.5


##------------------------------------------------------------------
## Linear mixed-effects model analysis of intensity data:
##------------------------------------------------------------------

## Loudness full model with quadratic predictors:

summary(loud.mdl <- glmer(Resp ~ IntNum_c + IntNum_c2 + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker) +
	(0+IntNum_c2|Listener) + (0+IntNum_c2|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Loudness model with only linear pitch predictor:

summary(loud.linr <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker) +
	(0+IntNum_c2|Listener) + (0+IntNum_c2|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
	
## Test for quadratic effect:

anova(loud.linr, loud.mdl, test = 'Chisq')

## Construct full model with linear effect and no quadratic random effects:
## Add loudness/gender interactions:

summary(loud.lin <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	IntNum_c:ListenerSex01 + IntNum_c:SpeakerSex01 +
	IntNum_c:ListenerSex01:SpeakerSex01 +
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Get rid of three-way interaction:

summary(loud.nothreeway <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	IntNum_c:ListenerSex01 + IntNum_c:SpeakerSex01 +
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model without intensity/listener-sex interaction:

summary(loud.nolistenerint <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	1 + IntNum_c:SpeakerSex01 +
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model without intensity/speaker-sex interaction:

summary(loud.nospeakerint <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	IntNum_c:ListenerSex01 + 1 +
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test intensity/sex interactions:

anova(loud.nospeakerint, loud.nothreeway, test = 'Chisq')
anova(loud.nolistenerint, loud.nothreeway, test = 'Chisq')
anova(loud.nothreeway, loud.lin, test = 'Chisq')

## Model without intensity interactions:

summary(loud.nogenderint <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model for Intensity effect:

loud.noIntensity <- glmer(Resp ~ 1 + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Model for listenersex/speakersex interaction:

loud.noint <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Model for listenersex main effect:

loud.nolistener <- glmer(Resp ~ IntNum_c + 
	1 + SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Model for speakersex main effect:

loud.nospeaker <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + 1 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener) + (0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Likelihood ratio tests:

anova(loud.noIntensity, loud.nogenderint, test = 'Chisq')
anova(loud.noint, loud.nogenderint, test = 'Chisq')			# speaker/listener interaction
anova(loud.nospeaker, loud.noint, test = 'Chisq')
anova(loud.nolistener, loud.noint, test = 'Chisq')

## Significance of random effects, no speaker slopes:

summary(loud.nospeakerslope <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Listener),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Significance of random effects, no listener slopes:

summary(loud.nolistenerslope <- glmer(Resp ~ IntNum_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+IntNum_c|Speaker),
	data = loud, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test this:

anova(loud.nolistenerslope, loud.nogenderint)
anova(loud.nospeakerslope, loud.nogenderint)



##------------------------------------------------------------------
## Linear mixed-effects model analysis of pitch data:
##------------------------------------------------------------------

## Pitch full model with quadratic predictors:

summary(f0.mdl <- glmer(Resp ~ F0Num_c + F0Num_c2 + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker) +
	(0+F0Num_c2|Listener) + (0+F0Num_c2|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Pitch model with only linear pitch predictor:

summary(f0.linr <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker) +
	(0+F0Num_c2|Listener) + (0+F0Num_c2|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test for quadratic effect:

anova(f0.linr, f0.mdl, test = 'Chisq')	# n.s., drop quadratic effects

## New pitch model without quadratic random effects:
## Also, add f0 interaction effects for a potential gender interaction of the manipulation:

summary(f0.lin <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 + 
	ListenerSex01:F0Num_c + SpeakerSex01:F0Num_c +
	ListenerSex01:SpeakerSex01:F0Num_c +
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model without the three-way interaction:

summary(f0.lin.pitchnothreeway <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + SpeakerSex01 + 
	ListenerSex01:SpeakerSex01 + 
	ListenerSex01:F0Num_c + SpeakerSex01:F0Num_c + 	
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model without speaker-sex/pitch interaction:

summary(f0.lin.nospeakersexint <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + SpeakerSex01 + 
	ListenerSex01:SpeakerSex01 + 
	ListenerSex01:F0Num_c + 1 + 	
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model without listener-sex/pitch interaction:

summary(f0.lin.nolistenersexint <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 + 
	1 + SpeakerSex01:F0Num_c + 	
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test for pitch condition / gender interactions:

anova(f0.lin.nospeakersexint, f0.lin.pitchnothreeway, test = 'Chisq')
anova(f0.lin.nolistenersexint, f0.lin.pitchnothreeway, test = 'Chisq')
anova(f0.lin.pitchnothreeway, f0.lin, test = 'Chisq')	# p = 0.079

## Model without any condition / gender interactions:

f0.nogenderint <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Model for pitch effect:

f0.nopitch <- glmer(Resp ~ 1 + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Model for listenersex/speakersex interaction:

f0.noint <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Model for listenersex main effect:

f0.nolistener <- glmer(Resp ~ F0Num_c + 
	1 + SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Model for speakersex main effect:

f0.nospeaker <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + 1 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener) + (0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Likelihood ratio tests:

anova(f0.nopitch, f0.nogenderint, test = 'Chisq')
anova(f0.noint, f0.nogenderint, test = 'Chisq')			# speaker/listener interaction
anova(f0.nospeaker, f0.noint, test = 'Chisq')		# speaker gender effect
anova(f0.nolistener, f0.noint, test = 'Chisq')


## Significance of random effects, no speaker slopes:

summary(f0.nospeakerslope <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Listener),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Significance of random effects, no listener slopes:

summary(f0.nolistenerslope <- glmer(Resp ~ F0Num_c + 
	ListenerSex01 + SpeakerSex01 + ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+F0Num_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test this:

anova(f0.nolistenerslope, f0.nogenderint)
anova(f0.nospeakerslope, f0.nogenderint)


slopes <- coef(f0.nogenderint)$Listener$F0Num_c
subjs <- data.frame(slopes, Age = f0[match(1:24, f0$Listener),]$Age)
subjs$PitchDirection <- ifelse(slopes < 0, -1, 1)
aggregate(Age ~ PitchDirection, subjs, mean)




##------------------------------------------------------------------
## Plot predictions of intensity model:
##------------------------------------------------------------------

## Create dataframe for getting predictions of intensity effect:

intens.pred <- data.frame(IntNum_c = c(-2, -1, 0, 1, 2),
	ListenerSex01 = 0, SpeakerSex01 = 0, Resp = 0)

## Extract model matrix for standard errors:

mm <- model.matrix(terms(loud.lin), intens.pred)

## Extract fitted values:

intens.pred$Resp <- predict(loud.lin,
	newdata = intens.pred, re.form = NA)

## Extract variance component:

pvar1 <- diag(mm %*% tcrossprod(vcov(loud.lin), mm))

## Add lower and upper bound confidence intervals to pred.data:

intens.pred$UB <- intens.pred$Resp + 1.96 * sqrt(pvar1)
intens.pred$LB <- intens.pred$Resp - 1.96 * sqrt(pvar1)

## Back-transform everything to probabilities for plotting:

intens.pred <- mutate(intens.pred,
	Resp = plogis(Resp), UB = plogis(UB), LB = plogis(LB))

## Create dataframe for getting predictions of gender interaction effect:

gender.pred <- data.frame(IntNum_c = 0,
	ListenerSex01 = c(-0.5, -0.5, 0.5, 0.5),
	SpeakerSex01 = c(-0.5, 0.5, -0.5, 0.5),
	Resp = 0)

## Same spiel, extract predictions:

mm <- model.matrix(terms(loud.lin), gender.pred)
gender.pred$Resp <- predict(loud.lin,
	newdata = gender.pred, re.form = NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(loud.lin), mm))
gender.pred$UB <- gender.pred$Resp + 1.96 * sqrt(pvar1)
gender.pred$LB <- gender.pred$Resp - 1.96 * sqrt(pvar1)
gender.pred <- mutate(gender.pred,
	Resp = plogis(Resp), UB = plogis(UB), LB = plogis(LB))

## Plot this:

xfactor <- 0.1	# for second plot
quartz('', 11.5, 5.5)
par(mfrow = c(1, 2), mai = c(0, 0, 0, 0.25), omi = c(1.25, 1.75, 1, 0.5))
# PLot 1:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0.5, 5.5), ylim = c(0.2, 0.8))
axis(side = 2, at = seq(0.2, 0.8, 0.2),
	labels = paste0(seq(20, 80, 20), '%'),
	las = 2, font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
axis(side = 1, at = 1:5,
	labels = c('-10%', '-7.5%', '-5%', '-2.5%', '0%'),
	font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Intensity level', side = 1, line = 3, cex = 1.5, font = 2)
mtext(text = '% Polite', side = 2, font = 2, line = 4.65, cex = 1.75)
mtext(text = 'Intensity effect', side = 3, font = 2, line = 1.25, cex = 2)
text(x = 0.6125, y = 0.77, labels = '(a)', font = 2, cex = 1.5)
# Actual data:
points(1:5, intens.pred$Resp, type = 'b', pch = 19, lwd = 2)
arrows(x0 = 1:5, x1 = 1:5,
	y0 = intens.pred$LB, y1 = intens.pred$UB, lwd = 2,
	code = 3, angle = 90, length = 0.1)
box(lwd = 2)
# Plot 2:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0, 4), ylim = c(0.2, 0.8))
mtext(text = 'Gender interaction', side = 3, font = 2, line = 1.25, cex = 1.85)
axis(side = 1, at = c(1, 3),
	font = 2, lwd.ticks = 2, labels = c('', ''))
axis(side = 1, at = c(1, 3),
	labels = c('female\nlisteners', 'male\nlisteners'),
	font = 2, line = 1.5, tick = F, cex.axis = 1.5)
legend('topright', pch = 16:17, lty = 1:2, lwd = 2,
	legend = c('female speakers', 'male speakers'))
text(x = 0.1, y = 0.77, labels = '(b)', font = 2, cex = 1.5)
# Actual data:
points(x = c(1, 3) - xfactor, y = gender.pred[c(1, 3),]$Resp, type = 'b',
	pch = 16, cex = 1.5,
	lwd = 2)
points(x = c(1, 3) + xfactor, y = gender.pred[c(2, 4),]$Resp, type = 'b',
	pch = 17, cex = 1.5,
	lwd = 2, lty = 2)
arrows(x0 = c(1, 3) - xfactor,
	x1 = c(1,3 ) - xfactor,
	y0 = gender.pred[c(1, 3),]$LB,
	y1 = gender.pred[c(1, 3),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
arrows(x0 = c(1, 3) + xfactor,
	x1 = c(1,3 ) + xfactor,
	y0 = gender.pred[c(2, 4),]$LB,
	y1 = gender.pred[c(2, 4),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
box(lwd = 2)



##------------------------------------------------------------------
## Plot predictions of pitch model:
##------------------------------------------------------------------

## Extract pitch effect:

pitch.pred <- data.frame(F0Num_c = c(-2, -1, 0, 1, 2),
	ListenerSex01 = 0, SpeakerSex01 = 0, Resp = 0)
mm <- model.matrix(terms(f0.lin.pitchnothreeway), pitch.pred)
pitch.pred$Resp <- predict(f0.lin.pitchnothreeway,
	newdata = pitch.pred, re.form = NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(f0.lin.pitchnothreeway), mm))
pitch.pred$UB <- pitch.pred$Resp + 1.96 * sqrt(pvar1)
pitch.pred$LB <- pitch.pred$Resp - 1.96 * sqrt(pvar1)
pitch.pred <- mutate(pitch.pred,
	Resp = plogis(Resp), UB = plogis(UB), LB = plogis(LB))

## Create dataframe for getting predictions of gender interaction effect:

gender.f0.pred <- data.frame(F0Num_c = 0,
	ListenerSex01 = c(-0.5, -0.5, 0.5, 0.5),
	SpeakerSex01 = c(-0.5, 0.5, -0.5, 0.5),
	Resp = 0)

## Same spiel, extract predictions:

mm <- model.matrix(terms(f0.lin.pitchnothreeway), gender.f0.pred)
gender.f0.pred$Resp <- predict(f0.lin.pitchnothreeway,
	newdata = gender.pred, re.form = NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(f0.lin.pitchnothreeway), mm))
gender.f0.pred$UB <- gender.f0.pred$Resp + 1.96 * sqrt(pvar1)
gender.f0.pred$LB <- gender.f0.pred$Resp - 1.96 * sqrt(pvar1)
gender.f0.pred <- mutate(gender.f0.pred,
	Resp = plogis(Resp), UB = plogis(UB), LB = plogis(LB))

## Plot this:

xfactor <- 0.1	# for second plot
quartz('', 11.5, 5.5)
par(mfrow = c(1, 2), mai = c(0, 0, 0, 0.25), omi = c(1.25, 1.75, 1, 0.5))
# PLot 1:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0.5, 5.5), ylim = c(0.2, 0.8))
axis(side = 2, at = seq(0.2, 0.8, 0.2),
	labels = paste0(seq(20, 80, 20), '%'),
	las = 2, font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
axis(side = 1, at = 1:5,
	labels = c('-16%', '-8%', '0%', '+8%', '+16%'),
	font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Pitch level', side = 1, line = 3, cex = 1.5, font = 2)
mtext(text = '% Polite', side = 2, font = 2, line = 4.65, cex = 1.75)
mtext(text = 'Pitch effect', side = 3, font = 2, line = 1.25, cex = 2)
text(x = 0.6125, y = 0.77, labels = '(a)', font = 2, cex = 1.5)
# Actual data:
points(1:5, pitch.pred$Resp, type = 'b', pch = 19, lwd = 2)
arrows(x0 = 1:5, x1 = 1:5,
	y0 = pitch.pred$LB, y1 = pitch.pred$UB, lwd = 2,
	code = 3, angle = 90, length = 0.1)
box(lwd = 2)
# Plot 2:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0, 4), ylim = c(0.2, 0.8))
mtext(text = 'Gender interaction', side = 3, font = 2, line = 1.25, cex = 1.85)
axis(side = 1, at = c(1, 3),
	font = 2, lwd.ticks = 2, labels = c('', ''))
axis(side = 1, at = c(1, 3),
	labels = c('female\nlisteners', 'male\nlisteners'),
	font = 2, line = 1.5, tick = F, cex.axis = 1.5)
legend('topright', pch = 16:17, lty = 1:2, lwd = 2,
	legend = c('female speakers', 'male speakers'))
text(x = 0.1, y = 0.77, labels = '(b)', font = 2, cex = 1.5)
# Actual data:
points(x = c(1, 3) - xfactor, y = gender.f0.pred[c(1, 3),]$Resp, type = 'b',
	pch = 16, cex = 1.5,
	lwd = 2)
points(x = c(1, 3) + xfactor, y = gender.f0.pred[c(2, 4),]$Resp, type = 'b',
	pch = 17, cex = 1.5,
	lwd = 2, lty = 2)
arrows(x0 = c(1, 3) - xfactor,
	x1 = c(1,3 ) - xfactor,
	y0 = gender.f0.pred[c(1, 3),]$LB,
	y1 = gender.f0.pred[c(1, 3),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
arrows(x0 = c(1, 3) + xfactor,
	x1 = c(1,3 ) + xfactor,
	y0 = gender.f0.pred[c(2, 4),]$LB,
	y1 = gender.f0.pred[c(2, 4),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
box(lwd = 2)


##------------------------------------------------------------------
## Raw pitch analysis:
##------------------------------------------------------------------

## Center pitch predictor:

f0 <- mutate(f0,
	StimF0_c = StimF0 - mean(StimF0),
	StimF0_c2 = StimF0_c ^ 2)

## Make model with pitch predictor:

summary(f0.raw <- glmer(Resp ~ StimF0_c + StimF0_c2 + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 +
	StimF0_c:SpeakerSex01 + StimF0_c:SpeakerSex01:ListenerSex01 + 
	StimF0_c2:SpeakerSex01 + StimF0_c2:SpeakerSex01:ListenerSex01 +
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+StimF0_c|Listener) + (0+StimF0_c|Speaker) +
	(0+StimF0_c2|Listener) + (0+StimF0_c2|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))	# convergence issues

## Without quadratic:

summary(f0.raw.linr <- glmer(Resp ~ StimF0_c + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 +
	StimF0_c:SpeakerSex01 + StimF0_c:ListenerSex01 + 
	StimF0_c:SpeakerSex01:ListenerSex01 + 	
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+StimF0_c|Listener) + (0+StimF0_c|Speaker) +
	(0+StimF0_c2|Listener) + (0+StimF0_c2|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))	# convergence issues

## Compare the two:

anova(f0.raw.lin, f0.raw, test = 'Chisq')

## Model without the quadratic random effects:

summary(f0.raw.lin <- glmer(Resp ~ StimF0_c + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 + StimF0_c:SpeakerSex01 + 
	StimF0_c:SpeakerSex01:ListenerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+StimF0_c|Listener) + (0+StimF0_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))	# convergence issues

## Without three-way interaction:

summary(f0.raw.nothree <- glmer(Resp ~ StimF0_c + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 + StimF0_c:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+StimF0_c|Listener) + (0+StimF0_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))	# convergence issues

## Without two-way pitch/speaker sex interaction:

summary(f0.raw.nospeakerint <- glmer(Resp ~ StimF0_c + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 + 1 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+StimF0_c|Listener) + (0+StimF0_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))	# convergence issues

## Without two-way pitch/listener sex interaction:

summary(f0.raw.nolistenerint <- glmer(Resp ~ StimF0_c + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 + 
	1 + StimF0_c:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+StimF0_c|Listener) + (0+StimF0_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))	# convergence issues

## Without any pitch interaction:

summary(f0.raw.nopitchints <- glmer(Resp ~ StimF0_c + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+StimF0_c|Listener) + (0+StimF0_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))	# convergence issues

## Without pitch at all:

summary(f0.raw.nopitch <- glmer(Resp ~ 1 + 
	ListenerSex01 + SpeakerSex01 +
	ListenerSex01:SpeakerSex01 +
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0+StimF0_c|Listener) + (0+StimF0_c|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))	# convergence issues

## Test effects:

anova(f0.raw.nopitch, f0.raw.nopitchints, test = 'Chisq')
anova(f0.raw.nolistenerint, f0.raw.nothree, test = 'Chisq')
anova(f0.raw.nospeakerint, f0.raw.nothree, test = 'Chisq')
anova(f0.raw.nothree, f0.raw.lin, test = 'Chisq')		# p = 0.0046


