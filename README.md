## speedDating


I wrote this code to analyze a [rich data set](http://www.stat.columbia.edu/~gelman/arm/examples/speed.dating/) compiled by Columbia Business School professors Ray Fisman and Sheena Iyengar for their paper [Gender Differences in Mate Selection: Evidence From a Speed Dating Experiment](http://faculty.chicagobooth.edu/emir.kamenica/documents/genderDifferences.pdf). My core intent was to predict when two speed dating participants will express interest in exchanging contact with one another, assuming that one had data on them from previous similar events. 

I published  [Using machine learning to predict romantic compatibility: empirical results](http://lesswrong.com/lw/leh/using_machine_learning_to_predict_romantic/) on December 17th 2014, where I begin to describe what I discovered. Over the coming weeks, I'll finish presenting the results, and discuss my methodology.

Participants didn't attend multiple events, so rather than using information about previous events to predict what will happen at an event, we predict participants' decisions on a given date based on information about other dates at the same event. The other dates that we used usually included dates that in fact happened after the date for which we predicted decisions, and there are *a priori* reasons to question whether the results would generalize to situations where one was trying to predict the future exclusively based on knowledge of the past. It seems that if one takes suitable precautionary measures, there's very little cause for concern on this front, but implementing these measures requires more complicated code and more computational power than would be the case if we had enough data so that we could restrict to using information about past dates to predict what happens on a given date. 

The methodology is largely implicit in the code, which is coherently organized, up to some minor changes that I still need to make. I describe the contents of each folder below:

**dataProcessor**

* /dataCleaner.R — Drops a large fraction of the features that I ended up not using, fills in missing entries, and drops examples coming from very small events.

* binaries/basicBinaries.R — Converts numerically coded features such as race, career and field of study into collections of binary features. 

* /ratingMetrics/ratingAvgs.R — Creates average ratings for each participant on a date. This is considerably more involved than it would be if it wasn't necessary to exclude the two participants' ratings of each other. In fact, simply excluding these ratings introduces structure in the dataset that results in the features being contaminated with the participants' ratings of each other in a subtle way, and we introduce a stochastic element to eliminate the contamination. The code also computes correlations between the individuals' decisions on others and the others' average ratings of various types, with a view toward individualizing the model by using revealed preferences.  


* /ratingMetrics/collabFilt.R — Uses collaborative filtering to generate guesses for participants' ratings of each other, by excluding one date from an event at a time. For reasons that are unclear to me, the features generated turned out not to yield incremental predictive power, even when aggregated with each other or with other features.

* /merger.R —  The original dataset has a row for each participant on a given date rather than a single row for each date. I joined the dataset with itself so that the rows correspond to dates, with all information about both participants present in a single row.

**auxFunctions**

Contains the libraries and functions that I used repeatedly. 

**Machine learning**

In this context, random forest models are unusually nontransparent on account of their ability to infer the identities of the events' participants, and unless one includes the same participant in both the train and test sets, they perform very poorly. Because we construct features by using examples other than the one that we're trying to make a prediction about, splitting up an event between train and test sets gives rise to a situation where the train set is's contaminated with the variable that we're trying to predict, so random forest models end up being a poor choice. I instead used logistic regression. 

I initially used hundreds of randomly generated train/test splits for cross validation, and later realized that doing doesn't add clarity beyond that offered by the simpler method of doing cross validation at the level of individual events. 

**Visuals**

The folder contains some visualizations that are helpful in understanding the dataset, but is currently very incomplete.

**Scratch work**

This contains code that I used to select features. I still need to clean this up: it's incomplete and may not compile.

**Recommendation System**

I attempted to answer the question of how much the model's predictive power could improve speed dating events by creating schedules with a higher concentration of matches at the beginning. This is with a view toward ultimately giving a algorithm to schedule events which are more heavily loaded with likely matches. The problem of generating speed dating schedules to load them heavily with matches is quite a bit more subtle than it may initially appear, and achieving optimal performance is probably NP-complete. 

My initial attempt is in "/eventScheduler.R", but I think that the algorithm is suboptimal to a significant degree, even amongst polynomial time algorithms, and to the extent that it works, its effectiveness may derive from the fact that for a given event, the probabilities that the model generates reflect information about what happened on other dates at the event. The file "/topNLists.R" compiles some metrics that are less relevant, but also easier to interpret and on more solid ground.

I've halted this line of investigation for now, but hope to return to it later.