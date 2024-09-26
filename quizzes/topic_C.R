library(exams)

set.seed(2022)

Quiz8 <- list(
  c("regressioncoefficient.Rmd","regressionxy.Rmd","regressionlinearmodel.Rmd","regressionresiduals.Rmd"),
  c("detailspredictioninterval.Rmd","detailsresidualse.Rmd","detailssimulation.Rmd", "detailsRsquared.Rmd"),
  c("lmvisualise.Rmd"),c("regressionScatterplot.Rmd"),
  c("lmsignificance.Rmd","lmconfinterval.Rmd"),
  c("lmprediction.Rmd")
)

exams2moodle(Quiz8, edir = 'quizzes/topicC', name = "quizC1", n=40, mchoice = list(shuffle = TRUE))

Quiz9 <- list(
  c("assumption1.Rmd","assumption2.Rmd","residuals2.Rmd"),
  c("timeseries1.Rmd","timeseries2.Rmd","timeseries3.Rmd","timeseries4.Rmd"),
  c("diagnostics.Rmd"),
  c("outliers.Rmd","qqplot.Rmd"),
  c("timeseriesmodelling.Rmd")
)

exams2moodle(Quiz9, edir = 'quizzes/topicC', name = "quizC2", n=40, mchoice = list(shuffle = TRUE))

Quiz10 <- list(
  c("ftest.Rmd","adjustedR2.Rmd","response.Rmd","predictor.Rmd"),
  c("multiplelinearmodel.Rmd","multiplelinearmodel2.Rmd","interaction.Rmd","factor.Rmd"),
  c("modellingadjr2.Rmd"),
  c("modellingfactor.Rmd"),
  c("factornumeric.Rmd"))

exams2moodle(Quiz10, edir = 'quizzes/topicC', name = "quizC3", n=40)
