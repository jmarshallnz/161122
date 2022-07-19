library(exams)

set.seed(2021)

Quiz4 <- list( # done
  c("BasicProb1.Rmd", "BasicProb4.Rmd", "BasicProb6.Rmd"),
  c("BasicProb5.Rmd", "BasicProb8.Rmd"),
  c("BasicProb2.Rmd", "BasicProb7.Rmd", "BasicProb3.Rmd"),
  c("GenProb1.rnw", "GenProb2.rnw", "GenProb5.rnw", "GenProb4.rnw", "GenProb6.rnw", "GenProb7.rnw", "GenProb9.rnw"),
  c("Prob8.rnw", "Prob3.Rmd", "Prob9.Rmd")
)

exams2moodle(Quiz4, edir = 'quizzes/topicB', name = "quizB1", n=40, mchoice = list(shuffle = TRUE))

Quiz5 <- list( # done
  c("NormalProb5.Rmd", "NormalProb7.Rmd", "NormalEmpiricalRules1.Rmd", "NormalEmpiricalRules1.Rmd", "NormalEmpiricalRules1.Rmd", "NormalCalcQuartile.Rmd"),
  c("NormalProb1.Rmd", "NormalProb2.Rmd", "NormalProb3.Rmd", "NormalProb4.Rmd", "NormalProb6.Rmd", "NormalProb8.Rmd"),
  c("BinomialProb1.Rmd", "BinomialProb2.Rmd", "BinomialProb4.Rmd"),
  c("ExponentialMore.Rmd", "ExponentialLess.Rmd", "ExponentialBetween.Rmd", "ExponentialInverse.Rmd"),
  c("PoissonMore.Rmd", "PoissonLess.Rmd", "PoissonBetween.Rmd", "PoissonEqual.Rmd")
)

exams2moodle(Quiz5, edir = 'quizzes/topicB', name = "quizB2", n=40, mchoice = list(shuffle = TRUE))

Quiz6 <- list( # done
  c("NormalityPlot1.Rmd", "NormalityPlot2.Rmd"),
  c("StandardError1.Rmd", "SamplingDist1.Rmd", "SamplingDist2.Rmd", "SamplingDist3.Rmd"),
  c("AddXY1.Rmd", "AddXY2.Rmd"),
  c("CLT1.Rmd", "CLT3.Rmd", "CLT4.Rmd"),
  c("DistChoice1.Rmd", "DistChoice2.Rmd", "DistChoice3.Rmd", "DistChoice4.Rmd", "DistChoice5.Rmd")
)

exams2moodle(Quiz6, edir = 'quizzes/topicB', name = "quizB3", n=40, mchoice = list(shuffle = TRUE))

Quiz7 <- list(# done
  c("CCChoice1.Rmd",  "CCChoice2.Rmd",  "CCChoice3.Rmd",  "CCChoice4.Rmd",  "CCChoice5.Rmd"),
  "CCConstruct1.Rmd",
  "CCConstruct2.Rmd",
  "Reliability1.Rmd",
  c("Reliability3.Rmd",  "Reliability4.Rmd")
)

exams2moodle(Quiz7, edir = 'quizzes/topicB', name = "quizB4", n=40, mchoice = list(shuffle = TRUE))