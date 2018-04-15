library (mlr ); library ( mlbench ); data ( Sonar )
task = makeClassifTask ( data =Sonar , target ="Class")
lrn = makeLearner ("classif.ksvm")
rdesc = makeResampleDesc ( method ="CV", iters =5)
ps = makeParamSet (
 makeDiscreteParam ("kernel", values =c("polydot", "rbfdot")),
  makeNumericParam ("C", lower =-15, upper =15 , trafo = function (x) 2^x),
  makeNumericParam ("sigma", lower =-15, upper =15 , trafo = function (x) 2^x,
                    requires = quote ( kernel == "rbfdot")),
  makeIntegerParam ("degree", lower = 1, upper = 5,
                       requires = quote ( kernel == "polydot")))
ctrl = makeTuneControlRandom ( maxit =50)
res = tuneParams (lrn , task , rdesc , par.set =ps , control =ctrl , measures = mmce )