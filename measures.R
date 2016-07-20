measureNames = c("[^[:alpha:]]ROC", ###  this works!
                 "AUROC", 
                 "PPV|positive predictive value", "NPV|negative predictive value", "sensitivity", "specificity",
                 "hazard ratio", "risk ratio", "risk difference",
                  "odds ratio", 
                 "NNT|number needed to treat", 
                 "accuracy", "expected utility")
measures_in_abstracts = 
  sapply(measureNames, grep, x=all_abstract_text, ignore.case=TRUE)
### article 4 has PPV and NPV in the abstract. But the outcome was receiving chemo!
fronts[[4]]
all_title_strings[4]
pmc197$pmc[4]
pmc197$doi[4]
#http://www.ncbi.nlm.nih.gov/pmc/?term=10.1007%2Fs10549-016-3833-9%5BDOI%5D
#http://www.ncbi.nlm.nih.gov/pmc/?term=10.1007/s10549-016-3833-9
pmc_show = function(n)
  system(paste0('open "http://www.ncbi.nlm.nih.gov/pmc/?term=', pmc197$doi[n], '"') )
