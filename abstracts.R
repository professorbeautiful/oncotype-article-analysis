# abstracts = xml_find_all(fronts, './/article-meta/abstract') 
# length(abstracts)  ### 210
# xml_length(abstracts)  ### Error: expecting an external pointer
# 
# abstracts[[1]]
# xml_length(abstracts[[1]])  ## 4
# xml_length(abstracts[[1]], only_elements = FALSE)  ## 9
# length(abstracts[[1]])  ## 2
# xml_parents(abstracts[[1]])
# xml_parents(abstracts[[1]])[[1]]
# xml_parents(abstracts[[1]])[[2]]
# ### For each abstract, produce the number of its article among the 197.
# metas_by_ab = lapply(abstracts, function(ab) {
#   par = xml_parents(ab)
#   meta = par[[1]]
#   meta
# })
# is_a_second_abstract = 
#   sapply(seq_along(metas_by_ab)[-1], 
#          function(n) identical(metas_by_ab[[n]], metas_by_ab[[n-1]]) ) 
# sum(is_a_second_abstract) ## 17 are true.  but 210 - 197 = 13, not 17.
# which_have_twin_abstracts = which(is_a_second_abstract)
# pmc197[35, ]
# pmc197[which_have_twin_abstracts, "pmc"]  ## one NA
# pmc197$pmid[which_have_twin_abstracts]  ## same NA
# length(unique(pmc197$pmid)) #194
# length(pmc197$pmid[!is.na(pmc197$pmid)]) #193
# 
# length(fronts) #197 ok.
# xml_find_all(fronts[[35]], './/article-meta/abstract') 
# #  the first of two is <abstract abstract-type="precis">
# 
# xml_find_all(fronts[which_have_twin_abstracts], './/article-meta/abstract') 
# sapply(which_have_twin_abstracts, function(n) {
#   cat(n, "  ==\n")
#   print(xml_find_all(fronts[[n]], './/article-meta/abstract'))
# } )
#
#### OK but none of that matters.
#### Here we pull out the abstracts:
get_abs_text = function(front) {
  abs = xml_find_all(front, './/article-meta/abstract')
  abs_text = paste(as.character((xml_contents(abs))), collapse="\n===\n")
  abs_text
}
all_abstract_text = sapply(fronts, function(front) 
  get_abs_text(front)
)
length(all_abstract_text)  ## 197
table(head(sort(nchar(all_abstract_text)), 80))  # 4 have length 0
pmc197$abstract = all_abstract_text
grep("hazard ratio", pmc197$abstract, ignore.case = T)  ### 8 
all_abstract_text[28]  ### Yup, there it is.

