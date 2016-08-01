library(xml2)
help(package=xml2)
result = read_xml("135-roger.xml")
#  http://www.w3schools.com/xsl/xpath_syntax.asp

all_articles = xml_children(xml_children(result))  ## 135
length(all_articles)

#### DOI #####
doi135 = lapply(all_articles, xml_find_all, xpath=".//electronic-resource-num")
doi135length = sapply(doi135, length)
length(doi135)
table(is.null(doi135))
doi135content = sapply(sapply(doi135, xml_contents), as.character)
#doi135content[sapply(doi135content == 0 ]
doi135content = sapply(doi135content, function(doi) return(ifelse(length(doi)==0, "", doi)))
articles_without_doi = which(doi135length == 0)
### 7 are missing. we have PMID for 3.
article.df[articles_without_doi, 1:6]

###PMID ####
pmid135 = sapply(all_articles, xml_find_all, xpath=".//accession-num")
sapply(pmid135, length)
pmid135content = sapply(sapply(pmid135, xml_contents), as.character)
pmid135content = sapply(pmid135content, function(pmid) return(ifelse(length(pmid)==0, "", pmid)))


table( sapply(doi135, length), sapply(pmid135, length))


#### TITLES ####
titles = lapply(all_articles, xml_find_all, xpath=".//title")
sapply(titles, length)
titlescontent = sapply(sapply(titles, xml_contents), as.character)

#### YEARS ####
years = lapply(all_articles, xml_find_all, xpath=".//year")
sapply(years, length)
yearscontent = sapply(sapply(years, xml_contents), as.character)
yearscontent = as.numeric(yearscontent)

#### FIRST AUTHOR ####
author1 = lapply(all_articles, xml_find_all, xpath=".//author")
sapply(author1, length)
author1content = sapply(sapply(author1, xml_contents), as.character)
author1 = sapply(author1content, '[', i = 1)

#### NUMBER OF AUTHORS ####
all_authors = lapply(all_articles, xml_find_all, xpath=".//author")
Nauthors = sapply(all_authors, length)

##### TAGS ####
tags135 = lapply(all_articles, xml_find_all, xpath=".//label")
sapply(tags135, length)
tags135content = sapply(sapply(tags135, xml_contents), as.character)
tags135content =  gsub(pattern = "t os-cohort", replacement = "tos-cohort", x = tags135content)
tags135content = strsplit(tags135content, split=";")
taglist = sort(unique(unlist(tags135content)))
length(taglist)
tags.by.articles = lapply(tags135content, is.element, el = taglist)
### simpler:   lapply(tags135content, is.element, el = taglist)
as.matrix(tags.by.articles)
as.matrix(tags.by.articles)[135, ]
tags.by.articles = apply(
  as.matrix(tags.by.articles), 1, unlist)
str(tags.by.articles)
tags.by.articles = t(tags.by.articles)
str(tags.by.articles)
tags.by.articles = as.data.frame(tags.by.articles)

names(tags.by.articles) = taglist
with(tags.by.articles, table(done, DONE))

#### corrections ####


#### article.df ####
article.df =  data.frame(pmid=pmid135content, doi=doi135content, title=titlescontent, 
                         author1, Nauthors= Nauthors, year=yearscontent, tags.by.articles)
with(article.df, table(pmid=="", doi=="", notrelevant))

#### notrelevant ####
table(article.df$notrelevant)
notrelevant.df = article.df[article.df$notrelevant == TRUE, ]
dim(notrelevant.df)
relevant.df = article.df[article.df$notrelevant == FALSE, ]
dim(relevant.df)

#### analysis of focus tags ####  

focusCols = grep("focus\\.", names(relevant.df), value=TRUE)
focusTables = sapply(focusCols, function(col) table(relevant.df[[col]]))
focusTable = t(focusTables)
dimnames(focusTable)[[1]] = 
  gsub(".*focus\\.", "", dimnames(focusTable)[[1]])
dimnames(focusTable)[[1]] = 
  gsub("\\.", " ", dimnames(focusTable)[[1]])
focusTable = focusTable[  ,  -1, drop=FALSE]
dimnames(focusTable)[[2]] = "#articles" 
focusTable  = cbind(focusTable,
 round(focusTable[ , 1]/99, digits=2) )
dimnames(focusTable)[[2]] [2] = "proportion" 
focusTable = focusTable[order(rownames(focusTable)),  ]
focusTable = focusTable[order(focusTable[ , 1], decreasing = TRUE),  ]
plot(focusTable[ , "#articles"],
     nrow(focusTable):1, ylab="", axes=F, xlab="#articles", pch=15,
     col="darkred")
axis(1)
sapply(1:nrow(focusTable), function(row)
  text(focusTable[ row, "#articles"], nrow(focusTable) - row + 1, 
       labels = rownames(focusTable)[row], 
       adj = ifelse(row <=3, 1.1, -0.1),
       col="darkgreen")
  )

with(relevant.df, 
     table(fpb.focus.patient.benefit.from.oncotype,
           tos.discussion)
)



# SUMMARIES ####
with(relevant.df, plot(year, Nauthors))
with(relevant.df, plot(jitter(year), Nauthors))
with(relevant.df, plot(as.factor(year), Nauthors))

#### Assemble DOI query ####


#########  PREVIOUS CODE #####

# first_front = xml_children(xml_children(result)[[1]])[[1]]  ### <front>, includes title, abstract, etc., gets the first result
# first_title = xml_find_all(first_front, xpath=".//article-title")  ### <front>, includes title, abstract, etc.
# first_title = gsub(pattern = "<[/]*article-title>", "", as.character(first_title)) ## puts title into normal format
# first_journal = xml_find_all(xml_children(xml_children(result)[[1]])[[1]], xpath=".//journal-title")  ### <front>, includes title, abstract, etc.
# first_journal = gsub(pattern = "<[/]*journal-title>", "", as.character(first_journal))
# ###  So we can get info from the XML.  A little tricky, but powerful.
# 
# fronts = xml_find_all(xml_children(result), xpath=".//front")  ### We DON"T want titles from the bibliographies!
# length(fronts)  ## 197  ## Good!
# all_titles = xml_find_all(fronts, xpath=".//article-title")  ### <front>, includes title, abstract, etc.
# length(all_titles)  ## 198, number of titles in total, should be 197
# all_titles[[199]]  ### doesn't exist. good.
# all_titles[[198]]  ### but why this one?
# 
# all_titles = lapply(fronts, function(front) xml_find_all(front, ".//article-title")) 
# length(all_titles)  ## 197   Seems to work better!
# 
# # for(a_title in all_titles) 
# #   print(paste(collapse=" ", as.character(xml_contents(a_title))))
# get_title = function(a_title) ## one title 
#   paste(collapse=" ", as.character(xml_contents(a_title))) ## put title into normal format
# all_title_strings = sapply(all_titles, get_title)
# length(all_title_strings)  ### 197
# head(all_title_strings) ## first 6 
# tail(all_title_strings) ## last 6 
# which(nchar(all_title_strings) < 10)  ## None.
# ### counting # of authors
# table(
#   sapply(all_articles, function(an_article)
#     length(xml_find_all(xml_find_all(an_article, xpath=".//front") , 
#                         xpath=".//article-meta/contrib-group/contrib"))
#   )
# )
# 
# first_author_nodes = sapply(all_articles, 
#                        function(an_article)
#                          xml_find_one(xml_find_all(an_article, xpath=".//front") , 
#                                                    xpath=".//article-meta/contrib-group/contrib")
# )
# first_author_surnames = sapply(first_author_nodes, 
#                        function(an_author)
#                          as.character(xml_contents(xml_find_all(an_author, 
#                                                    xpath=".//name/surname")))
# )
# whichNoFirstAuthor = which(sapply(first_author_surnames, length) == 0)  ### These two have no surnames
# first_author_surnames[whichNoFirstAuthor] = "(none)"
# 
# # ids = xml_find_one(fronts, './/article-meta/article-id')
# pmcidnodes = xml_find_all(fronts, './/article-meta/article-id[@pub-id-type="pmc"]') 
# length(pmcidnodes)  #197
# pmcids = sapply(sapply(sapply(pmcidnodes, xml_contents), as.character), as.numeric)
# pmcids = as.vector(pmcids)  ### strip out the names
# length(pmcids)  #197
# 
# pmids = xml_find_all(fronts, './/article-meta/article-id[@pub-id-type="pmid"]') 
# length(pmids) # only 193 
# pmidnodes = sapply(fronts, function(front) ## finds the pmids in the "front" 
#   xml_find_all(front, './/article-meta/article-id[@pub-id-type="pmid"]') ) 
# length(pmidnodes) # now 197. For example#197: xml_nodeset (1)} [1] <article-id pub-id-type="pmid">6324199</article-id>
# pmids = 
#   sapply(sapply(sapply(pmidnodes, xml_contents), as.character), as.numeric)
# which(sapply(pmids, length) == 0)  ### These four have no pmids
# pmids[sapply(pmids, length) == 0] = NA
# table(is.na(pmids))
# pmids = unlist(pmids)
# 
# dois = xml_find_all(fronts, './/article-meta/article-id[@pub-id-type="doi"]') 
# length(dois) # only 179
# doinodes = sapply(fronts, function(front)
#   xml_find_all(front, './/article-meta/article-id[@pub-id-type="doi"]') ) 
# length(doinodes) # now 197. For example#197: xml_nodeset (1)} [1] <article-id pub-id-type="doi">6324199</article-id>
# dois = 
#   sapply(sapply(doinodes, xml_contents), as.character)
# which(sapply(dois, length) == 0)  ### 18 have no dois
# dois[sapply(dois, length) == 0] = ""
# table(dois=="")
# dois = unlist(dois)
# 
# pmc197 = data.frame(pmc=pmcids, pmid=pmids, doi=dois,
#                          author1=unlist(first_author_surnames), 
#                          title=all_title_strings)
# write.csv(file = "pmc197.csv", x = pmc197)
# pmc_show = function(n)
#   system(paste0('open "http://www.ncbi.nlm.nih.gov/pmc/?term=', pmc197$doi[n], '"') )
# 
# ###### Subsample for more intensive study:
# Narticles = 8 ## sample of 8 articles 
# short_result = all_articles[sample(1:length(all_articles), Narticles)] 
# length(short_result)
# short_title_strings = sapply(xml_find_all(short_result, 
#                                           xpath=".//front/article-meta/title-group/article-title"), get_title)
# sapply(all_articles, function(an_article)
#   length(xml_find_all(xml_find_all(an_article, xpath=".//front") , 
#                       xpath=".//article-meta/title-group/article-title"))
# )
# 
# ## to find link 
# find_link = sapply (all_articles, 
#                    function(an_article)
#                      xml_find_one(xml_find_all(an_article, xpath=".//front") , 
#                                   first_front xpath=".//article-meta/http") 
# ) 
# ## from all of the articles, look at one, find the link -> which is in between <http>    
# 
# 
# ## search for words with EM 
# EM_words = c("Risk ratio", 
#              "Risk difference",
#              "Hazard ratio", 
#              "Sensitivity and specificity",
#              "Positive predictive value",
#              "negative predictive value",
#              "Expected utility",
#              "Relative utility", 
#              "Number needed to treat", 
#              "Area under the ROC curve",
#              "Net reclassification improvement", 
#              "Accuracy", 
#              "Odds ratio") ## create vector of words, see if they match the html 
#   

                   
              
  
