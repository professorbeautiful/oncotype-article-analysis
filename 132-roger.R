library(xml2)
help(package=xml2)
result = read_xml("132-roger.xml")
#  http://www.w3schools.com/xsl/xpath_syntax.asp

all_articles = xml_children(xml_children(result))  ## 132
length(all_articles)
doi132 = lapply(all_articles, xml_find_all, xpath=".//electronic-resource-num")
doi132length = sapply(doi132, length)
length(doi132)
table(is.null(doi132))
doi132content = sapply(sapply(doi132, xml_contents), as.character)
#doi132content[sapply(doi132content == 0 ]
doi132content = sapply(doi132content, function(doi) return(ifelse(length(doi)==0, "", doi)))

pmid132 = sapply(all_articles, xml_find_all, xpath=".//accession-num")
sapply(pmid132, length)
pmid132content = sapply(sapply(pmid132, xml_contents), as.character)
pmid132content = sapply(pmid132content, function(pmid) return(ifelse(length(pmid)==0, "", pmid)))


table( sapply(doi132, length), sapply(pmid132, length))

articles_without_doi = all_articles[doi132length == 0]

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
tags132 = lapply(all_articles, xml_find_all, xpath=".//label")
sapply(tags132, length)
tags132content = sapply(sapply(tags132, xml_contents), as.character)
tags132content =  gsub(pattern = "t os-cohort", replacement = "tos-cohort", x = tags132content)
tags132content = strsplit(tags132content, split=";")
taglist = sort(unique(unlist(tags132content)))
length(taglist)
tags.by.articles = lapply(tags132content, is.element, el = taglist)
### simpler:   lapply(tags132content, is.element, el = taglist)
as.matrix(tags.by.articles)
as.matrix(tags.by.articles)[132, ]
tags.by.articles = apply(
  as.matrix(tags.by.articles), 1, unlist)
str(tags.by.articles)
tags.by.articles = t(tags.by.articles)
str(tags.by.articles)
tags.by.articles = as.data.frame(tags.by.articles)

names(tags.by.articles) = taglist
with(tags.by.articles, table(done, DONE))


#### article.df ####
article.df =  data.frame(pmid=pmid132content, doi=doi132content, title=titlescontent, 
                         author1, Nauthors= Nauthors, year=yearscontent, tags.by.articles)
with(article.df, table(pmid=="", doi=="", notrelevant))


# SUMMARIES ####
with(article.df, plot(year, Nauthors))
with(article.df, plot(jitter(year), Nauthors))
with(article.df, plot(as.factor(year), Nauthors))



#########  PREVIOUS CODE #####

first_front = xml_children(xml_children(result)[[1]])[[1]]  ### <front>, includes title, abstract, etc., gets the first result
first_title = xml_find_all(first_front, xpath=".//article-title")  ### <front>, includes title, abstract, etc.
first_title = gsub(pattern = "<[/]*article-title>", "", as.character(first_title)) ## puts title into normal format
first_journal = xml_find_all(xml_children(xml_children(result)[[1]])[[1]], xpath=".//journal-title")  ### <front>, includes title, abstract, etc.
first_journal = gsub(pattern = "<[/]*journal-title>", "", as.character(first_journal))
###  So we can get info from the XML.  A little tricky, but powerful.

fronts = xml_find_all(xml_children(result), xpath=".//front")  ### We DON"T want titles from the bibliographies!
length(fronts)  ## 197  ## Good!
all_titles = xml_find_all(fronts, xpath=".//article-title")  ### <front>, includes title, abstract, etc.
length(all_titles)  ## 198, number of titles in total, should be 197
all_titles[[199]]  ### doesn't exist. good.
all_titles[[198]]  ### but why this one?

all_titles = lapply(fronts, function(front) xml_find_all(front, ".//article-title")) 
length(all_titles)  ## 197   Seems to work better!

# for(a_title in all_titles) 
#   print(paste(collapse=" ", as.character(xml_contents(a_title))))
get_title = function(a_title) ## one title 
  paste(collapse=" ", as.character(xml_contents(a_title))) ## put title into normal format
all_title_strings = sapply(all_titles, get_title)
length(all_title_strings)  ### 197
head(all_title_strings) ## first 6 
tail(all_title_strings) ## last 6 
which(nchar(all_title_strings) < 10)  ## None.
### counting # of authors
table(
  sapply(all_articles, function(an_article)
    length(xml_find_all(xml_find_all(an_article, xpath=".//front") , 
                        xpath=".//article-meta/contrib-group/contrib"))
  )
)

first_author_nodes = sapply(all_articles, 
                       function(an_article)
                         xml_find_one(xml_find_all(an_article, xpath=".//front") , 
                                                   xpath=".//article-meta/contrib-group/contrib")
)
first_author_surnames = sapply(first_author_nodes, 
                       function(an_author)
                         as.character(xml_contents(xml_find_all(an_author, 
                                                   xpath=".//name/surname")))
)
whichNoFirstAuthor = which(sapply(first_author_surnames, length) == 0)  ### These two have no surnames
first_author_surnames[whichNoFirstAuthor] = "(none)"

# ids = xml_find_one(fronts, './/article-meta/article-id')
pmcidnodes = xml_find_all(fronts, './/article-meta/article-id[@pub-id-type="pmc"]') 
length(pmcidnodes)  #197
pmcids = sapply(sapply(sapply(pmcidnodes, xml_contents), as.character), as.numeric)
pmcids = as.vector(pmcids)  ### strip out the names
length(pmcids)  #197

pmids = xml_find_all(fronts, './/article-meta/article-id[@pub-id-type="pmid"]') 
length(pmids) # only 193 
pmidnodes = sapply(fronts, function(front) ## finds the pmids in the "front" 
  xml_find_all(front, './/article-meta/article-id[@pub-id-type="pmid"]') ) 
length(pmidnodes) # now 197. For example#197: xml_nodeset (1)} [1] <article-id pub-id-type="pmid">6324199</article-id>
pmids = 
  sapply(sapply(sapply(pmidnodes, xml_contents), as.character), as.numeric)
which(sapply(pmids, length) == 0)  ### These four have no pmids
pmids[sapply(pmids, length) == 0] = NA
table(is.na(pmids))
pmids = unlist(pmids)

dois = xml_find_all(fronts, './/article-meta/article-id[@pub-id-type="doi"]') 
length(dois) # only 179
doinodes = sapply(fronts, function(front)
  xml_find_all(front, './/article-meta/article-id[@pub-id-type="doi"]') ) 
length(doinodes) # now 197. For example#197: xml_nodeset (1)} [1] <article-id pub-id-type="doi">6324199</article-id>
dois = 
  sapply(sapply(doinodes, xml_contents), as.character)
which(sapply(dois, length) == 0)  ### 18 have no dois
dois[sapply(dois, length) == 0] = ""
table(dois=="")
dois = unlist(dois)

pmc197 = data.frame(pmc=pmcids, pmid=pmids, doi=dois,
                         author1=unlist(first_author_surnames), 
                         title=all_title_strings)
write.csv(file = "pmc197.csv", x = pmc197)
pmc_show = function(n)
  system(paste0('open "http://www.ncbi.nlm.nih.gov/pmc/?term=', pmc197$doi[n], '"') )

###### Subsample for more intensive study:
Narticles = 8 ## sample of 8 articles 
short_result = all_articles[sample(1:length(all_articles), Narticles)] 
length(short_result)
short_title_strings = sapply(xml_find_all(short_result, 
                                          xpath=".//front/article-meta/title-group/article-title"), get_title)
sapply(all_articles, function(an_article)
  length(xml_find_all(xml_find_all(an_article, xpath=".//front") , 
                      xpath=".//article-meta/title-group/article-title"))
)

## to find link 
find_link = sapply (all_articles, 
                   function(an_article)
                     xml_find_one(xml_find_all(an_article, xpath=".//front") , 
                                  first_front xpath=".//article-meta/http") 
) 
## from all of the articles, look at one, find the link -> which is in between <http>    


## search for words with EM 
EM_words = c("Risk ratio", 
             "Risk difference",
             "Hazard ratio", 
             "Sensitivity and specificity",
             "Positive predictive value",
             "negative predictive value",
             "Expected utility",
             "Relative utility", 
             "Number needed to treat", 
             "Area under the ROC curve",
             "Net reclassification improvement", 
             "Accuracy", 
             "Odds ratio") ## create vector of words, see if they match the html 
  

                   
              
  
