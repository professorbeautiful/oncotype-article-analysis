library(xml2)
help(package=xml2)
result = read_xml("pmc_result-oncotype-or-21-ti-or-ab.xml")
##  from this search: http://www.ncbi.nlm.nih.gov/pmc/?term=(oncotype%5BTitle%5D)+OR+oncotype%5BAbstract%5D+OR++(21-gene%5BTitle%5D)+OR+21-gene%5BAbstract%5D
# xml_structure(result)
# result[[1]]
# xml_ns(result)
# contents  = xml_contents(result)
# contents = contents[contents != "\\n"]
# length(contents)

#  http://www.w3schools.com/xsl/xpath_syntax.asp

all_articles = xml_children(result)  ### 197, the same as the search.
first_front = xml_children(xml_children(result)[[1]])[[1]]  ### <front>, includes title, abstract, etc.
first_title = xml_find_all(first_front, xpath=".//article-title")  ### <front>, includes title, abstract, etc.
first_title = gsub(pattern = "<[/]*article-title>", "", as.character(first_title))
first_journal = xml_find_all(xml_children(xml_children(result)[[1]])[[1]], xpath=".//journal-title")  ### <front>, includes title, abstract, etc.
first_journal = gsub(pattern = "<[/]*journal-title>", "", as.character(first_journal))
###  So we can get info from the XML.  A little tricky, but powerful.

fronts = xml_find_all(xml_children(result), xpath=".//front")  ### We DON"T want titles from the bibliographies!
length(fronts)  ## 197  ## Good!
all_titles = xml_find_all(fronts, xpath=".//article-title")  ### <front>, includes title, abstract, etc.
length(all_titles)  ## 198
all_titles[[199]]  ### doesn't exist. good.
all_titles[[198]]  ### but why this one?

all_titles = lapply(fronts, function(front) xml_find_all(front, ".//article-title"))
length(all_titles)  ## 197   Seems to work better!

# for(a_title in all_titles) 
#   print(paste(collapse=" ", as.character(xml_contents(a_title))))
get_title = function(a_title) 
  paste(collapse=" ", as.character(xml_contents(a_title)))
all_title_strings = sapply(all_titles, get_title)
length(all_title_strings)  ### 197
head(all_title_strings)
tail(all_title_strings)
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
                         as.character(xml_find_all(an_author, 
                                                   xpath=".//name/surname"))
)
which(sapply(first_author_surnames, length) == 0)  ### These two have no surnames
first_author_surnames[sapply(first_author_surnames, length) == 0] = "(unknown)"

# ids = xml_find_one(fronts, './/article-meta/article-id')
pmcids = xml_find_all(fronts, './/article-meta/article-id[@pub-id-type="pmc"]') 
length(pmcids)  #197
pmcids = sapply(sapply(sapply(pmidnodes, xml_contents), as.character), as.numeric)
length(pmcids)  #197
which(sapply(pmcids, length) == 0)  ### These four have no pmids
pmcids[sapply(pmcids, length) == 0] = NA
pmcids = unlist(pmcids)

pmids = xml_find_all(fronts, './/article-meta/article-id[@pub-id-type="pmid"]') 
length(pmids) # only 193
pmidnodes = sapply(fronts, function(front)
  xml_find_all(front, './/article-meta/article-id[@pub-id-type="pmid"]') ) 
length(pmidnodes) # now 197. For example#197: xml_nodeset (1)} [1] <article-id pub-id-type="pmid">6324199</article-id>
pmids = 
  sapply(sapply(sapply(pmidnodes, xml_contents), as.character), as.numeric)
which(sapply(pmids, length) == 0)  ### These four have no pmids
pmids[sapply(pmids, length) == 0] = NA
table(is.na(pmids))
pmids = unlist(pmids)


write.csv(file = "pmc197.csv", x = 
            data.frame(pmc=pmcids, pmid=pmids, 
                         author1=unlist(first_author_surnames), 
                         title=all_title_strings)
          )

###### Subsample for more intensive study:
Narticles = 8
short_result = all_articles[sample(1:length(all_articles), Narticles)]
length(short_result)
short_title_strings = sapply(xml_find_all(short_result, 
                                          xpath=".//front/article-meta/title-group/article-title"), get_title)
sapply(all_articles, function(an_article)
  length(xml_find_all(xml_find_all(an_article, xpath=".//front") , 
                      xpath=".//article-meta/title-group/article-title"))
)
