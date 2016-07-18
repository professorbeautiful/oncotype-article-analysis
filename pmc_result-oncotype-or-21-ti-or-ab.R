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
for(a_title in all_titles) 
  print(paste(collapse=" ", as.character(xml_contents(a_title))))
get_title = function(a_title) 
  paste(collapse=" ", as.character(xml_contents(a_title)))
all_title_strings = sapply(all_titles, get_title)
length(all_title_strings)
head(all_title_strings)
length(all_title_strings)
tail(all_title_strings)
which(nchar(all_title_strings) < 10)  ## None.

short_result = all_articles[sample(1:length(all_articles), 8)]
length(short_result)
short_title_strings = sapply(xml_find_all(short_result, 
                                          xpath=".//front/article-meta/title-group/article-title"), get_title)
numberoftitle = sapply(xml_find_all(short_result, 
                                          xpath=".//front/article-meta/title-group/article-title"), 
                       count_titles)

### counting # of authors
table(
  sapply(all_articles, function(an_article)
    length(xml_find_all(xml_find_all(an_article, xpath=".//front") , 
                        xpath=".//article-meta/contrib-group/contrib"))
  )
)
### counting # of titles
table(
  sapply(all_articles, function(an_article)
    length(xml_find_all(xml_find_all(an_article, xpath=".//front") , 
                        xpath=".//article-meta/title-group/article-title"))
  )
)
sapply(all_articles, function(an_article)
  length(xml_find_all(xml_find_all(an_article, xpath=".//front") , 
                      xpath=".//article-meta/title-group/article-title"))
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
