library(pubmed.mineR)
library(easyPubMed)
library(rentrez)
## xPath syntax: http://www.w3schools.com/xml/xpath_syntax.asp

`%&%` = function(a, b) paste0(a, b)
pbcopy = function(x) write(x, file = "|pbcopy")

query  = '(oncotype[title/abstract] 
 OR ("21-gene"[title/abstract] NOT "ORF 21"[ti/ab] NOT IL21[ti/ab]
 NOT "Interleukin 21"[ti/ab]) 
 ) AND "loattrfree full text"[sb] AND English[lang]'
query = gsub('\n', ' ', query)
otc_on_pubmed <- easyPubMed::get_pubmed_ids(query)
otc_papers <- fetch_pubmed_data(otc_on_pubmed)

getField = function(fieldname, papers=otc_papers, verbose=FALSE){
  result = unlist(xpathApply(papers, "//" %&% fieldname, saveXML))
  fn = gsub(".*/", "", fieldname)
  result <- gsub("<" %&% fn %&% ".*>(.*)<\\/" %&% fn %&% ">",
                 "\\1", result)
  print(length(result))
  if(verbose) return(result)
 invisible(result)
}

#### getting oriented... ####
xmlName(xmlRoot(otc_papers))
length(lapply(xmlChildren(otc_papers), xmlChildren))
xmlApply(getNodeSet(otc_papers, "//PubmedArticleSet"), xmlName)

#### Get all the nodes in the entire tree. ####
table(unlist(sapply(otc_papers["//*|//text()|//comment()|//processing-instruction()"],
                    class)))
table(unlist(sapply(otc_papers["//*"], class)))
table(unlist(sapply(otc_papers["//*"], xmlName)))

#### all nodeNames ####
nodeNames = unique(unlist(sapply(otc_papers["//*"], xmlName)))  ### 96 
grep(value=T, "title", nodeNames, ignore.case = T)
getField("ArticleTitle")   # 401
grep(value=T, "abstract", nodeNames, ignore.case = T)
getField("Abstract")  # only 391, might not be one2one

#### The unit is the article ####
articlesAsStrings = getField("PubmedArticle")  #### 401
articles =  getNodeSet(otc_papers, "//PubmedArticle")
length(articles)
articles[[1]]
class(articles[[1]])
##[1] "XMLInternalElementNode" "XMLInternalNode"        "XMLAbstractNode" 


#### examples of text nodes: ####
otc_papers["//text()"] [1:20]

#### This gives all search keywords for pubmed! ####
pubmedKWs = rentrez::entrez_db_searchable('pubmed')
names(pubmedKWs)
length(pubmedKWs)  #51
pubmedKWs[[5]][[2]]
pubmedKWs[[5]]$Description
KWdescriptions = sapply(pubmedKWs, '[[', 'Description')
grep(value=T, 'journal', KWdescriptions, ignore.case=T)
grep(value=T, 'first', KWdescriptions, ignore.case=T)
grep(value=T, 'title', KWdescriptions, ignore.case=T)
grep(value=T, 'abstract', KWdescriptions, ignore.case=T)

#### distribution by year ####
grep(value=T, "year", nodeNames, ignore.case = T)
grep(value=T, "date", nodeNames, ignore.case = T)
articleYears = getField("ArticleDate/Year")
table(articleYears)

#### PMIDs to pasteboard ####
grep(value=T, "pmid", nodeNames, ignore.case = T)
PMIDlist = (getField("MedlineCitation/PMID"))
