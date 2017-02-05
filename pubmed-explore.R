library(pubmed.mineR)
library(easyPubMed)
library(rentrez)

`%&%` = function(a, b) paste0(a, b)
query  = '(oncotype[title/abstract] 
OR ("21-gene"[title/abstract] NOT "ORF 21"[ti/ab] NOT IL21[ti/ab]) 
) AND "loattrfree full text"[sb] AND English[lang]'
query = gsub('\n', ' ', query)
otc_on_pubmed <- easyPubMed::get_pubmed_ids(query)
otc_papers <- fetch_pubmed_data(otc_on_pubmed)
getField = function(fieldname, papers=otc_papers){
  result = unlist(xpathApply(otc_papers, "//" %&% fieldname, saveXML))
result_pos <- regexpr("<" %&% fieldname %&% ">.*<\\/" %&% fieldname %&% ">", 
                      result)
result <- gsub("<" %&% fieldname %&% ">(.*)<\\/" %&% fieldname %&% ">",
               "\\1", result)
#  substr(result, title_pos + 14, title_pos + attributes(title_pos)$match.length - 16)
print(length(result))
invisible(result)
}
getField("ArticleTitle")
getField("Abstract")

xmlName(xmlRoot(otc_papers))
lapply(xmlChildren(otc_papers), xmlChildren)
xmlApply(getNodeSet(otc_papers, "//PubmedArticleSet"), xmlName)
# Get all the nodes in the entire tree.
table(unlist(sapply(otc_papers["//*|//text()|//comment()|//processing-instruction()"],
                    class)))
table(unlist(sapply(otc_papers["//*"], class)))
table(unlist(sapply(otc_papers["//*"], xmlName)))
nodeNames = unique(unlist(sapply(otc_papers["//*"], xmlName)))  ### 96 
grep(value=T, "abstract", nodeNames, ignore.case = T)
### examples of text nodes:
otc_papers["//text()"] [1:20]



### This gives all search keywords for pubmed!
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

otc_papers

