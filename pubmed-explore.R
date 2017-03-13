library(pubmed.mineR)
library(easyPubMed)
library(rentrez)
library(xml2)
## xPath syntax: http://www.w3schools.com/xml/xpath_syntax.asp

`%&%` = function(a, b) paste0(a, b)
pbcopy = function(x) write(x, file = "|pbcopy")

exclusions21 = c('ORF','IL', 'Interleukin', 'FGF', 'kinase',
                 'kappa', 'mir', 'microrna', 'ighv3', 'factor', 'trisomy',
                 'chromosome')
exclusion21clause = paste('"', exclusions21, '21"[ti]', collapse=' NOT ')
query  = paste('
"loattrfree full text"[sb] 
 AND English[lang]
               NOT colon[ti] 
               NOT prostate[ti]
  AND (Oncotype[title/abstract] 
    OR ("21-gene"[title/abstract] NOT ',
               exclusion21clause,
               ') )'
)
query  = paste('
"loattrfree full text"[sb] 
               AND English[lang]
               AND breast[ti] 
               AND cancer[ti]
               AND (oncotype[title/abstract] 
               OR ("21-gene"[title/abstract] NOT ',
               exclusion21clause,
               ') )'
               )

#                NOT "ORF 21"[ti/ab] NOT IL21[ti/ab]
#   NOT "Interleukin 21"[ti/ab] NOT "FGF21" NOT "FGF 21"
#   NOT "thymidine kinase" NOT "kappa 21 gene" NOT "mir-21" NOT "microrna-21"
#   NOT "IGHV3-21" NOT "transcription factor 21") 
#  ) 
# '
# thymidine kinase (TK; ATP:thymidine 5'-phosphotransferase, EC 2.7.1.21) gene 

getField = function(fieldname, papers=otc_papers, verbose=FALSE){
  result = unlist(xpathApply(papers, "//" %&% fieldname, saveXML))
  fn = gsub(".*/", "", fieldname)
  result <- gsub("<" %&% fn %&% ".*>(.*)<\\/" %&% fn %&% ">",
                 "\\1", result)
  print(length(result))
  if(verbose) return(result)
 invisible(result)
}

makeQueryDataFrame = function(query){
  query = gsub('\n', ' ', query)
  otc_on_pubmed <- easyPubMed::get_pubmed_ids(query)
  otc_papers <- fetch_pubmed_data(otc_on_pubmed)
  titles =  getNodeSet(otc_papers, "//PubmedArticle/MedlineCitation/Article/ArticleTitle/text()")
  titlesText =  sapply(titles,
                       function(ti)xmlValue(ti))
  abstracts = xmlSApply(getNodeSet(otc_papers, "//PubmedArticle"),
                        getNodeSet, path="MedlineCitation/Article/Abstract/AbstractText/text()")
  abstractsText = sapply(abstracts,
                         function(ab)
                           paste(sapply(ab, xmlValue), collapse="\n"))
  PMIDlist = (getField("MedlineCitation/PMID", papers=otc_papers))
  articleYears =  xmlSApply(
    getNodeSet(otc_papers, 
               "//PubmedArticle"),
    getNodeSet, path="MedlineCitation/Article/ArticleDate/Year/text()")
  articleYearsNodes = sapply(articleYears, '[[', i=1)
  articleYearsS = sapply(articleYearsNodes, xmlValue)
  return(data.frame(
    PMID=PMIDlist,
    year=articleYearsS,
    title=titlesText,
    abstract=abstractsText,
    hasOncotype= (grepl("Oncotype", paste(titlesText, abstractsText)))
  ))
}

queryDF = makeQueryDataFrame(query)


#### getting oriented... ####
xmlName(xmlRoot(otc_papers))
length(lapply(xmlChildren(otc_papers), xmlChildren))
xmlApply(getNodeSet(otc_papers, "//PubmedArticleSet"), xmlName)

#### Get all the nodes in the entire tree. ####
# table(unlist(
#   sapply(FUN = class, 
#          X = otc_papers["//*|//text()|//comment()|//processing-instruction()"]
#   )))
# XMLAbstractNode XMLInternalElementNode        XMLInternalNode 
# 168605                 100245                 168605 
# XMLInternalPINode    XMLInternalTextNode 
# 5                  68355 
#table(unlist(sapply(otc_papers["//*"], class)))
# XMLAbstractNode XMLInternalElementNode        XMLInternalNode 
# 100245                 100245                 100245 
#table(unlist(sapply(otc_papers["//*"], xmlName)))
# Abstract            AbstractText         AccessionNumber 
# 395                     840                     609 
# AccessionNumberList                 Acronym             Affiliation 
# 33                     668                    1365 
# AffiliationInfo                  Agency                 Article 
# 1365                     707                     405 
# ArticleDate               ArticleId           ArticleIdList 
# 264                    1350                     411 
# ArticleTitle                  Author              AuthorList 
# 405                    3067                     403 
# Book            BookDocument               BookTitle 
# 3                       3                       3 
# Chemical            ChemicalList          CitationSubset 
# 1061                     266                     375 
# CoiStatement         CollectionTitle          CollectiveName 
# 13                       3                       8 
# CommentsCorrections CommentsCorrectionsList    CopyrightInformation 
# 11035                     296                      63 
# Country                DataBank            DataBankList 
# 1112                      33                      31 
# DataBankName           DateCompleted             DateCreated 
# 33                     364                     405 
# DateRevised                     Day          DescriptorName 
# 405                    3187                    4568 
# ELocationID                ForeName              GeneSymbol 
# 300                    3155                      14 
# GeneSymbolList                   Grant                 GrantID 
# 2                     707                     696 
# GrantList                 History                    Hour 
# 140                     408                    1182 
# Identifier                Initials            Investigator 
# 13                    3155                      97 
# InvestigatorList         ISOAbbreviation                    ISSN 
# 3                     404                     378 
# ISSNLinking                   Issue                 Journal 
# 394                     351                     405 
# JournalIssue                 Keyword             KeywordList 
# 405                     472                      93 
# Language                LastName                  Medium 
# 409                    3156                       3 
# MedlineCitation             MedlineDate      MedlineJournalInfo 
# 405                       8                     405 
# MedlinePgn               MedlineTA             MeshHeading 
# 397                     405                    4568 
# MeshHeadingList                  Minute                   Month 
# 316                    1182                    3380 
# NameOfSubstance             NlmUniqueID                    Note 
# 1061                     405                       1 
# NumberOfReferences           OtherAbstract                 OtherID 
# 10                       2                     324 
# Pagination                    PMID                 PubDate 
# 397                   11435                     408 
# PublicationStatus         PublicationType     PublicationTypeList 
# 408                     868                     405 
# Publisher       PublisherLocation           PublisherName 
# 3                       3                       3 
# PubmedArticle        PubmedArticleSet       PubmedBookArticle 
# 405                       1                       3 
# PubmedBookData              PubmedData           PubMedPubDate 
# 3                     405                    1610 
# QualifierName               RefSource          RegistryNumber 
# 2667                   11035                    1061 
# Season                 Section                Sections 
# 1                      39                       3 
# SectionTitle                  Suffix           SupplMeshList 
# 39                      17                       5 
# SupplMeshName                   Title                  Volume 
# 5                     405                     398 
# Year 
# 3448 

#### all nodeNames ####
nodeNames = unique(unlist(sapply(otc_papers["//*"], xmlName)))  ### 103 

#### Titles ####
cat(paste(collapse=", ", grep(value=T, "title", nodeNames, ignore.case = T) ) )
#Title, ArticleTitle, BookTitle, CollectionTitle, SectionTitle
getField("ArticleTitle")   # 401

titles =  getNodeSet(otc_papers, "//PubmedArticle/MedlineCitation/Article/ArticleTitle/text()")
length(titles)
class(titles)   ### "XMLNodeSet"
titles[[1]]
titlesText =  sapply(titles,
                     function(ti)xmlValue(ti))
length(titlesText)  # 386
titlesText
pbcopy(titlesText)
#### The unit is the article ####
# articlesAsStrings = getField("PubmedArticle")  #### 401
articles =  getNodeSet(otc_papers, "//PubmedArticle")
length(articles)  # 386
articles[[1]]
class(articles[[1]])
##[1] "XMLInternalElementNode" "XMLInternalNode"        "XMLAbstractNode" 

#### abstracts ####
cat(paste(collapse=", ", grep(value=T, "abstract", nodeNames, ignore.case = T) ))
# Abstract, AbstractText, OtherAbstract
getField("Abstract")  # only 376, might not be one2one
# abstracts = lapply(articles, function(article)
#   nodeset = getNodeSet(article, "//PubmedArticle/MedlineCitation/Article/Abstract/AbstractText/text()")
# )
# length(abstracts)  ### 386
# abstracts[[2]]
# saveXML(abstracts[[1]])  # fails
abstracts = xmlSApply(getNodeSet(otc_papers, "//PubmedArticle"),
                      getNodeSet, path="MedlineCitation/Article/Abstract/AbstractText/text()")
length(abstracts)  ### 386
abstracts[[1]]  ### Looks good.
abstracts[[5]]  ### 4 paragraphs.
sapply(abstracts, length)
table(sapply(abstracts, length))
abstractsText = sapply(abstracts,
                       function(ab)
                         paste(sapply(ab, xmlValue), collapse="\n"))
abstractsText[[length(abstractsText)]]
length(abstractsText)  ### 342.
##### OK, use abstractsText
summary(nchar(abstractsText))
#### Do any abstracts end in "..."?
sum("..."==substr(abstractsText, nchar(abstractsText)-3, nchar(abstractsText)))
### None do.


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
pbcopy(PMIDlist)


#### trying entrez_fetch instead - need full abstracts ####
fetched = rentrez::entrez_fetch(
  db = "pubmed", rettype="xml", id=PMIDlist, parsed = TRUE)
class(fetched)
getField("Abstract", papers=fetched)  # 329
getField("Abstract", papers=otc_papers)  # 332
# # And... missing 3?
# length(PMIDlist)  ## 342
# abstracts =  getNodeSet(fetched, "//PubmedArticle/MedlineCitation/Article/Abstract/AbstractText/text()")
# abstracts[[1]]  ## Also truncated. Why? By NCBI? XML?
# saveXML(abstracts[[1]], file = "ab1.txt")  ## Also truncated. Why? By NCBI? XML?
# fetchedUnparsed = rentrez::entrez_fetch(db = "pubmed", rettype="xml", id=PMIDlist,
#                                 parsed = F)
# class(fetchedUnparsed)
# write(x = fetchedUnparsed, file='fetchedUnparsed.txt')
# # OK the abstract is NOT truncated.
# fetchedUnparsedParsed = xmlParse(file='fetchedUnparsed.txt',
#                                  options=PEDANTIC)
# abstracts = getNodeSet(fetchedUnparsedParsed, "//PubmedArticle/MedlineCitation/Article/Abstract/AbstractText/text()")
# xmlValue(abstracts[[1]])  ## Also truncated! So either xmlParse or the print method.
# fetched2 = xml2::read_xml(x='fetchedUnparsed.txt')
# abstracts2 = xml2::xml_find_all(fetched2, "//PubmedArticle/MedlineCitation/Article/Abstract/AbstractText/text()")
# abstracts2[[1]]

### The solution to the truncated problem:  in RStudio preferences,
# Code/Display/Limit length of lines displayed in console to… 

#### Look for obvious non-oncotype dx articles to remove ####
accepted = grep("Oncotype|21-gene|21 tumor- *associated genes",
                abstractsText)
accepted = union(accepted, 
             grep("Oncotype|21-gene|21 tumor- *associated genes",
                titlesText) )
tiabText = paste(titlesText, abstractsText)
breast = grep("breast", tiabText, ignore.case = TRUE)
setdiff(accepted, breast)
setdiff(breast, accepted)
length(accepted)
rejected = c(grep("FGF21|trisomy", tiabText))
working = setdiff(grep("[^0-9\\.-]21", abstractsText), 
        union(accepted, rejected) )
working = union(setdiff(accepted, breast), setdiff(breast, accepted))
while(length(working) > 0) {
  N=working[1]; cat(titlesText[N], "\n", abstractsText[N], "\n", N, "\n")
  ans = readline("Accept (a)? Reject (CR)?: ")
  if(ans=="a") accepted = c(accepted, N)
  else rejected = c(rejected, N)
  working = working [-1]
}

# 108 109
