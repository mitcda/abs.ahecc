### =========================================================================
### Filename:     build-ahecc.R
### Created:      2017-09-28
### Updated:      <2017-10-05 15:17:44 david at grover>
### Author:       David Mitchell <david.p.mitchell@homemail.com.au>
### Description:  Build Australian Harmonised Export Commodity Classification
###               \url{http://www.abs.gov.au/ausstats/abs@.nsf/mf/1269.0}
### 
### =========================================================================

######  Section 0 - Libraries & settings
library(magrittr);
library(rvest);        ## Web scraping functions
library(urltools);     ## URL parsing and composing
library(XML);          ## 
library(httr);         ## 
library(readxl);       ## Read Excel files
library(dplyr);
library(tidyr);

### Settings
DEBUG <- FALSE


### Functions
#' @name which_link
#' @title return URL link number matching specified \code{pattern}
#' @description return the link numbers
#' @import xml2::read_html rvest::html_nodes, rvest::html_text
#' @param x a session
#' @param node a node set or a single mode
#' @param pattern character string containing a regular expression to be matched against the node set string
#' @return returns integer specifying the number of the matching link
which_link <- function(x, node, pattern) {
  x %>% xml2::read_html(.) %>%
  rvest::html_nodes(node) %>%
  rvest::html_text() %>%
  grep(pattern, .)
}



######  Section 1 - Import AHECC supporting data
Ausstats.url <- "http://www.abs.gov.au/ausstats/abs@.nsf/mf";
Data.url     <- file.path(Ausstats.url, "1233.0");
File.regex       <- "123301.*\\.xls(x)*"; ## AHECC Complete File
## Return the file path
.Path <- Data.url %>%
  html_session %>%
  follow_link("Downloads") %>%
  read_html %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  gsub("\\s","%20",.) %>%   ## Preserve/restore HTML whitespace marker '%20'  
  grep(File.regex, ., ignore.case=TRUE, value=TRUE);
## Get URL
.URL <- sapply(.Path,
               function(x) Data.url %>% url_parse %>%
                           inset("path", value=x) %>%
                           url_compose,
               USE.NAMES=FALSE);
.Destfiles <- .URL %>% basename %>% sub(paste0(".*(",File.regex,").*"),"\\1",.) %>% file.path(tempdir(), .)
for (i in seq_along(.URL))
  download.file(.URL[i], .Destfiles[i], mode="wb");


##### 1.1 - Load AHECC units of quantity (UQ) data
ahecc_uq <- read_excel(.Destfiles, sheet="UQ", skip=7) %>%
  select(-matches("X_+\\d+")) %>%
  set_names(c("abbreviation", "description")) %>%
  mutate_at(vars(description), funs(sub("^(.+)\\s+\\(\\w\\)\\s*$", "\\1", .))) %>%
  mutate(note = ifelse(description == "Basic carton",
                       "A basic carton consists of 24 x 825g cans or equivalent.  For industry purposes there are 50 basic cartons to the tonne.",
                ifelse(description == "Carton",
                       "A carton consists of 24 x 425g cans or equivalent.  For industry purposes there are approximately 100 cartons to the tonne.", 
                       ""))) %>%
  filter(grepl("^\\w{1,2}$", abbreviation));

if (DEBUG) {
  ahecc_uq %>% dim
  ahecc_uq %>% as.data.frame %>% head; ## tail
}


##### 1.2 - Load AHECC abbreviations
ahecc_abb <- read_excel(.Destfiles, sheet="Abbreviations", skip=7) %>%
  select(-matches("X_+\\d+")) %>%
  set_names(c("abbreviation", "description")) %>%
  filter(!is.na(description));

if (DEBUG) {
  ahecc_abb %>% dim
  ahecc_abb %>% as.data.frame %>% tail; ## head
}


##### 1.3 - Load AHECC FOB currency codes
ahecc_currencies <- read_excel(.Destfiles, sheet="FOB Currency Codes", skip=7) %>%
  set_names(names(.) %>% tolower %>% gsub("\\s+", "_", .)) %>%
  filter(grepl("\\w{3}", currency_code));

if (DEBUG) {
  ahecc_currencies %>% dim
  ahecc_currencies %>% as.data.frame %>% head; ## tail
}

##### 1.4 - Load AHECC countries
ahecc_countries <- read_excel(.Destfiles, sheet="Countries", skip=7) %>%
  select(-matches("X_+1")) %>%
  set_names(c("code","country","notes")) %>%
  filter(grepl("\\w{2}", code));

if (DEBUG) {
  ahecc_countries %>% dim
  ahecc_countries %>% as.data.frame %>% head; ## tail
}



######  Section 2 - Import AHECC 2017 data
Ausstats.url <- "http://www.abs.gov.au/ausstats/abs@.nsf/mf";
Data.url     <- file.path(Ausstats.url, "1233.0");
File.regex       <- "1233024.*\\.xls(x)*"; ## AHECC Complete File
## Return the file path
.Path <- Data.url %>%
  html_session %>%
  follow_link("Downloads") %>%
  read_html %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  gsub("\\s","%20",.) %>%   ## Preserve/restore HTML whitespace marker '%20'  
  grep(File.regex, ., ignore.case=TRUE, value=TRUE);
## Get URL
.URL <- sapply(.Path,
               function(x) Data.url %>% url_parse %>%
                           inset("path", value=x) %>%
                           url_compose,
               USE.NAMES=FALSE);
.Destfiles <- .URL %>% basename %>% sub(paste0(".*(",File.regex,").*"),"\\1",.) %>% file.path(tempdir(), .)
for (i in seq_along(.URL))
  download.file(.URL[i], .Destfiles[i], mode="wb");


### Load AHECC data
ahecc <- read_excel(.Destfiles, sheet="Complete File", skip=4) %>%
  set_names(c("ahecc_code", "chapter_code", "chapter_label",
              "heading_code", "heading_label", "hs5_code", "hs5_label",
              "hs_code", "hs_label", "hs7_code", "hs7_label",
              "export_item", "quantity_unit",
              "description", "free_descriptor")) %>%
  filter(grepl("AHECC_\\d+", ahecc_code)) %>%
  mutate_at(vars(chapter_code, chapter_label, heading_code, heading_label,
                 hs5_code, hs5_label, hs_code, hs_label),
            funs(zoo::na.locf(., na.rm=FALSE))) %>%
  filter(!is.na(export_item)) %>%
  mutate_at(vars(chapter_label, heading_label, hs5_label, hs_label, description),
            funs(sub("^(.+):$", "\\1", sub("^\\s*-*\\s*(.+)$", "\\1", .))))

if (DEBUG) {
  ahecc %>% dim
  ahecc %>% as.data.frame %>% head; ## tail
}
ahecc_2017 <- ahecc;



######  Section 2 - Import AHECC 2012 data
## Return the file path
.Path <- Data.url %>%
  html_session %>%
  follow_link(which_link(Data.url, "a", "Past.+&.+Future.+Releases"));
.Path <- .Path %>%
  follow_link(which_link(.Path, "a", "Electronic.+Publication.+Jan.+2012"))
.Path <- .Path %>%
  follow_link("Downloads") %>%
  read_html %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  gsub("\\s","%20",.) %>%   ## Preserve/restore HTML whitespace marker '%20'  
  grep(File.regex, ., ignore.case=TRUE, value=TRUE);
## Get URL
.URL <- sapply(.Path,
               function(x) Data.url %>% url_parse %>%
                           inset("path", value=x) %>%
                           url_compose,
               USE.NAMES=FALSE);
.Destfiles <- .URL %>% basename %>% sub(paste0(".*(",File.regex,").*"),"\\1",.) %>% file.path(tempdir(), .)
for (i in seq_along(.URL))
  download.file(.URL[i], .Destfiles[i], mode="wb");


### Load AHECC data
ahecc <- read_excel(.Destfiles, sheet="Complete File", skip=4) %>%
  set_names(c("ahecc_code", "chapter_code", "chapter_label",
              "heading_code", "heading_label", "hs5_code", "hs5_label",
              "hs_code", "hs_label", "hs7_code", "hs7_label",
              "export_item", "quantity_unit",
              "description", "free_descriptor")) %>%
  filter(grepl("AHECC_\\d+", ahecc_code)) %>%
  mutate_at(vars(chapter_code, chapter_label, heading_code, heading_label,
                 hs5_code, hs5_label, hs_code, hs_label),
            funs(zoo::na.locf(., na.rm=FALSE))) %>%
  filter(!is.na(export_item)) %>%
  mutate_at(vars(chapter_label, heading_label, hs5_label, hs_label, description),
            funs(sub("^(.+):$", "\\1", sub("^\\s*-*\\s*(.+)$", "\\1", .))))
if (DEBUG) {
  ahecc %>% dim
  ahecc %>% as.data.frame %>% head; ## tail
}

ahecc_2012 <- ahecc;



######  Section 3 - Import AHECC 2007 data
## Return the file path
.Path <- Data.url %>%
  html_session %>%
  follow_link(which_link(Data.url, "a", "Past.+&.+Future.+Releases"));
.Path <- .Path %>%
  follow_link(which_link(.Path, "a", "Electronic.+Publication.+Jan.+2007"))
.Path <- .Path %>%
  follow_link("Downloads") %>%
  read_html %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  gsub("\\s","%20",.) %>%   ## Preserve/restore HTML whitespace marker '%20'  
  grep(File.regex, ., ignore.case=TRUE, value=TRUE);
## Get URL
.URL <- sapply(.Path,
               function(x) Data.url %>% url_parse %>%
                           inset("path", value=x) %>%
                           url_compose,
               USE.NAMES=FALSE);
.Destfiles <- .URL %>% basename %>% sub(paste0(".*(",File.regex,").*"),"\\1",.) %>% file.path(tempdir(), .)
for (i in seq_along(.URL))
  download.file(.URL[i], .Destfiles[i], mode="wb");


### Load AHECC data
ahecc <- read_excel(.Destfiles, sheet="Sheet1", skip=5, col_names=F) %>%
  set_names(c("X1",
              "ahecc_code",
              "chapter_code", "chapter_label",
              "heading_code", "heading_label",
              "hs5_code", "hs5_label",
              "hs_code", "hs_label",
              "hs7_code", "hs7_label",
              "X2",
              "export_item", "quantity_unit",
              "description",
              "free_descriptor")) %>%
  filter(grepl("AHECC_\\d+", ahecc_code)) %>%
  select(-X1, -X2) %>%
  mutate_at(vars(chapter_code, chapter_label, heading_code, heading_label,
                 hs5_code, hs5_label, hs_code, hs_label),
            funs(zoo::na.locf(., na.rm=FALSE))) %>%
  filter(!is.na(export_item)) %>%
  mutate_at(vars(chapter_label, heading_label, hs5_label, hs_label, description),
            funs(sub("^(.+):$", "\\1", sub("^\\s*-*\\s*(.+)$", "\\1", .))))
if (DEBUG) {
  ahecc %>% dim
  ahecc %>% as.data.frame %>% head; ## tail
}

ahecc_2007 <- ahecc;


###### Section 4 - Write data sets files
devtools::use_data(ahecc_uq, ahecc_abb, ahecc_currencies, ahecc_countries,
                   ahecc_2017, ahecc_2012, ahecc_2007,
                   overwrite=TRUE);

### =============================== EOF =====================================
