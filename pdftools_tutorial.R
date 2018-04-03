library(tidyverse)
library(stringr)

library(pdftools)

?pdftools
# pdf_info(pdf, opw = "", upw = "")
# pdf_text(pdf, opw = "", upw = "")
# pdf_fonts(pdf, opw = "", upw = "")
# pdf_attachments(pdf, opw = "", upw = "")
# pdf_toc(pdf, opw = "", upw = "")

pdf_smith <- file.path('pdfs/smith_wilen_2003.pdf')

smith_text <- pdf_text(pdf_smith)

### create a data frame to work with more easily
smith_df <- data.frame(text = smith_text) 

smith_df <- data.frame(text = smith_text) %>%
  mutate(page = 1:n()) %>%
  mutate(text_sep = str_split(text, '\n')) %>%
  unnest(text_sep)

smith_df <- data.frame(text = smith_text) %>%
  mutate(page = 1:n()) %>%
  mutate(text_sep = str_split(text, '\n')) %>%
  unnest(text_sep) %>%
  group_by(page) %>%
  mutate(line = 1:n()) %>%
  ungroup()

### We want to extract data from the table on page 8
page8_df <- smith_df %>%
  filter(page == 8)

### Let's just brute force cut out the table
col_lbls <- c('n_patches', paste0('y', 1988:1999))

table1_df <- page8_df %>%
  filter(line %in% 8:18) %>%
  separate(text_sep, col_lbls, ' +') 

table1_tidy_df <- table1_df %>%
  select(-text, -line, -page) %>%
  gather(year, n_divers, starts_with('y')) %>%
  mutate(year = str_replace(year, 'y', ''), ### or str_extract(year, '[0-9]{4}')
         year = as.integer(year),
         n_patches = as.integer(n_patches),
         n_divers = as.integer(n_divers))

#####################################################
### Try with tabulizer
#####################################################
### Requires Java - I downloaded/installed the JDK (includes JRE)
### http://www.oracle.com/technetwork/java/javase/downloads/index.html

### dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/jre/lib/server/libjvm.dylib'))
### in terminal: R CMD javareconf

# rJava should load
# install.packages('rJava')
library(rJava)

# devtools::install_github(c("ropensci/tabulizerjars"))
# devtools::install_github(c("ropensci/tabulizer"))
library(tabulizer)

pdf_smith <- file.path('pdfs/smith_wilen_2003.pdf')

tables_list <- tabulizer::extract_tables(pdf_smith)

tab1_df <- tables_list[[1]] %>%
  as.data.frame(stringsAsFactors = FALSE)
### Not perfect but pretty good for a starting point, and for automating
### PDF extraction