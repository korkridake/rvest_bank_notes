# ---------------------------------------------------------
# Rvest in action: list of people on bank notes 
# https://en.wikipedia.org/wiki/List_of_people_on_banknotes
# Author: Kyle Akepanidtaworn, Data Scientist
# https://www.linkedin.com/in/korkridakepan/
# © Copyright 2018, Kyle Akepanidtaworn, All rights reserved.
# ---------------------------------------------------------

# ---------------------------------------------------------
# Step 1: Import Library
# ---------------------------------------------------------
library(tidyverse)
library(rvest)
library(rlist)
library(stringi)
library(htmltab)

# ---------------------------------------------------------
# Step 2: Inspect HTML
# ---------------------------------------------------------
url = 'https://en.wikipedia.org/wiki/List_of_people_on_banknotes'
url2 = html('https://en.wikipedia.org/wiki/List_of_people_on_banknotes')

# ---------------------------------------------------------
# Simple Cases
# ---------------------------------------------------------
sample = url %>%
  read_html() %>%
  html_node('body #content #bodyContent #mw-content-text .mw-parser-output table') %>%
  html_table(fill = TRUE)

# Return a data frame
sample2 = url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE)

# Return a list
sample3 = url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE)
sample3 = sample3[[1]]

sample_all = rbind(sample, sample2, sample3)

# Language of rvest
# Use # in front of 'id' element
# Use . in front of 'class' element
# Use a, div, h1, h2, h3, or body to get into that chunk

# html_node (get only the top one) vs. html_nodes (get multiple of them)
countries = url2 %>%
  html_nodes("body #content #bodyContent #mw-content-text .mw-parser-output h3 .mw-headline") %>%
  html_text() %>%
  as.character()
countries[9:209] <- c('Bangladesh', countries[9:208])
countries[52:210] <- c('Israel', countries[52:209])
countries[86:211] <- c('Poland', countries[86:210])
countries[97:212] <- c('Singapore', countries[97:211])
countries <- countries[-94] # Remove Scotland
countries[113:212] <- c('United Kingdom', countries[113:211])
countries[113:213] <- c('United Kingdom', countries[113:212])
countries[175:214] <- c('Lithuania', countries[175:213])
countries[208:215] <- c('United Kingdom', countries[208:214])
print(paste('There are', length(countries), 'countries in total'))

# How many tables are there? 215 Tables
num_tables = url2 %>%
  html_nodes("body #content #bodyContent #mw-content-text .mw-parser-output table") %>%
  length() %>%
  as.character()
print(paste('There are', num_tables, 'tables in total'))

# This condition needs to be true to proceeed
length(countries) == num_tables # TRUE

# ---------------------------------------------------------
# Iterate All Cases
# ---------------------------------------------------------
for (i in 1:3){
  print(paste0('//*[@id="mw-content-text"]/div/table[', i, ']'))
}

# Create an accumulator for all xpaths
# Last one: //*[@id="mw-content-text"]/div/table[215]
xpathall = c()
for (i in 1:as.numeric(num_tables)){
  xpathall = list.append(xpathall, paste0('//*[@id="mw-content-text"]/div/table[', i, ']'))
}

# Create an accumulator for aggregated data frames 
df_all = data.frame()

for (i in 1:length(xpathall)){
  if (i %in% c(113, 114, 115)){
    print(stri_dup("-",40))
    print(paste('Start the', as.character(i), 'iteration'))
    print('This is a special case.')
    df = url %>%
      read_html() %>%
      html_nodes(xpath = xpathall[i]) %>%
      html_table(fill = TRUE)
    df = df[[1]]
    colnames(df) = df[1, ]
    df = df[-1,]
    df['iteration'] = i
    df['country_name'] = countries[i]
    colnames(df) <- tolower(colnames(df))
    colnames(df) <- gsub(' ', '_', colnames(df))
    colnames(df) <- gsub('/', '_', colnames(df))
    colnames(df) <- gsub('-', '_', colnames(df))
    colnames(df) <- gsub(" ", "", colnames(df), fixed = TRUE)
    df = df %>% 
      mutate_all(as.character) %>%
      filter(person != "Person") %>%
      filter(!grepl("Bank", person))
    print(paste('Complete the handling of special case'))
    print(paste('Append to the data frame accumulator'))
    df_all = bind_rows(df_all, df)
    print(paste('Remove the', as.character(i), 'data'))
    rm(df)
    print(paste('Finish the', as.character(i), 'iteration'))
    print(stri_dup("-",40))
  } else if (i %in% c(130)){
    print(stri_dup("-",40))
    print(paste('Start the', as.character(i), 'iteration'))
    print('rvest Bug Error, Fix by htmltab')
    df = url %>% 
      htmltab(130, rm_nodata_cols = F)
    df['iteration'] = i
    df['country_name'] = countries[i]
    colnames(df) <- tolower(colnames(df))
    colnames(df) <- gsub(' ', '_', colnames(df))
    colnames(df) <- gsub('/', '_', colnames(df))
    colnames(df) <- gsub('-', '_', colnames(df))
    colnames(df) <- gsub(" ", "", colnames(df), fixed = TRUE)
    df = df %>% mutate_all(as.character)
    print('Complete the handling of rvest bug')
    print(paste('Append to the data frame accumulator'))
    df_all = bind_rows(df_all, df)
    print(paste('Remove the', as.character(i), 'data'))
    rm(df)
    print(paste('Finish the', as.character(i), 'iteration'))
    print(stri_dup("-",40))
  } else{
    print(stri_dup("-",40))
    print(paste('Start the', as.character(i), 'iteration'))
    df = url %>%
      read_html() %>%
      html_nodes(xpath = xpathall[i]) %>%
      html_table(fill = TRUE)
    df = df[[1]]
    df['iteration'] = i
    df['country_name'] = countries[i]
    colnames(df) <- tolower(colnames(df))
    colnames(df) <- gsub(' ', '_', colnames(df))
    colnames(df) <- gsub('/', '_', colnames(df))
    colnames(df) <- gsub('-', '_', colnames(df))
    colnames(df) <- gsub(" ", "", colnames(df), fixed = TRUE)
    df = df %>% 
      mutate_all(as.character)
    print(paste('Append to the data frame accumulator'))
    df_all = bind_rows(df_all, df)
    print(paste('Remove the', as.character(i), 'data'))
    rm(df)
    print(paste('Finish the', as.character(i), 'iteration'))
    print(stri_dup("-",40))
  }
}

# in_circulation = in_circulation_since
df_all$in_circulation_since = ifelse(is.na(df_all$in_circulation_since), 
                                     df_all$in_circulation,
                                     df_all$in_circulation_since)
df_all$in_circulation <- NULL

# years_of_birth-death = years_of_birth_death
df_all$years_of_birth_death = ifelse(is.na(df_all$years_of_birth_death), 
                                     df_all$`years_of_birth-death`,
                                     df_all$years_of_birth_death)
df_all$`years_of_birth-death` <- NULL

# create a flag of 1 = 'Yes, banknotes are in circulation', 
# 0 = 'Banknotes are no longer in circulation' 
df_all$is_circulation = ifelse(is.na(df_all$years_of_circulation), 1, 0)

# check missing values per column
colSums(is.na(df_all))

# data dimensionality
print(paste('Banknotes data has', nrow(df_all), 'rows and', ncol(df_all), 'columns'))

# reorder column names
df_all = df_all[, c('iteration',
                    'country_name',
                    'currency',
                    'is_circulation',
                    'person',
                    'years_of_birth_death',
                    'in_circulation_since',
                    'years_of_circulation',
                    'reason_for_honor',
                    'denomination',
                    'obverse_or_reverse')]

# export the csv file
write_excel_csv(df_all, '../Playground/Bank_Notes-Wiki_Webscraping/output_bank_notes/wiki_bank_notes_data.csv')