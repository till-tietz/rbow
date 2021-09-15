
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbow

<!-- badges: start -->

[![R-CMD-check](https://github.com/till-tietz/rbow/workflows/R-CMD-check/badge.svg)](https://github.com/till-tietz/rbow/actions)
<!-- badges: end -->

rbow allows you to analyze texts with a view towards relevant terms,
their contexts and associations among each other. It can can run word
frequency in context analyses on multiple texts and dictionaries.

rbow currently has implementations for three main types of analysis:

1.  Deductive approach: allows you to analyze frequency of co-occurrence
    of two sets of terms (phenomena and descriptors). You may wish to
    use this feature in order to test hypotheses as to the manner in
    which phenomena are commonly described.

2.  Inductive approach: allows you to analyze frequency of terms
    associated with a set of terms. You may wish to use this feature for
    exploratory analysis about how phenomena are commonly described

3.  Term Frequency Inverse Document Frequency (TF-IDF) Analysis. You may
    wish to use this feature to identify texts that are most relevant to
    a chosen set of terms.

rbow further enables you to compute bootstrapped confidence intervals
for the frequency measures derived from analysis 1, and plot your
results. It also includes some utility functions for text cleaning and
stemming.

## Installation

You can install the development version of rbow from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("till-tietz/rbow")
```

## Usage

The following example, while not at all an interesting/principled
analysis, will nonetheless hopefully give a decent overview of the types
of analyses rbow supports and you may wish to perform.

``` r
library(rbow)
library(tidyverse)

#lets get some data to analyze. we'll be using the corpus of jane austen novels from 
#janeaustenr (because why not)

#install.packages("janeaustenr")
library(janeaustenr)

books <- janeaustenr::austen_books()

#lets transform our data such that we have a data.frame of book names and each book's text 
#as a single character string 
books <- books%>%
  dplyr::group_by(book)%>%
  dplyr::mutate(text = paste(text, collapse = " "))%>%
  dplyr::slice(.,1)%>%
  dplyr::ungroup()%>%
  dplyr::mutate_at(.,2, as.character())%>%
  tibble::column_to_rownames(., var = "book")

#as rbow operates on lists we'll transform our data.frame into a list 
books <- setNames(split(books, seq(nrow(books))), rownames(books))

#now we'll transform each book's text from a character string to a character vector
#(i.e we'll tokenize each book)
books <- lapply(books, function(x)strsplit(x[,1]," ")[[1]])

#you may wish to run rbow's utility functions for text cleaning and stemming at this point
#clean_text will turn your tokens to lower case, remove stopwords, symbols, numbers etc. 
#stem_texts stems your tokens
books <- rbow::clean_text(texts = books, rm_stopwords = TRUE, stopwords_language = "en")
books <- rbow::stem_texts(texts = books, language = "english")

#now let's define some phenomena we want to analyze. we might wish to know whether 
#men and women are described/regarded differently in jane austen's work.
#we'll define a list of terms capturing men and women
#you can supply your own regex and set own_regex to TRUE in the analysis functions
#or let rbow construct a default regex for you (* overrides the word end boundary)
phenomena <- list(female = c("mrs","ms","miss","she","her","lady"),
                  male = c("mr","sir","he","him","lord"))

phenomena <- rbow::stem_texts(texts = phenomena, language = "english")

#let's create a set of descriptor terms to deductively test some hypothesis about how descriptions of men and women differ in jane austen's work 
descriptors <- list(positive = c("ador*","affection*","appreciat*","cheer*","content*","deligh*","ecsta*","enjoy*","fondness","glad","happy","hope","joy","love","loves","lovin"),
                    anxiety = c("araid","anxi*","apprehens*","doom","dread*","fear*","fright","nervous","panic","paranoi*","petrif*","phobi*","scare*","scary","terrifi*","terrify*"),
                    anger = c("aggravat*","anger","angr*","annoy*","appall*","contempt","despis*","frustrat","fury","furious","hate*","mad","resent"),
                    sadness = c("aline","anguish*","apath*","bitter","crushed","depress*","despair","disappoint*","grief","griev*","heartbreak*","helpless","hopeless","loss","sad","melanchol*","sorrow"))
```

You can now analyze how frequently positive, anxious, angry or sad
descriptors occur within some window around male or female words
i.e. whether male or female words are relatively more frequently
associated with these four descriptors.

``` r
bow_analysis <- rbow::bow_analysis(corpus = books, phenomenon = phenomena, descriptors = descriptors,
                                   window = 10, per_occurrence = TRUE, own_regex = FALSE)
```

You can create bootstrap confidence intervals for these estimates like
this

``` r
future::plan("multisession")
cis <- rbow::bow_ci(bow_analysis_output = bow_analysis, bootstraps = 1000,
                    alpha = 0.95, window = 10, per_occurrence = TRUE, 
                    bootstrap_terms = TRUE)
```

and create a simple ci plot

``` r
plot_data <- rbow::create_plot_data(bstrap_output = cis)

# we now have a data frame of ggplot ready results for each text
# to plot the estimates and cis for text_1 simply call 

rbow::ci_plot(plot_data = plot_data[[1]])
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r

# you can plot subsets of phenomena and descriptors like this 

plot_data <- rbow::create_plot_data(bstrap_output = cis,
                                    phenomena = c("female"),
                                    descriptors = c("positive","anxiety"))

rbow::ci_plot(plot_data = plot_data[[1]])
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

You may also wish to explore how female and male words are commonly
described in Jane Austen’s work inductively. dfm\_analysis caputres the
most frequently used terms within some window of your phenomena terms.

``` r
dfm <- rbow::dfm_analysis(corpus = books, phenomenon = phenomena, window = 10,
                          n_terms = 10, own_regex = FALSE)
head(dfm[[1]])
#> $female
#>         Var1 Freq
#> 1   dashwood  333
#> 2        jen  320
#> 3          s  308
#> 4     elinor  192
#> 5        mrs  182
#> 6  middleton  168
#> 7       said  159
#> 8    mariann  153
#> 9     ferrar  123
#> 10    sister  111
#> 
#> $male
#>          Var1 Freq
#> 1        john  139
#> 2           s  116
#> 3         mrs   68
#> 4      palmer   63
#> 5        said   63
#> 6      ferrar   56
#> 7    dashwood   55
#> 8      elinor   53
#> 9  willoughbi   51
#> 10       know   47
```

You can extract words that are unique to the context of your phenomena
terms by computing tf-idf instead of raw frequencies

``` r
dfm <- rbow::dfm_analysis(corpus = books, phenomenon = phenomena, window = 10,
                          n_terms = 10, tf_idf = TRUE ,own_regex = FALSE)
head(dfm[[1]])
#> $female
#>           Var1       tf-idf
#> 1115       jen 0.0120285845
#> 1321       mrs 0.0068412574
#> 1291      miss 0.0039468793
#> 1145      ladi 0.0031950928
#> 1313    morton 0.0009021438
#> 645      elder 0.0004886612
#> 673     enforc 0.0002631253
#> 1102 introduct 0.0001879466
#> 1286     mirth 0.0001879466
#> 1872     spark 0.0001879466
#> 
#> $male
#>        Var1       tf-idf
#> 843      mr 0.0042394323
#> 1171    sir 0.0022257020
#> 983   pratt 0.0013778155
#> 766    lord 0.0007419007
#> 260  conjur 0.0003179574
#> 612   henri 0.0003179574
#> 47    allus 0.0002119716
#> 223   clerk 0.0002119716
#> 421      em 0.0002119716
#> 547    fuss 0.0002119716
```

If you additionally only want to consider certain types of words
(i.e. adjectives or adverbs) in your frequency analysis you can do the
following

``` r
#look at types of words to filter by 
unique(tidytext::parts_of_speech[,"pos"])
#> # A tibble: 14 x 1
#>    pos                  
#>    <chr>                
#>  1 Adjective            
#>  2 Noun                 
#>  3 <NA>                 
#>  4 Plural               
#>  5 Adverb               
#>  6 Preposition          
#>  7 Verb (transitive)    
#>  8 Verb (usu participle)
#>  9 Verb (intransitive)  
#> 10 Interjection         
#> 11 Noun Phrase          
#> 12 Conjunction          
#> 13 Definite Article     
#> 14 Pronoun


#display only adjectives and adverbs
dfm <- rbow::dfm_analysis(corpus = books, phenomenon = phenomena, window = 10, n_terms = 10,
                                  tf_idf = TRUE, filter_ps = TRUE, ps = c("Adjective","Adverb"),
                                  own_regex = FALSE)
head(dfm[[1]])
#> $female
#>            Var1       tf-idf
#> 645       elder 4.886612e-04
#> 1590     putrid 1.127680e-04
#> 2216   westward 1.127680e-04
#> 1063     infect 7.517865e-05
#> 1270    merrier 7.517865e-05
#> 1537  prettiest 7.517865e-05
#> 2036 throughout 7.517865e-05
#> 2245  withdrawn 7.517865e-05
#> 97        amiss 3.758933e-05
#> 262       brave 3.758933e-05
#> 
#> $male
#>              Var1       tf-idf
#> 82           arch 0.0001059858
#> 145        bigger 0.0001059858
#> 149         black 0.0001059858
#> 163         brave 0.0001059858
#> 362 disinterested 0.0001059858
#> 540   friendliest 0.0001059858
#> 616        hinder 0.0001059858
#> 778           mad 0.0001059858
#> 815           mid 0.0001059858
#> 939         pearl 0.0001059858
```

You can finally subset the output of dfm\_analysis by another
dictionary/set of terms

``` r
dfm <- rbow::dfm_analysis(corpus = books, phenomenon = phenomena, window = 10, n_terms = 10,
                                  tf_idf = TRUE, filter_ps = TRUE, ps = c("Adjective","Adverb"),
                                  filter_dictionary = descriptors[[4]], own_regex = FALSE)
head(dfm[[4]])
#> $female
#>          Var1 tf-idf
#> 246    bitter      0
#> 1015 grievous      0
#> 1913      sad      0
#> NA       <NA>     NA
#> NA.1     <NA>     NA
#> NA.2     <NA>     NA
#> NA.3     <NA>     NA
#> NA.4     <NA>     NA
#> NA.5     <NA>     NA
#> NA.6     <NA>     NA
#> 
#> $male
#>          Var1       tf-idf
#> 983  grievous 0.0001128904
#> 239    bitter 0.0000000000
#> 1040 helpless 0.0000000000
#> 1878      sad 0.0000000000
#> NA       <NA>           NA
#> NA.1     <NA>           NA
#> NA.2     <NA>           NA
#> NA.3     <NA>           NA
#> NA.4     <NA>           NA
#> NA.5     <NA>           NA
```

If you simply wish to find out which text is most relevant to a certain
dictionary you can use rbow’s implementation of tf-idf

``` r
pride_and_prejudice_names <- c("bingley","bennet","darcy")

tf_idf <- rbow::tf_idf(corpus = books, terms = pride_and_prejudice_names)

head(tf_idf)
#>                   doc      tf.idf
#> 2   Pride & Prejudice 0.004277573
#> 1 Sense & Sensibility 0.000000000
#> 3      Mansfield Park 0.000000000
#> 4                Emma 0.000000000
#> 5    Northanger Abbey 0.000000000
#> 6          Persuasion 0.000000000
```
