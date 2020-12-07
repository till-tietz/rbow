
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbow

<!-- badges: start -->

<!-- badges: end -->

rbow allows you to run simple types of word frequency in context
analysis on multiple texts and dictionaries.  
It currently has implementations for two main types of analysis:  
   1. Frequency of occurrence of select terms (called descriptors in
rbow)  
       within some window around other terms (called phenomena in
rbow).  
   2. Frequency of all terms within some window around phenomena
terms.  
rbow further enables you to compute bootstrapped confidence intervals
for the frequency measures derived from analysis 1.

## Installation

You can install the development version of rbow from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("till-tietz/rbow")
```

## Usage

The following example will run through the type of analysis you might
wish to perform with rbow

``` r
library(rbow)

# create some text data to analyse
# you may wish to stem, remove stopwrods, remove punctuation, convert to lower case etc. in the process 
# of turning your texts into character vectors 
words <- c("I","you","he","she","it","we","they","boat","apple","orange","good",
           "bad","neutral","certain","certainty","positive","positivity","bold",
           "some","other","words","something","else","blue","green","red","hello",
           "this","why","uncertain")

text_1 <- sample(words, 10000, replace = TRUE)
text_2 <- sample(words, 20000, replace = TRUE)
text_3 <- sample(words, 30000, replace = TRUE)

texts <- list(text_1 = text_1, text_2 = text_2, text_3 = text_3)

# as rbow uses grep for matching you can use regular expression syntax
# create a list of phenomena terms
phenomena <- list(pronouns = c("I","you","he","she","it","we","they"),
                  nouns = c("boat","apple","orange"))

# create a list of descriptor terms 
descriptors <- list(moral = c("good","bad","neutral"),
                    confident = c("certain*","positiv*","bold"))
```

You can now analyze how often moral and confident descriptor terms occur
within the context of pronoun and noun phenomena.

``` r
bow_analysis <- rbow::bow_analysis(corpus = texts, phenomenon = phenomena, descriptors = descriptors,
                                   window = 5, per_occurrence = TRUE)
```

You can create bootstrap confidence intervals for these estimates like
this

``` r
future::plan("multisession")
cis <- rbow::bow_ci(bow_analysis_output = bow_analysis, bootstraps = 1000,
                    alpha = 0.95, window = 5, per_occurrence = TRUE, 
                    bootstrap_terms = TRUE)
```

You can now create a simple ci plot like this

``` r
plot_data <- rbow::create_plot_data(bstrap_output = cis)

# we now have a data frame of ggplot ready results for each text
# to plot the estimates and cis for text_1 simply call 

ci_plot <- rbow::ci_plot(plot_data = plot_data[[1]])
ci_plot
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

You can compute the frequency of all terms within some window around
phenomena terms like this

``` r
dfm <- rbow::dfm_analysis(corpus = texts, phenomenon = phenomena, window = 10, n_terms = 10)
head(dfm[[1]])
#> $pronouns
#>    analysis_text Freq
#> 1           good 2742
#> 2            she 2647
#> 3           bold 2612
#> 4      uncertain 2608
#> 5          apple 2605
#> 6             it 2601
#> 7           boat 2584
#> 8            why 2538
#> 9       positive 2498
#> 10          blue 2469
#> 
#> $nouns
#>    analysis_text Freq
#> 1        certain  771
#> 2            why  771
#> 3           good  770
#> 4            she  758
#> 5           bold  755
#> 6             it  745
#> 7       positive  729
#> 8           boat  719
#> 9          green  717
#> 10        orange  716
```

If you only want to consider certain types of words (i.e. adjectives or
adverbs) in your frequency analysis you can do the following

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
dfm_adj_adv <- rbow::dfm_analysis(corpus = texts, phenomenon = phenomena, window = 10, n_terms = 10, 
                                  filter_ps = TRUE, ps = c("Adjective","Adverb"))
head(dfm_adj_adv[[1]])
#> $pronouns
#>   analysis_text Freq
#> 1          good 2742
#> 3          bold 2612
#> 4     uncertain 2608
#> 8           why 2538
#> 9      positive 2498
#> 
#> $nouns
#>   analysis_text Freq
#> 1       certain  771
#> 2           why  771
#> 3          good  770
#> 5          bold  755
#> 7      positive  729
#> 9         green  717
```
