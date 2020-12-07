
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbow

<!-- badges: start -->

<!-- badges: end -->

rbow allows you to run simple types of document frequency in context
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
#> analysis_text
#> positivity        she        why       they       this  certainty      words 
#>       2868       2761       2679       2673       2624       2585       2583 
#>    neutral       bold        bad 
#>       2577       2551       2545 
#> 
#> $nouns
#> analysis_text
#>       this positivity        she       blue      apple  certainty      words 
#>        735        734        729        704        701        695        694 
#>  uncertain        bad       else 
#>        680        678        673
```