Laboratorio#7
================
Isaac Cyrman
2024-10-20

# Librerias y carga de datos

``` r
library(readr)  
library(dplyr)  
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)  
library(lubridate)  
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(stopwords)  
library(wordcloud)  
```

    ## Loading required package: RColorBrewer

``` r
product_reviews <- read_csv("Health_and_Personal_Care.csv")  
```

    ## Rows: 494121 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): title, text, product_id, parent_id, user_id
    ## dbl (2): rating, timestamp
    ## lgl (1): verified_purchase
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
product_reviews$text <- str_replace_all(product_reviews$text, '\\"', '')  

product_metadata <- read_csv("Health_and_Personal_Care_metadata.csv")  
```

    ## Rows: 60293 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): main_category, title, store, details, parent_id
    ## dbl (3): average_rating, rating_number, price
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# 1) Cuántos productos contienen reviews con las palabras “love”, “recommend” y “enjoy”?

``` r
keywords <- c("love", "recommend", "enjoy")  
positive_pattern <- paste(keywords, collapse = '|')  

positive_reviews <- product_reviews %>%  
  filter(str_detect(text, positive_pattern)) %>%  
  distinct(product_id) %>%  
  summarise(total_positive_products = n())  

positive_reviews  
```

    ## # A tibble: 1 × 1
    ##   total_positive_products
    ##                     <int>
    ## 1                   23180

# 2) De los reviews de la pregunta 1, encuentre el top 5 de las tiendas que los venden?

``` r
filtered_reviews <- product_reviews %>%  
  filter(str_detect(text, positive_pattern)) %>%  
  distinct(product_id, parent_id)  

top_stores <- filtered_reviews %>%  
  inner_join(product_metadata, by = "parent_id") %>%  
  filter(!is.na(store)) %>%  
  count(store, name = "total_products") %>%  
  arrange(desc(total_products)) %>%  
  top_n(5) %>%  
  select(store)  
```

    ## Selecting by total_products

``` r
top_stores  
```

    ## # A tibble: 5 × 1
    ##   store      
    ##   <chr>      
    ## 1 HAARBB     
    ## 2 Eyekepper  
    ## 3 US Organic 
    ## 4 Andaz Press
    ## 5 Generic

# 3) Genere un wordcloud sin stopwords de los reviews de la pregunta 1.

``` r
positive_texts <- product_reviews %>%  
  filter(str_detect(text, positive_pattern)) %>%  
  distinct(product_id, text) %>%  
  select(text)  

stop_words_list <- c(stopwords("en"), stopwords("es"))  
word_list <- str_split(positive_texts$text[1:100], boundary("word")) %>% unlist()  

filtered_words <- tibble(word = word_list) %>%  
  filter(!word %in% stop_words_list) %>%  
  count(word, name = "frequency")  

wordcloud(filtered_words$word, filtered_words$frequency)  
```

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B08KXWKDDM could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): associated
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): material
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): accurate
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): almost
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): beautiful
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): This could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): expensive
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): system
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): shower
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): immune
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): must could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): house
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): case could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): instead
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): received
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): online
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## rechargeable could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): foot could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): priced
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): indicator
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B08NT3QPMM could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): money
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): skin could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): washer
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): The could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): drying
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): smoothie
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): really
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): stores
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): basic
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B081HCZPY2 could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): handle
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): regular
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): speaking
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): blue could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): assist
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## designation could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): empty
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): bath could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): reminder
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): TENS could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Amazon
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): beyond
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Home could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): previously
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): enjoy
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Series
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): support
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B09KXD6SNP could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): bought
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): upside
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): unit could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): 198 could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): quality
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): leave
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): averaged
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): well could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): entire
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): either
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): kept could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): standard
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): battery
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): faceplate
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): effects
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): prefer
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): essential
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): right
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): gold could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): made could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): else could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): setting
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): set could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): know could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): hands
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): probiotic
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): come could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): diastolic
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Since
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): European
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): happens
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): per could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): You could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Epsom
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): item could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Famidoc
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B08QH16BVY could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): sized
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): After
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): record
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): natural
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): options
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): get could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): hour could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): irregular
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): texture
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): included
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): a.k.a
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): decent
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): increase
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): never
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): packaged
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): br could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): turn could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): users
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): color
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): though
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): tried
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): helps
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): notation
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): neck could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): June could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B0995WJ42M could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): adapter
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): unique
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): crashing
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): reach
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): two could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): however
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): batteries
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): clean
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): throw
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): button
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Two could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): automated
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): center
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): floor
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Date could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Digital
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): 9.99 could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): think
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): cycle
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): alarm
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): power
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): simply
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): voice
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): moderated
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## inflammation could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): taken
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Press
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Batteries
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B08DTWKYC9 could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): LEFT could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): cheaper
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B09JYND7QX could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): fit could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): another
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): sometimes
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): rings
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): remembered
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): piece
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): pain could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): area could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): timer
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): recommend
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## instruction could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Note could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): disrobe
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): results
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): bomb could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): monitor
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): personally
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): side could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Accuracy
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): apply
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): scents
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## portability could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): NOT could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Precision
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): User could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): heart
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): arm could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): used could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): stored
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Overall
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): 4663 could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): connect
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): spot could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B09FSNLQ21 could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): bottom
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): home could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): something
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): keychain
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): star could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): hope could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Multi
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): switch
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): issues
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): You'll
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Backagin
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): process
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): design
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): try could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Wrist
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): C.P could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): number
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): broadcast
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): atypical
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): negative
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): wrapped
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): indicates
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): able could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): price
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): reasonably
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): back could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B08HK71G16 could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): sweet
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): indicating
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Timer
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): written
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): videos
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): least
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## individuals could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): adjustable
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): trying
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): good could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): separately
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## Unfortunately could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): days could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): ankle
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): flavor
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): heavy
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): drawbacks
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): needs
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): provided
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): wash could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): 16.535
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): AVERAGED
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): brush
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): different
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): IBS could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): found
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): manual
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): forget
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): anyone
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): works
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): feet could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): goods
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): healthy
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): difference
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Type could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): hair could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## individually could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): typical
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): take could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): easy could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): operated
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): items
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): worth
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## Nevertheless could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): Most could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): fragrance
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): water
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): light
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): THE could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): comes
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): winter
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): charging
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency):
    ## ASIN:B09GVR6X6W could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): hold could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): reminders
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): bottle
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(filtered_words$word, filtered_words$frequency): just could
    ## not be fit on page. It will not be plotted.

![](Laboratorio-7_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# 4) Generar un wordcloud de los reviews de las 5 tiendas encontradas en la pregunta. Deberá de incluir todos los reviews de esas 5 tiendas.

``` r
selected_stores <- top_stores$store 

store_parents <- product_metadata %>%  
  filter(store %in% selected_stores) %>%  
  select(parent_id)

store_reviews <- product_reviews %>%  
  inner_join(store_parents, by = "parent_id") %>%  
  select(text)

word_list_store <- store_reviews$text %>% 
  head(100) %>%  
  str_split(boundary("word")) %>% 
  unlist()

filtered_words_store <- tibble(word = word_list_store) %>%  
  filter(!word %in% stop_words_list) %>%  
  count(word, name = "frequency")

wordcloud(filtered_words_store$word, filtered_words_store$frequency)
```

![](Laboratorio-7_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# 5) Cuáles son las 25 palabras más frecuentes de los reviews?

``` r
all_words <- str_split(product_reviews$text, boundary("word")) %>% unlist()  

filtered_words_general <- tibble(word = all_words) %>%  
  filter(!word %in% stop_words_list) %>%  
  count(word, name = "frequency") %>%  
  arrange(desc(frequency))  

top_25_words <- filtered_words_general %>%  
  top_n(25, frequency)  

top_25_words  
```

    ## # A tibble: 25 × 2
    ##    word    frequency
    ##    <chr>       <int>
    ##  1 I          590664
    ##  2 br         136733
    ##  3 product    100609
    ##  4 The         94872
    ##  5 It          81421
    ##  6 use         80636
    ##  7 like        76352
    ##  8 great       71485
    ##  9 This        69662
    ## 10 one         64936
    ## # ℹ 15 more rows
