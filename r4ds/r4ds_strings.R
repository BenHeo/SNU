library(tidyverse)
library(stringr)

x <- c("\"", "\\")
writeLines(x)

# non-english characters
(x <- "\u00b5")


## str_  Series
# length of strings
str_length(c("a", "R for data science", NA))
# combine strings
str_c("x", "y")
str_c("x", "y", sep = ", ") # sep and collapse work differently
# replace NA
x <- c("abc", NA)
str_c("|-", x, "-|") # NA is not considered
re_x <- str_replace_na(x) # replace NA with "NA"
str_c("|-", re_x, "-|")  # str_c() is vectorised, and it automatically recycles shorter vectors to the same length as the longest

# fun
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE
str_c("Good ", time_of_day, " ", name,
      if (birthday) " and Happy Birthday!")
birthday <- TRUE
str_c("Good ", time_of_day, " ", name,
      if (birthday) " and Happy Birthday!")

# str_sub(x, start, end)   # (inclusive) position
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
# it will just return as much as possible
str_sub("a", 1, 10)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x,1,1)); x # useful if combine other function

# locale
str_to_upper(c("i", "Ä±")) # no difference
str_to_upper(c("i", "Ä±"), locale = "tr") # Turkish

x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")  # English   # return sorted string vector
str_sort(x, locale = "haw") # Hawaiian
strOrd <- str_order(x, locale = "en") # return order
x[strOrd] # same as str_sort(x)
# search str_wrap and str_trim, too

## regulex
x <- c("apple", "banana", "pear")
str_view(x, "an") # Oooooooh----oooohhhhhh
str_view(x, ".a.") # . means some character exist
str_view(c("abc", "a.c", "bef"), "a\\.c") # to match real character .(dot)  # to create the regular expression \. we need the string "\\."
x <- "a\\b"
str_view(x, "\\\\")
#  \ is not same as "\", "\\", "\\\"

# anchors
# ^ to match the start of the string.
# $ to match the end of the string.
x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

v = "feaoihfi$^$fe"
writeLines(v)
str_view(v, "\\$\\^\\$")

words %>%
  str_view("^y", match = TRUE) 
words %>%
  str_view("x$", match = TRUE)
words %>% 
  str_view("^...$", match = TRUE) # exactly three letters long
words %>%
  str_view(".......", match = TRUE) # seven or more letters long

# \d: matches any digit.
# \s: matches any whitespace (e.g. space, tab, newline).
# [abc]: matches a, b, or c.
# [^abc]: matches anything except a, b, or c.
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]") # highlight one empty space exists

str_view(c("abc", "deaf", "soap", "cars"), "abc|d..f") # it is for abc or d..f,  not for abc..f or abd..f
str_view(c("grey", "gray"), "gr(e|a)y") # use parenthesis if you want like this

# ?: 0 or 1
# +: 1 or more
# *: 0 or more
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, "C[LX]+")

# {n}: exactly n
# {n,}: n or more
# {,m}: at most m
# {n,m}: between n and m
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
# ? is lazy, greedy 
str_view(x, "C{2,3}?")
str_view(x, 'C[LX]+?')

str_view(fruit, "(..)\\1", match = TRUE)


# str_detect: TRUE or FALSE
x <- c("apple", "banana", "pear")
str_detect(x, "e")
sum(str_detect(words, "^t")) # How many common words start with t?
mean(str_detect(words, "[aeiou]$")) # What proportion of common words end with a vowel?

words[str_detect(words, "x$")] # first detect it True or False
str_subset(words, "x$") # Then apply

x <- c("apple", "banana", "pear")
str_count(x, "a")
# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))

df <- tibble(
  word = words, 
  i = seq_along(word) # return row number
)

df %>% 
  filter(str_detect(words, "x$"))

df %>% 
  mutate(
    vowels = str_count(words, "[aeiou]"),
    consonants = str_count(words, "[^aeiou]")
  )

# matches never overlap!!
str_count("abababa", "aba")
str_view("abababa", "aba") # it shows one thing and break
str_view_all("abababa", "aba")

# let's use sentences data
length(sentences)
head(sentences)
colors <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <- str_c(colors, collapse = "|")
color_match # did it for regex
has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match) # extracts everything that first matches
head(matches)

more <- sentences[str_count(sentences, color_match) > 1]
str_view_all(more, color_match)
str_extract_all(more, color_match) # it returns list
str_extract_all(more, color_match, simplify = TRUE) # return matrix


noun <- "(a|the) ([^ ]+)" # look white space carefully between () and ([])

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>% 
  str_extract(noun)
has_noun %>% 
  str_match(noun) # first column for complete match string, rests are for each string that matches


# extract is for tibble
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", 
    remove = FALSE
  )
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three")) # can do like this

sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%  # used backreference ==> second and third word changed 
  head(5)

sentences %>%
  head(5) %>% 
  str_split(" ") # return list

sentences %>%
  head(5) %>%
  str_split(" ", simplify = TRUE) # return data.frame

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE) # n means maximum number of pieces

# Instead of splitting up strings by patterns, you can also split up by character, line, sentence and word boundary()s:
x <- "This is a sentence.  This is another sentence."
str_view_all(x, boundary("word"))

str_view(fruit, "nana")
# Is shorthand for
str_view(fruit, regex("nana")) # so same things are returned

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE)) # ignore cases(upper, lower, capital, ......) # don't forget regex()

x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]] # multiline accepted

apropos("replace") # searches all objects available from global environment. useful when can¡¯t remember name of function
head(dir(pattern = "\\.Rmd$")) # lists all the files in a directory, only returns file names that match the pattern