library(tidyverse)
# read_csv not only reads csv file in storage, also reads input itself
read_csv("The first line of metadata
  The second line of metadata
         x,y,z
         1,2,3", skip = 2)

read_csv("# A comment I want to skip
  x,y,z
         1,2,3", comment = "#") # skip line that starts with comment

read_csv("1,2,3\n4,5,6", col_names = FALSE) # no column names

read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

# specifies the value (or values) that are used to represent missing values in your file:

read_csv("a,b,c\n1,2,.", na = ".")

read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")


# parse_*() functions
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))

# if you have problem use problems()

# some countries use , as decimal mark not . 
parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))

# parse_number() ignore non-numeric characters before and after numbers
parse_number("$100")
parse_number("20%")
parse_number("It costs $123.45")

# grouping mark
parse_number("$123,456,789")
parse_number("123.456.789", locale = locale(grouping_mark = "."))
parse_number("123'456'789", locale = locale(grouping_mark = "'"))

charToRaw("Hadley")
# parse character
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit) # no bananana in fruit!

# parse_datetime() expects an ISO8601 date-time. 
#ISO8601 is an international standard in which the components of a date are organised from biggest to smallest: year, month, day, hour, minute, second.

parse_datetime("2010-10-01T2010")
parse_datetime("20101010")

parse_date("2010-10-01") # expects a four digit year, a - or /, the month, a - or /, then the day

library(hms)
parse_time("01:10 am")
parse_time("20:10:01")
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")

# french month reading
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))


# readr is not perfect, it predict type of column by reading first 1000 rows. If there is error about column type, fix yourself like below
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_integer(),
    y = col_character()
  )
)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)

tail(challenge)
# y is date in tail
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)

# strategy 2 for fixing type problem
challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
challenge2
challenge2 <- read_csv(readr_example("challenge.csv"), 
                       col_types = cols(.default = col_character()))

df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df

# Note the column types
type_convert(df) #  applies the parsing heuristics to the character columns in a data frame
 
