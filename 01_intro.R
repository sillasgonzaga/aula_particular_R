library(tidyverse)


#### Intro super básica ####

## Como criar um objeto no R

x <- 1
x
print(x)
class(x)


## Tipos de objetos básicos no R


# character
x1 <- c("Sillas", "R")
x2 <- c("Sillas", 1)
# numeric e integer
x1 <- 1:20
x2 <- seq(1, 20, by = 2)
# logical
x1 <- TRUE
x2 <- FALSE
x1 == x2
!x1 == x2



## Data frames

# São um tipo de objeto usado para armazenar dados em formato tabular, semelhante a uma tabela.

df <- data.frame(
  x1 = c("a", "b", "c", "d"),
  x2 = c(10, 15, 20, 25)
)
df
str(df)



# Input de dados no R
?read.table


#Funcões como `read.csv` e `read.csv2` são wrapers de `data.table`.

df <- read.csv("https://raw.githubusercontent.com/sillasgonzaga/curso_redes_sociais/master/data/scielo_amostra.csv",
               stringsAsFactors = FALSE)

head(df)
glimpse(df)

#### Tidyverse ####

# https://www.tidyverse.org/

#### dplyr ####

# Principais funções:
# 
# * filter()  
# * `select()` e `rename()`
# * `mutate()` e `transmute()`  
# * `summarise()`  

### filter ###


df <- as.tibble(df)
?filter
filter(df, area == "Engineering")
filter(df, document_publishing_year >= 2017)
filter(df, area == "Engineering" & document_publishing_year >= 2017)


### arrange


?arrange
arrange(df, document_publishing_year)
arrange(df, desc(document_publishing_year))
arrange(df, journal)



### select


select(df, journal, author)
select(df, 2, 3)
select(df, area:author)
select(df, -journal)
select(df, -journal, -area)
select(df, starts_with("author"))
select_if(df, is.numeric)


### mutate
mutate(df, journal_lower = tolower(journal))
mutate(df, ytd = lubridate::year(Sys.Date()) - document_publishing_year)

extrair_sobrenome <- function(x){
  x <- strsplit(x, " ")[[1]]
  x[length(x)]
}

transmute(df, author, author_last_name = extrair_sobrenome(author))


### summarise
summarise(df, qtd_linhas = n())
summarise_if(df, is.numeric, mean)
summarise_if(df, is.character, n_distinct)

# com pipe
df %>% summarise(qtd_linhas = n())
df %>% summarise_if(is.numeric, mean)
df %>% summarise_if(is.character, n_distinct)

### group_by
df %>% 
  group_by(journal) %>% 
  summarise(qtd = n()) %>% 
  arrange(desc(qtd))


df %>% 
  group_by(document_id) %>% 
  summarise(qtd_autores = n_distinct(author)) %>% 
  arrange(desc(qtd_autores))

df %>% 
  group_by(area, author_institution) %>% 
  summarise(qtd = n_distinct(document_id)) %>% 
  group_by(area) %>% 
  mutate(tt_artigos = sum(qtd),
         pct_artigos = qtd/tt_artigos) %>% 
  filter(qtd == max(qtd)) %>% 
  arrange(desc(pct_artigos))



### joins
data("band_members")
data("band_instruments")
head(band_members)
head(band_instruments)

left_join(band_members, band_instruments, by = "name")
inner_join(band_members, band_instruments, by = "name")
right_join(band_members, band_instruments, by = "name")
semi_join(band_members, band_instruments, by = "name")
anti_join(band_members, band_instruments, by = "name")
full_join(band_members, band_instruments, by = "name")


#### tidyr ####

## gather()
data("USArrests")
?USArrests
us <- USArrests
head(us)
us <- rownames_to_column(us, "state")
us %>% gather(tipo_crime, valor, Murder:Rape) %>% head()
us.long <- us %>% gather(tipo_crime, valor, -state)

us.long %>% 
  group_by(tipo_crime) %>% 
  filter(valor == max(valor))

## spread
data(table2)
table2
table2 %>% spread(key = type, value = count)

## separate
head(df)
df %>% 
  separate(document_id, into = c("doc_id_prefixo", "doc_id"), sep = "-")

#### stringr ####
ls("package:stringr")
data("fruit")
fruit

str_c("a", "b", "c", sep = " ")
frutas <- c("apple", "banana", "pear", "pineapple")

nome_prof <- c("Sillas", "Fulano")
nome_aluno <- c("Rafael", "Cicrano")
str_glue("A aula será dada pelo professor {nome_prof} para o aluno {nome_aluno}")

str_to_lower(df$journal[1:5])
str_to_upper(df$journal[1:5])
str_to_title("o titulo DO LIVRO é o ATENEU")

str_pad(7, width = 3, pad = "0")
str_trim("    ESPAÇOS     INUTEIS           ")
str_squish("    ESPAÇOS     INUTEIS           ")
fruit
str_detect(fruit, "s")
fruit[str_detect(fruit, "s")]
str_subset(fruit, "s")
str_replace(fruit, "a|o", "@")
word(df$area[1:20], 1)

str_sub(fruit, start = 1, end = 3)
str_replace(df$document_id[1:20], ".*-", "")
str_replace(df$document_id[1:20], "-.*", "")

#### lubridate ####
library(lubridate)
ls("package:lubridate")
now()
now() %>% minute()
now() %>% hour()

x <- dmy(01091993)
x
class(x)
x == dmy("01-09-93")
x %>% day
x %>% month()
x %>% year()

today() %>% floor_date("week")
today() %>% floor_date("month")
today() %>% floor_date("6 months")

today() + ddays(2)
today() + dweeks(52)



#### ggplot2 ####

vetor_anos <- seq(min(df$document_publishing_year), 
                  max(df$document_publishing_year),
                  by = 1)


df %>% 
  rename(doc_year = document_publishing_year) %>% 
  filter(between(doc_year, 1999, 2017)) %>% 
  group_by(area, doc_year) %>% 
  summarise(qtd = n_distinct(document_id)) %>% 
  ggplot(aes(x = doc_year, y = area, fill = qtd)) + 
    geom_tile()

# incrementos: adicionar complete(), normalizar por area, retirar nome Sciences

min_max_norm <- function(x) {
  (x - min(x))/(max(x) - min(x))
}

df %>% 
  rename(doc_year = document_publishing_year) %>% 
  filter(between(doc_year, 1999, 2017)) %>% 
  mutate(area = str_replace_all(area, " Sciences", "")) %>% 
  group_by(area, doc_year) %>% 
  summarise(qtd = n_distinct(document_id)) %>% 
  ungroup() %>% 
  complete(area, doc_year, fill = list(qtd = 0)) %>% 
  # agrupar por area
  group_by(area) %>% 
  mutate(qtd_norm = min_max_norm(qtd)) %>% 
  ggplot(aes(x = doc_year, y = area, fill = qtd_norm)) + 
  geom_tile() +
  viridis::scale_fill_viridis() + 
  geom_text(aes(label = qtd), color = 'white')



