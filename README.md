# mini-project4
---
title: Avengers:Endgame, Worth the Hype?
subtitle: A brief overview of the budget and box office performance of the origin movies leading up to one of the most anticipated movie of 2019.
author: Lauren Forando, Sarah Daum, Lydia Ivanovic
output:
 html_document:
   code_folding: hide
   fig_width: 8
   fig_caption: true
   theme: cosmo
   df_print: paged
---
`r format(Sys.time(), "%a %b %d %X %Y")`

![](https://media.giphy.com/media/WbNqQbrnAGr5e/giphy.gif)

```{r setup, include=FALSE}
library(tidyverse)
library(devtools)
library(RMySQL)
library(vembedr)
library(sqldf)
library(dbplyr)
db <- dbConnect(MySQL(), 
                host = "scidb.smith.edu", 
                user = "mth292", 
                password = "RememberPi", 
                dbname = "imdb")
knitr::opts_chunk$set(connection = db, max.print = 20)
```

```{r, include=FALSE}
db %>%
  dbGetQuery("SELECT *
FROM movie_info
LIMIT 0, 5;")
```

```{r}
gross_table <-db %>%
  dbGetQuery("SELECT t.title AS Movie_Title, t.production_year AS Year, m.info AS gross
FROM title t
JOIN movie_info m ON m.movie_id = t.id
LEFT JOIN info_type i ON i.info = m.info_type_id
WHERE title IN ('Iron Man', 'Iron Man 2', 'The Incredible Hulk', 'Thor', 'Captain America: The First Avenger', 'The Avengers', 'Iron Man 3','Thor: The Dark World','Captain America: The Winter Soldier', 'Guardians of the Galaxy', 'Avengers: Age of Ultron', 'Ant-Man', 'Captain America: Civil War', 'Doctor Strange')
AND kind_id = 1
AND info_type_id = 107
AND production_year > 2000
GROUP BY movie_id
ORDER BY gross DESC;") %>%
  filter(grepl('\\(USA)', gross)) %>%
  mutate(gross = gsub('\\s.*', '', gross))

gross_table
```


```{r, include = FALSE}
#Movies behind the Avengers Franchise organized through overall gross (USA).
gross_data <-db %>%
  dbGetQuery("SELECT t.title AS Movie_Title, t.production_year AS Year, m.info AS gross
FROM title t
JOIN movie_info m ON m.movie_id = t.id
LEFT JOIN info_type i ON i.info = m.info_type_id
WHERE title IN ('Iron Man', 'Iron Man 2', 'The Incredible Hulk', 'Thor', 'Captain America: The First Avenger', 'The Avengers', 'Iron Man 3','Thor: The Dark World','Captain America: The Winter Soldier', 'Guardians of the Galaxy', 'Avengers: Age of Ultron', 'Ant-Man', 'Captain America: Civil War', 'Doctor Strange')
AND kind_id = 1
AND info_type_id = 107
AND production_year > 2000
GROUP BY movie_id
ORDER BY gross DESC;") %>%
  filter(grepl('\\$', gross)) %>%
  filter(grepl('\\(USA)', gross)) %>%
  mutate(gross = gsub('\\s.*', '', gross)) %>%
  mutate(gross = gsub('\\D', '', gross)) %>%
  mutate(gross = as.numeric(gross))

gross_data
```

[![Marvel Studios' Avengers: Endgame](https://i.kinja-img.com/gawker-media/image/upload/s--3ClQMwPu--/c_fill,fl_progressive,g_center,h_900,q_80,w_1600/lnuyy61l5oz6x2x3odrg.jpg)](https://youtu.be/sOgwPtVWMhE "Marvel Studios' Avengers: Endgame - Click to Watch!")
Marvel Studios' Avengers: Endgame - Trailer

###Movies behind the Avengers Franchise organized by overall budget.
```{sql, connection = db}
SELECT t.title AS Movie_Title, t.production_year AS Year, m.info AS Production_Cost
FROM title t
JOIN movie_info m ON m.movie_id = t.id
LEFT JOIN info_type i ON i.info = m.info_type_id
WHERE title IN ('Iron Man', 'Iron Man 2', 'The Incredible Hulk', 'Thor', 'Captain America:The First Avenger', 'The Avengers','Thor:The Dark World', 'Guardians of the Galaxy', 'Captain America:The Winter Soldier','Avengers:Age of Ultron', 'Ant-Man', 'Captain America: Civil War', 'Doctor Strange')
AND kind_id = 1
AND info_type_id = 105
AND production_year > 2000
GROUP BY movie_id
ORDER BY Production_Cost DESC;
```


```{r, include = FALSE}
Production_data <- db %>%
  dbGetQuery("SELECT t.title AS Movie_Title, t.production_year AS Year, m.info AS Production_Cost
FROM title t
JOIN movie_info m ON m.movie_id = t.id
LEFT JOIN info_type i ON i.info = m.info_type_id
WHERE title IN ('Iron Man', 'Iron Man 2', 'The Incredible Hulk', 'Thor', 'Captain America:The First Avenger', 'The Avengers','Thor:The Dark World', 'Guardians of the Galaxy', 'Captain America:The Winter Soldier','Avengers:Age of Ultron', 'Ant-Man', 'Captain America: Civil War', 'Doctor Strange')
AND kind_id = 1
AND info_type_id = 105
AND production_year > 2000
GROUP BY movie_id
ORDER BY Production_Cost DESC;") %>%
  filter(grepl('\\$', Production_Cost)) %>%
  mutate(Production_Cost = gsub('\\s.*', '', Production_Cost)) %>%
  mutate(Production_Cost = gsub('\\D', '', Production_Cost)) %>%
  mutate(Production_Cost = as.numeric(Production_Cost))

Production_data
```

```{r}
library(ggplot2)
comparison_data <- gross_data %>%
  full_join(Production_data, by = c("Movie_Title", "Year")) %>%
  mutate(revenue = gross - Production_Cost)
```

```{r, include=FALSE}
comparison_data_tidy <- gather(comparison_data,
                   key = "money_type",
                   value = "amount",
                   gross:revenue,
                   -Movie_Title, -Year)
comparison_data_tidy
```

```{r, message = FALSE, warning=FALSE}
#Money Plot
money_plot <- ggplot(comparison_data_tidy, aes(y = amount, x = factor(Movie_Title, levels = c('Iron Man', 'The Incredible Hulk', 'Iron Man 2', 'Thor', 'The Avengers', 'Guardians of the Galaxy', 'Ant-Man', 'Captain America: Civil War', 'Doctor Strange')), fill = money_type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  geom_point(aes(1, 98600000), col = "purple", size = 3) +
  geom_point(aes(3, 128100000), col = "purple", size = 3) +
  geom_point(aes(4, 65700000), col = "purple", size = 3) +
  geom_point(aes(5, 207400000), col = "purple", size = 3) +
  geom_point(aes(6, 94300000), col = "purple", size = 3) +
  geom_point(aes(7, 57200000), col = "purple", size = 3) +
  geom_point(aes(8, 179100000),col = "purple", size = 3) +
  geom_point(aes(9, 85100000), col = "purple", size = 3) +
  # scale_fill_manual(name="Money",
  #                     values=c(gross = "#56B4E9", Production_Cost = "#CC6666", revenue = "forest green")) +
  ylab("Costs") +
  xlab("Movies") +
  ggtitle("The Lucrative History of 'The Avengers'") +
  theme_classic()
money_plot
```

Opening night box office statistics.^[https://www.ign.com/articles/2019/04/30/how-much-each-marvel-cinematic-universe-movie-made-its-opening-weekend]

```{r}
# production_plot <- ggplot(comparison_data2, aes(y = Production_Cost, x = factor(Movie_Title, levels = c('Iron Man', 'The Incredible Hulk', 'Iron Man 2', 'Thor', 'The Avengers', 'Guardians of the Galaxy', 'Ant-Man', 'Captain America: Civil War', 'Doctor Strange')))) +
#   geom_bar(stat = "identity") + 
#   ylab("Total Production Cost") +
#   xlab("Movies")
#   ggtitle("Production Cost Timeline") 
# production_plot
```

```{r}
# production_plot <- ggplot(comparison_data2, aes(y = revenue, x = factor(Movie_Title, levels = c('Iron Man', 'The Incredible Hulk', 'Iron Man 2', 'Thor', 'The Avengers', 'Guardians of the Galaxy', 'Ant-Man', 'Captain America: Civil War', 'Doctor Strange')))) +
#   geom_bar(stat = "identity") + 
#   ylab("Total Revenue") +
#   xlab("Movies") +
#   ggtitle("Raking it in") 
# production_plot
```

![](https://media.giphy.com/media/Ajyi28ZdneUz6/giphy.gif)

Want to see the data for yourself? Check out the link to our GitHub repository!^[git@github.com:lforando/mini-project4.git]
