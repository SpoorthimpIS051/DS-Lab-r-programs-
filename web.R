library("dplyr")
library("rvest")
link="https://www.imdb.com/search/title/?groups=top_250&sort=user_rating"
page=read_html(link)
page
name= page %>% html_node(" .lister-item-header a") %>% html_text()
name
year=page %>% html_node(".lister-item-header a") %>% html_text()