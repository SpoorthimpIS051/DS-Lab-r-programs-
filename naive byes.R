library("dplyr")
library("stringr")
library("rsample")
library("yardstick")
set.seed(2417)
spam <- readr::read_csv("spam.csv")
spam
spam <- spam %>%
  tidyr::unite(col = "msg", 2:5, sep = " ", na.rm = TRUE) %>% 
  rename("label" = v1)
split <- rsample::initial_split(spam, strata = label)
train_spam <- rsample::training(split)
test_spam <- rsample::testing(split)
prop.table(table(train_spam$label))
prop.table(table(test_spam$label))
string_cleaner <- function(text_vector) {
  tx <- text_vector %>%
    str_replace_all("[^[:alnum:] ]+", "") %>%
    str_to_lower() %>%
    str_replace_all("\\b(http|www.+)\\b", "_url_") %>%
    str_replace_all("\\b(\\d{7,})\\b", "_longnum_") %>%
    str_split(" ")
  
  tx <- lapply(tx, function(x) x[nchar(x) > 1])
  
  tx
}
train_spam <- train_spam %>%
  mutate(msg_list = string_cleaner(.$msg))
train_spam$msg_list[1:3]
vocab <- train_spam %>%
  select(msg_list) %>%
  unlist() %>%
  unique() %>%
  tibble::enframe(name = NULL, value = "word")

vocab
ham_vocab <- train_spam %>%
  filter(label == "ham") %>%
  select(msg_list) %>%
  tibble::deframe() %>%
  unlist()

spam_vocab <- train_spam %>%
  filter(label == "spam") %>%
  select(msg_list) %>%
  tibble::deframe() %>%
  unlist()

head(ham_vocab)
vocab <- table(ham_vocab) %>%
  tibble::as_tibble() %>%
  rename(ham_n = n) %>%
  left_join(vocab, ., by = c("word" = "ham_vocab"))

vocab <- table(spam_vocab) %>%
  tibble::as_tibble() %>%
  rename(spam_n = n) %>%
  left_join(vocab, ., by = c("word" = "spam_vocab"))

vocab
word_n <- c("unique" = nrow(vocab),
            "ham" = length(ham_vocab),
            "spam" = length(spam_vocab))

class_probs <- prop.table(table(train_spam$label))
word_probabilities <- function(word_n, category_n, vocab_n, smooth = 1) {
  prob <- (word_n + smooth) / (category_n + smooth * vocab_n)
  prob
}
vocab <- vocab %>%
  tidyr::replace_na(list(ham_n = 0, spam_n = 0)) %>%
  rowwise() %>%
  mutate(ham_prob = word_probabilities(
    ham_n, word_n["ham"], word_n["unique"])) %>%
  mutate(spam_prob = word_probabilities(
    spam_n, word_n["spam"], word_n["unique"])) %>%
  ungroup()

vocab
#classification
classifier <- function(msg, prob_df, ham_p = 0.5, spam_p = 0.5) {
  clean_message <- string_cleaner(msg) %>% unlist()
  
  probs <- sapply(clean_message, function(x) {
    filter(prob_df, word == x) %>%
      select(ham_prob, spam_prob)
  })
  
  if (!is.null(dim(probs))) {
    ham_prob <- prod(unlist(as.numeric(probs[1, ])), na.rm = TRUE)
    spam_prob <- prod(unlist(as.numeric(probs[2, ])), na.rm = TRUE)
    ham_prob <- ham_p * ham_prob
    spam_prob <- spam_p * spam_prob
    
    if (ham_prob > spam_prob) {
      classification <- "ham"
    } else if (ham_prob < spam_prob) {
      classification <- "spam"
    } else {
      classification <- "unknown"
    }
  } else {
    classification <- "unknown"
  }
  
  classification
}
spam_classification <- sapply(test_spam$msg,
                              function(x) classifier(x, vocab, class_probs["ham"],
                                                     class_probs["spam"]), USE.NAMES = FALSE)
fct_levels <- c("ham", "spam", "unknown")

test_spam <- test_spam %>%
  mutate(label = factor(.$label, levels = fct_levels),
         .pred = factor(spam_classification, levels = fct_levels))

performance <- yardstick::metrics(test_spam, label, .pred)
performance

table(paste("actual", test_spam$label), paste("pred", test_spam$.pred))
test_spam %>% 
  mutate(all_ham = "ham") %>% 
  mutate(all_ham = factor(all_ham, levels = fct_levels)) %>% 
  yardstick::metrics(label, all_ham)

