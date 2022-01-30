library(tidyverse) #Data Manipulation and Plotting
library(lubridate) #Date Manipulation
library(arulesSequences) #Running the Sequence mining algorithm
library(ggtext) #Making adding some flair to plots
library(tidygraph)  ## Creating a Graph Structure
library(ggraph) ## Plotting the Network Graph Structure
library(dplyr)




seq_pps_c_c <- read.csv('/Users/baselhussein/Projects/seqMining/newData/level/changeless/df_changeless_augment.csv')
seq_pps_c_c

#Convert Everything to Factor
seq_pps_c_c['items'] <- lapply(seq_pps_c_c['items'], factor)
seq_pps_c_c

sessions <-  as(seq_pps_c_c %>% transmute(move = items), "transactions")
transactionInfo(sessions)$sequenceID <- seq_pps_c_c$sequenceID
transactionInfo(sessions)$eventID = seq_pps_c_c$eventID

inspect(head(sessions))

sessions


itemsets <- cspade(sessions, 
                   parameter = list(support = 0.4, maxgap = 0), 
                   control = list(verbose = FALSE))

inspect(head(itemsets))



#inspect(head(seq_pps_c_c))


#Convert Back to DS
itemsets_df <- as(itemsets, "data.frame") %>% as_tibble()
itemsets_df

#Top Frequent Item Sets
itemsets_df %>%
  slice_max(support, n = 20) %>% 
  ggplot(aes(x = fct_reorder(sequence, support),
             y = support,
             fill = sequence)) + 
  geom_col() + 
  geom_label(aes(label = support %>% scales::percent()), hjust = 0.5) + 
  labs(x = "Move", y = "Support", title = "Most Frequent Itemsets (Changeless)",
       caption = "**Support** is the percent of segments the contain the item set") + 
  scale_fill_discrete(guide = F) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, .1))) + 
  coord_flip() + 
  cowplot::theme_cowplot() + 
  theme(
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = 'plot',
    plot.title.position = 'plot'
  )


rules <- ruleInduction(itemsets, 
                       confidence = 0.6, 
                       control = list(verbose = FALSE))

inspect(head(rules, 10))
