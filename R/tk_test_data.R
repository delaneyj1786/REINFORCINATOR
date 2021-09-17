
# Test data for TK
```{r reinforcement example }
reinforcement_ex_rcdf<-recounter(reinforcement,BEH,"o","A")$recounted_data_frame
tables_recount_table(reinforcement_ex_rcdf)$output_list
```

```{r punishment example}
punishment_ex_rcdf<-recounter(punishment,BEH,"o","A")$recounted_data_frame
tables_recount_table(punishment_ex_rcdf)$output_list


# Using Process list$sub_series_prop_tabs_without_margins
# # Average Probabilities
#  # B_NT (BEFORE NOT TARGET)
#  B_NT<- apply(ftable(sub_series_prop_tabs_without_margins),1,mean)[1]
#
#  # B_T (BEFORE NOT TARGET)
#  B_T<- apply(ftable(sub_series_prop_tabs_without_margins),1,mean)[2]
#
#  # A_NT (BEFORE NOT TARGET)
#  A_NT<- apply(ftable(sub_series_prop_tabs_without_margins),1,mean)[3]
#
#  # A_T (BEFORE NOT TARGET)
#  A_T<- apply(ftable(sub_series_prop_tabs_without_margins),1,mean)[4]
```

```{r 2 person overall example}
test_4<-rbind(reinforcement,punishment)
reinforcement_ex_4 <- recounter(test_4,BEH,"o","A")$recounted_data_frame

tables_recount_table(reinforcement_ex_4)$output_list

```


```{r 2 person split example}
group_splitter()

split_test4 <- group_splitter(test_4,BEH,"o","A",
                              group = "TAR", actor = "actor")

split_test4_recounted <- group_split_recounter(split_test4,"BEH","o","A", actor = "TAR")

tables_recount_table(split_test4_recounted )$output_list

```
