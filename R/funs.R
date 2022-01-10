# install_github("wrengels/HWxtest", subdir="pkg")
# https://kbroman.org/pkg_primer/pages/github.html

# need to add partner in
#
recounter<-function(data,behaviorstream,
                       behavior,consequence,
                       actor = NULL,
                       missing_data = NULL,
                       contingency = NULL){ # adding actor = FALSE ... for now ...

  # reserve quoting for column names (probably symb for characters)
  behaviorstream <- dplyr::enquo(behaviorstream)
  actor <- dplyr::enquo(actor)
  #missing_data <- enquo(missing_data)

  data <-as.data.frame(data)

  ## Descriptive stats from cols ##
  n_obs <- data %>% dplyr::summarize(n_obs = length(!!behaviorstream))
  n_tar <- data %>% dplyr::summarize(n_targets = length(which(!!behaviorstream == behavior)))
  n_reinf <- data %>% dplyr::summarize(n_reinforcers = length(which(!!behaviorstream == consequence)))
  n_actor <- data %>% dplyr::summarize(n_unique_actors = length(unique(!!actor)))
  n_cont <- data %>% dplyr::summarize(n_cont = length(which(!!behaviorstream == consequence & lag(!!behaviorstream == behavior))))
  #  list(n_obs,n_tar[[1]])

  ## Probabilities ##
  ## currently unlabeled ##
  prob_tar_uncorrected <- n_tar[[1]]/n_obs[[1]]
  prob_tar_corrected <- n_tar[[1]]/(n_obs[[1]]-n_reinf[[1]])


  ## For Calculations ##
  # Overall Reinforcers
  reinf_index<- data %>% dplyr::summarize(reinf_index = which(!!behaviorstream==consequence))
  # contingencies
  contingency_index <-data %>% dplyr::summarize(contingency_index = which(!!behaviorstream == consequence & lag(!!behaviorstream == behavior)))


  #reinf_index$reinf_index # returns the vector

  # replace NA values
  na_index <- data %>% dplyr::summarize(na_index = which(is.na(!!behaviorstream)))

  # create a list item for descriptives
  descriptives <- list(n_obs = n_obs,
                       n_tar = n_tar,
                       n_reinf = n_reinf,
                       n_actor = n_actor,
                       n_cont = n_cont,
                       reinf_index =  reinf_index,
                       contingency_index = contingency_index,
                       na_index =  na_index,
                       prob_tar_uncorrected = prob_tar_uncorrected,
                       prob_tar_corrected = prob_tar_corrected)

  #   descriptives

  #
  #
  ## ** Initialize K Matrix **
  # remove missing values
  # behaviorstream <- ifelse(is.na(!!behaviorstream),0,!!behaviorstream)
  #
  data <- data %>% mutate(behaviorstream = ifelse(is.na(!!behaviorstream),0, !!behaviorstream))

  if(is.null(contingency)){

    #
    #   # check for missing contingenceis
    if (n_reinf[[1]]==0 | n_tar[[1]]==0){
      recount_stream<-c()
      sub_series<-c()
      recount_stream_index<-c()
      recount_recode_stream<-c()
      recount_sequence<-c()
      recount_df<-data.frame(recount_stream,
                             sub_series,
                             recount_stream_index,
                             recount_recode_stream,
                             recount_sequence,
                             stringsAsFactors = FALSE)
    } else {

      ## 1. create the single recounted stream
      # behaviorstream replicated m times
      # recount_stream = rep(data[,behaviorstream][[1]], n_cont)

      recount_stream = data %>% dplyr::summarize(recount_stream = rep(!!behaviorstream,n_reinf))


      #   # 1.1 recount actor stream
      recount_actor = data %>% dplyr::summarize(rep(!!actor, n_reinf))


      #     # 2. sub-series vector
      #     ## spans the recounted_stream
      #     ## indicates which 'copy' is the stream - one copy per reinforcer
      sub_series <- c()
      for(i in 1:n_reinf[[1]]){
        sub_series=c(sub_series,(rep(i,n_obs[[1]])))
      }


      # 3. Recount index stream

      recount_stream_index= rep(1:n_obs[[1]],n_reinf[[1]])

      # 4. recounted recode stream
      # recodes the recounted stream as T or NT
      # across all events (i.e. all reinforcers are non targets)
      # these have to be picked up by the conditional recount_sequence

      recount_recode_stream = vector("numeric",length(recount_stream))



      # 6. sequence vector
      recount_sequence= rep("",length(recount_stream))

      # 7. bind all to a dataframe

      recount_df<-data.frame(recount_stream,
                             sub_series,
                             recount_stream_index,
                             recount_recode_stream,
                             recount_sequence,
                             stringsAsFactors = FALSE)

      #     # 5. Recode Recount Stream
      #     ## This adds a 2 for each reinforcer ....
      #     ### note should adjust to create a new column without the recode

      for(i in seq_along(reinf_index[[1]])){
        recount_df[recount_df[,"sub_series"]==i,4][reinf_index[[1]][i]]<-2
      }

      #
      #     # 7. Fill Sequence Vector

      for(i in seq_along(reinf_index[[1]])){
        recount_df[recount_df[,"sub_series"]==i,5][1:reinf_index[[1]][i]-1]<-"B"
        recount_df[recount_df[,"sub_series"]==i,5][(reinf_index[[1]][i]+1):n_obs[[1]]]<-"A"
        recount_df[recount_df[,"sub_series"]==i,5][reinf_index[[1]][i]]<-"R"
      }



      #     # 8. Recode the recount stream
      recount_df$recount_recode_stream<-ifelse(recount_df$recount_stream==behavior,"T","NT")


      #   #9. factor the variable
      recount_df<- recount_df %>%
        mutate(recount_sequence = factor(recount_sequence))


      #10 relevel
      recount_df<- recount_df %>%
        mutate(recount_sequence = forcats::fct_relevel(recount_sequence, "B","A","R"))

      #11 recount numeric to recode
      recount_df<- recount_df %>%
        mutate(recount_recode_numeric = ifelse(recount_recode_stream == "T",1,0))


      #12 add in recounted actor
      if(!is.null(actor)){
        recount_df <- cbind(recount_df,recount_actor)
      }

      # 13. Add a regression column for the sequence (see BODR_71419_3)
      recount_df<- recount_df %>%
        mutate(regression_recount_sequence = ifelse(recount_sequence == "R",NA,recount_sequence))

      # 13.b make a factor
      recount_df<- recount_df %>%
        mutate(regression_recount_sequence = factor(regression_recount_sequence,
                                                    labels = c("B","A")))

    }

    # Full List
    output_list<-list(descriptive_statistics = descriptives, recounted_data_frame = recount_df)

    # print list
    return(output_list)

  }else{

    if (n_cont[[1]]==0 | n_tar[[1]]==0){
      recount_stream<-c()
      sub_series<-c()
      recount_stream_index<-c()
      recount_recode_stream<-c()
      recount_sequence<-c()
      recount_df<-data.frame(recount_stream,
                             sub_series,
                             recount_stream_index,
                             recount_recode_stream,
                             recount_sequence,
                             stringsAsFactors = FALSE)
    } else {

      ## 1. create the single recounted stream
      # behaviorstream replicated m times
      # recount_stream = rep(data[,behaviorstream][[1]], n_cont)

      recount_stream = data %>% dplyr::summarize(recount_stream = rep(!!behaviorstream,n_cont))


      #   # 1.1 recount actor stream
      recount_actor = data %>% dplyr::summarize(rep(!!actor, n_cont))


      #     # 2. sub-series vector
      #     ## spans the recounted_stream
      #     ## indicates which 'copy' is the stream - one copy per reinforcer
      sub_series <- c()
      for(i in 1:n_cont[[1]]){
        sub_series=c(sub_series,(rep(i,n_obs[[1]])))
      }


      # 3. Recount index stream

      recount_stream_index= rep(1:n_obs[[1]],n_cont[[1]])

      # 4. recounted recode stream
      # recodes the recounted stream as T or NT
      # across all events (i.e. all reinforcers are non targets)
      # these have to be picked up by the conditional recount_sequence

      recount_recode_stream = vector("numeric",length(recount_stream))



      # 6. sequence vector
      recount_sequence= rep("",length(recount_stream))

      # 7. bind all to a dataframe

      recount_df<-data.frame(recount_stream,
                             sub_series,
                             recount_stream_index,
                             recount_recode_stream,
                             recount_sequence,
                             stringsAsFactors = FALSE)

      #     # 5. Recode Recount Stream
      #     ## This adds a 2 for each reinforcer ....
      #     ### note should adjust to create a new column without the recode

      for(i in seq_along(contingency_index[[1]])){
        recount_df[recount_df[,"sub_series"]==i,4][contingency_index[[1]][i]]<-2
      }

      #
      #     # 7. Fill Sequence Vector

      # issue here is it is using the n reinfrocers = 2

      for(i in seq_along(contingency_index[[1]])){
        recount_df[recount_df[,"sub_series"]==i,5][1:contingency_index[[1]][i]-1]<-"B"
        recount_df[recount_df[,"sub_series"]==i,5][(contingency_index[[1]][i]+1):n_obs[[1]]]<-"A"
        recount_df[recount_df[,"sub_series"]==i,5][contingency_index[[1]][i]]<-"R"
      }



      #     # 8. Recode the recount stream
      recount_df$recount_recode_stream<-ifelse(recount_df$recount_stream==behavior,"T","NT")


      #   #9. factor the variable
      recount_df<- recount_df %>%
        mutate(recount_sequence = factor(recount_sequence))


      #10 relevel
      recount_df<- recount_df %>%
        mutate(recount_sequence = forcats::fct_relevel(recount_sequence, "B","A","R"))

      #11 recount numeric to recode
      recount_df<- recount_df %>%
        mutate(recount_recode_numeric = ifelse(recount_recode_stream == "T",1,0))


      #12 add in recounted actor
      if(!is.null(actor)){
        recount_df <- cbind(recount_df,recount_actor)
      }

      # # 13. Add a regression column for the sequence (see BODR_71419_3)
      # recount_df<- recount_df %>%
      #   mutate(regression_recount_sequence = ifelse(recount_sequence == "R",NA,recount_sequence))
      #
      # # 13.b make a factor
      # recount_df<- recount_df %>%
      #   mutate(regression_recount_sequence = factor(regression_recount_sequence,
      #                                               labels = c("B","A")))
      #
    }
    # Full List
    output_list<-list(descriptive_statistics = descriptives, recounted_data_frame = recount_df)

    # print list
    return(output_list)
  }}

Recounter5 <- function(data,behaviorstream,
                       behavior,consequence,
                       actor = NULL,
                       missing_data = NULL){ # adding actor = FALSE ... for now ...

  data <-as.data.frame(data)
  # filter out NA
  data <- data[,!is.na(behaviorstream)]

#  behaviorstream <- behaviorstream[!is.na(behaviorstream)]

  # # only if there are reinforcers ...
  # if(length(which(data[,behaviorstream]==consequence))> 0){
  #
  #   if(which(data[,behaviorstream]==consequence)[length(which(data[,behaviorstream] == consequence))] == length(data[,behaviorstream][[1]])){
  #     data[,behaviorstream][[1]][length(data[,behaviorstream][[1]])] <- "ELSE"
  #   }
  # }else{
  #   data[,behaviorstream][[1]][length(data[,behaviorstream][[1]])] <- data[,behaviorstream][[1]][length(data[,behaviorstream][[1]])]
  # }
  #



  ## ** Descriptive Check ** #
  # n_obs <- length(data[,behaviorstream][[1]])     # n observations      length(data[,behaviorstream])
  n_obs <- length(data[,behaviorstream])     # n observations      length(data[,behaviorstream])

  # if(is.na(x)) {x=FALSE} else {if(x) {x}}
  # https://www.programmingr.com/r-error-messages/r-error-missing-value-where-true-false-needed/
  if(data[n_obs,behaviorstream]==consequence){
    #   data[n_obs,behaviorstream] <- "EOO"}
    data[n_obs,behaviorstream] = TRUE} else{data[n_obs,behaviorstream]==consequence}


  n_tar <- length(which(data[,behaviorstream]== behavior))             # n behaviors
  n_reinf<- length(which(data[,behaviorstream] == consequence))        # n reinforces
  n_actor <- length(unique(data[,actor][[1]]))
  prob_tar_uncorrected <- n_tar/n_obs
  prob_tar_corrected <- n_tar/(n_obs-n_reinf)
  #
  reinf_index<-which(data[,behaviorstream]==consequence)
  # replace NA values
  na_index <- which(is.na(data[,behaviorstream]))

  # create a list item for descriptives
  descriptives <- list(n_obs = n_obs,
                       n_tar = n_tar,
                       n_reinf = n_reinf,
                       n_actor = n_actor,
                       reinf_index =  reinf_index,
                       na_index =  na_index,
                       prob_tar_uncorrected = prob_tar_uncorrected,
                       prob_tar_corrected = prob_tar_corrected)

  ## ** Initialize K Matrix **
  # remove missing values
  # behaviorstream <- ifelse(is.na(behaviorstream),0,behaviorstream)
  # check for missing contingenceis
  if (n_reinf==0 | n_tar==0){
    recount_stream<-c()
    sub_series<-c()
    recount_stream_index<-c()
    recount_recode_stream<-c()
    recount_sequence<-c()
    recount_df<-data.frame(recount_stream,
                           sub_series,
                           recount_stream_index,
                           recount_recode_stream,
                           recount_sequence,
                           stringsAsFactors = FALSE)
  } else {



    ## 1. create the single recounted stream
    # behaviorstream replicated m times
    #    recount_stream = rep(data[,behaviorstream][[1]], n_reinf)  ## recount_stream = rep(data[,behaviorstream], n_reinf)

    recount_stream = rep(data[,behaviorstream], n_reinf)  ## recount_stream = rep(data[,behaviorstream], n_reinf)

    # 1.1 recount actor stream
    recount_actor = rep(actor, n_reinf)

    # 2. sub-series vector
    ## spans the recounted_stream
    ## indicates which 'copy' is the stream - one copy per reinforcer
    sub_series <- c()
    for(i in 1:n_reinf){
      sub_series=c(sub_series,(rep(i,n_obs)))
    }

    # 3. Recount index stream
    ##
    recount_stream_index= rep(1:n_obs,n_reinf)



    # 4. recounted recode stream
    # recodes the recounted stream as T or NT
    # across all events (i.e. all reinforcers are non targets)
    # these have to be picked up by the conditional recount_sequence
    recount_recode_stream = vector("numeric",length(recount_stream))

    # 6. sequence vector
    recount_sequence= rep("",length(recount_stream))

    # 7. bind all to a dataframe

    recount_df<-data.frame(recount_stream,
                           sub_series,
                           recount_stream_index,
                           recount_recode_stream,
                           recount_sequence,
                           stringsAsFactors = FALSE)
    # 5. Recode Recount Stream
    ## This adds a 2 for each reinforcer ....
    ### note should adjust to create a new column without the recode
    for(i in seq_along(reinf_index)){
      recount_df[recount_df[,"sub_series"]==i,4][reinf_index[i]]<-2
    }


    # 7. Fill Sequence Vector

    for(i in seq_along(reinf_index)){
      recount_df[recount_df[,"sub_series"]==i,5][1:reinf_index[i]-1]<-"B"
      recount_df[recount_df[,"sub_series"]==i,5][(reinf_index[i]+1):n_obs]<-"A"
      recount_df[recount_df[,"sub_series"]==i,5][reinf_index[i]]<-"R"
    }


    # 8. Recode the recount stream
    recount_df$recount_recode_stream<-ifelse(recount_df$recount_stream==behavior,"T","NT")

    #9. factor the variable
    recount_df<- recount_df %>%
      mutate(recount_sequence = factor(recount_sequence))

    #10 relevel
    recount_df<- recount_df %>%
      mutate(recount_sequence = forcats::fct_relevel(recount_sequence, "B","A","R"))

    #11 recount numeric to recode
    recount_df<- recount_df %>%
      mutate(recount_recode_numeric = ifelse(recount_recode_stream == "T",1,0))


    #12 add in recounted actor
    if(!is.null(actor)){
      recount_df <- cbind(recount_df,recount_actor)
    }
    # 13. Add a regression column for the sequence (see BODR_71419_3)
    recount_df<- recount_df %>%
      mutate(regression_recount_sequence = ifelse(recount_sequence == "R",NA,recount_sequence))
    # 13.b make a factor
    recount_df<- recount_df %>%
      mutate(regression_recount_sequence = factor(regression_recount_sequence,
                                                  labels = c("B","A")))



  }
  # Full List
  output_list<-list(descriptive_statistics = descriptives, recounted_data_frame = recount_df)

  # print list
  return(output_list)
}

################ SHINY VERSION RECOUNTER
# Note - this is the same function but WITHOUT the data frame
Recounter2 <-function(behaviorstream, behavior,consequence, actor = NULL, missing_data = NULL){ # adding actor = FALSE ... for now ...
  behaviorstream <- behaviorstream[!is.na(behaviorstream)]

  ## ** Descriptive Check ** #
  n_obs <- length(behaviorstream)                         # n observations
  n_tar <- length(which(behaviorstream == behavior))      # n behaviors
  n_reinf<- length(which(behaviorstream == consequence))  # n reinforcers
  #
  reinf_index<-which(behaviorstream==consequence)
  # replace NA values
  na_index <- which(is.na(behaviorstream))

  # create a list item for descriptives
  descriptives <- list(n_obs = n_obs,
                       n_tar = n_tar,
                       n_reinf = n_reinf,
                       reinf_index =  reinf_index,
                       na_index =  na_index)


  ## ** Initialize K Matrix **
  # remove missing values
  # behaviorstream <- ifelse(is.na(behaviorstream),0,behaviorstream)
  # check for missing contingenceis
  if (n_reinf==0 | n_tar==0){
    stop("there are no observations of these contingencies")
  } else {

    ## 1. create the single recounted stream
    # behaviorstream replicated m times
    recount_stream = rep(behaviorstream, n_reinf)

    # 1.1 recount actor stream
    recount_actor = rep(actor, n_reinf)

    # 2. sub-series vector
    ## spans the recounted_stream
    ## indicates which 'copy' is the stream - one copy per reinforcer
    sub_series <- c()
    for(i in 1:n_reinf){
      sub_series=c(sub_series,(rep(i,n_obs)))
    }

    # 3. Recount index stream
    ##
    recount_stream_index= rep(1:n_obs,n_reinf)





    # 4. recounted recode stream
    # recodes the recounted stream as T or NT
    # across all events (i.e. all reinforcers are non targets)
    # these have to be picked up by the conditional recount_sequence
    recount_recode_stream = vector("numeric",length(recount_stream))

    # 6. sequence vector
    recount_sequence= rep("",length(recount_stream))

    # 7. bind all to a dataframe

    recount_df<-data.frame(recount_stream,
                           sub_series,
                           recount_stream_index,
                           recount_recode_stream,
                           recount_sequence,
                           stringsAsFactors = FALSE)

    # 5. Recode Recount Stream
    ## This adds a 2 for each reinforcer ....
    ### note should adjust to create a new column without the recode
    for(i in seq_along(reinf_index)){
      recount_df[recount_df[,"sub_series"]==i,4][reinf_index[i]]<-2
    }


    # 7. Fill Sequence Vector

    for(i in seq_along(reinf_index)){
      recount_df[recount_df[,"sub_series"]==i,5][1:reinf_index[i]-1]<-"B"
      recount_df[recount_df[,"sub_series"]==i,5][(reinf_index[i]+1):n_obs]<-"A"
      recount_df[recount_df[,"sub_series"]==i,5][reinf_index[i]]<-"R"
    }


    # 8. Recode the recount stream
    recount_df$recount_recode_stream<-ifelse(recount_df$recount_stream==behavior,"T","NT")

    #9. factor the variable
    recount_df<- recount_df %>%
      mutate(recount_sequence = factor(recount_sequence))

    #10 relevel
    recount_df<- recount_df %>%
      mutate(recount_sequence = forcats::fct_relevel(recount_sequence, "B","A","R"))

    #11 recount numeric to recode
    recount_df<- recount_df %>%
      mutate(recount_recode_numeric = ifelse(recount_recode_stream == "T",1,0))


    #12 add in recounted actor
    if(!is.null(actor)){
      recount_df <- cbind(recount_df,recount_actor)
    }
    # 13. Add a regression column for the sequence (see BODR_71419_3)
    recount_df<- recount_df %>%
      mutate(regression_recount_sequence = ifelse(recount_sequence == "R",NA,recount_sequence))
    # 13.b make a factor
    recount_df<- recount_df %>%
      mutate(regression_recount_sequence = factor(regression_recount_sequence,
                                                  labels = c("B","A")))

    # 13.c relevel
    #     recount_df<- recount_df %>%
    #        mutate(regression_recount_sequence =  fct_relevel(regression_recount_sequence,"B","A"))



  }
  # Full List
  output_list<-list(descriptive_statistics = descriptives, recounted_data_frame = recount_df)

  # print list
  return(output_list)
}


group_splitter <- function(data, behaviorstream, behavior,consequence,
                           group,
                           actor = NULL, missing_data = NULL,
                           contingency = NULL){
  # this just splits the data frame
  split(data, data[,group])

  # test
  #group_wise_test(two_person_picture,BEH, "x","A",group = "VIDELT", actor = "TAR")

}

group_splitter2 <- function(data, behaviorstream, behavior,consequence,
                    group1,group2,
                    actor = NULL, missing_data = NULL,
                    contingency = NULL, filt = NULL){
  # this just splits the data frame
  group1 <- as.factor(group1)
  group2 <- as.factor(group2)
  split_dat_2 <-split(data, f = (c(data[,group1],data[,group2])))
if(is.null(filt) == FALSE){
  #double_list<-double_split_test[sapply(double_split_test, function(x) dim(x)[1]) > 0]
  split_dat_2[sapply(split_dat_2, function(x) dim(x)[1]) > 0]
}else{
  split_dat_2
}
  # test
#  group_splitter2(two_person_picture,behaviorstream = "BEH", behavior = "o", consequence = "A", group1 = "VIDELT", group2 = "TAR")
}

group_split_recounter<- function(list, behaviorstream, behavior,consequence,actor){
  list %>% purrr::map(~Recounter5(.x,behaviorstream,behavior,consequence,actor)$recounted_data_frame) %>%
    purrr::map_dfr(~as.data.frame(.), .id = "GROUP")
}

# Group Split recounter for descriptives

group_split_recounter_desc <- function(list, behaviorstream, behavior,consequence,actor){
  list %>% purrr::map(~Recounter5(.x,behaviorstream,behavior,consequence,actor)$descriptive_statistics)
}



###################
### Reformatting Functions ####

## Need to make these tidyverse compatible


# Combiner function
combiner<- function(data, behaviorstream, code1, code2){
  behaviorstream <- dplyr::enquo(behaviorstream)
  data <- tibble(data)
  dplyr::mutate(data,Combined = ifelse(!!behaviorstream == code1 | !!behaviorstream == code2, paste(code1,code2,sep="_"),!!behaviorstream))

  }

# Deleter function
deleter<- function(data,behaviorstream,code1){
behaviorstream <- dplyr::enquo(behaviorstream)
data <- tibble::as_tibble(data)
dplyr::filter(data, !!behaviorstream != code1)
}

# Partner function recode
partner_recoder <- function(data,behaviorstream, type, consequence, partner){
  behaviorstream <- dplyr::enquo(behaviorstream)
  type <- dplyr::enquo(type)
  data <- tibble::as_tibble(data)
  data %>% dplyr::mutate(newcol = ifelse(!!behaviorstream == consequence & !!type == partner, paste(partner,consequence,sep="_"),!!behaviorstream))
}

group_filter <- function(data, GROUP1, GROUP2, DV){
  #  https://stackoverflow.com/questions/58846126/using-rlang-quasiquotation-with-dplyr-join-functions
  data <- data.frame(dat)
  GROUP1 <- dplyr::enquo(GROUP1)
  GROUP2 <- dplyr::enquo(GROUP2)
  DV <- dplyr::enquo(DV)

  #  data <- data.frame(data)
  # dat
  # FRQ OF DV by group 2
  FRQ_G2_DV <- dat %>%
    dplyr::group_by(!!GROUP1, !!GROUP2, !!DV) %>%
    dplyr::summarize(count = n()) %>%
    ungroup()
  # FRQ_G2_DV
  # Number of cases of group2
  FRQ_F2 <- dat%>%
    dplyr::group_by(!!GROUP1,!!GROUP2) %>%
    dplyr::summarize(n = n()) %>%
    ungroup()
  #FRQ_F2
  # Summary by video
  # vid_sum<-dplyr::left_join(FRQ_G2_DV,FRQ_F2, by = c(!!GROUP1,!!GROUP2) )
  vid_sum<-dplyr::left_join(FRQ_G2_DV,FRQ_F2)
  vid_sum<-vid_sum %>% dplyr::mutate(prop = count/n)
  vid_sum
}




#### Contingency Table Functions #####
tables_recount_table<- function(recounted_df){
  # will create three separate tables and output them to a list
  # provide a recount (sum lapply)
  # average
  # recompute

  # Sub-Series tables (Frequency)
  sub_series_tables <- addmargins(table(recounted_df$recount_sequence,
                                        recounted_df$recount_recode_stream,
                                        recounted_df$sub_series))

  # Sub-Series tables (Frequency) Sum with Margins (default last table)
  sum_table <- sub_series_tables[,,"Sum"]


  # Sub-Series tables without margins (Frequency)
  sub_series_tables_without_margins <- table(recounted_df$recount_sequence,
                                             recounted_df$recount_recode_stream,
                                             recounted_df$sub_series)

  # Sum Sub-Series tables without margins (Frequency)
  sub_sum_no_margins_setup <- ftable(sub_series_tables_without_margins)
  sub_sum_no_margins<-apply(sub_sum_no_margins_setup,1,sum)
  #
  sub_sum_no_margins_table<-matrix(sub_sum_no_margins,nrow=3,byrow = T)
  #sub_sum_no_margins_table <- sub_sum_no_margins_table[c(1,2),]
  rownames(sub_sum_no_margins_table) <- c("B","A", "R")
  colnames(sub_sum_no_margins_table) <- c("NT","T")

  # prop table without margins
  # https://stackoverflow.com/questions/13151394/how-to-create-a-prop-table-for-a-three-dimension-table
  sub_series_prop_tabs_without_margins <- prop.table(table(recounted_df$recount_sequence,
                                                           recounted_df$recount_recode_stream,
                                                           recounted_df$sub_series),c(3,1))

  # Average Probabilities
  # B_NT (BEFORE NOT TARGET)
  B_NT<- apply(ftable(sub_series_prop_tabs_without_margins),1,mean)[1]

  # B_T (BEFORE NOT TARGET)
  B_T<- apply(ftable(sub_series_prop_tabs_without_margins),1,mean)[2]

  # A_NT (BEFORE NOT TARGET)
  A_NT<- apply(ftable(sub_series_prop_tabs_without_margins),1,mean)[3]

  # A_T (BEFORE NOT TARGET)
  A_T<- apply(ftable(sub_series_prop_tabs_without_margins),1,mean)[4]

  #####
  avg_prob_list <- c(B_NT,B_T,A_NT,A_T)
  avg_diff <- A_T-B_T


  # Average Table (no margins)
  average_tab_no_margins<-sub_sum_no_margins_table / max((recounted_df)$sub_series)

  # Average Table (with margins )
  average_tab_with_margins<-sum_table/max((recounted_df)$sub_series) # use max reinforcer

  # Average table row margins
  average_row_margins<-average_tab_with_margins[c(1,2,3),3]

  # Recompute : Multiple each proportion by totals
  recompute_margins<-sub_series_prop_tabs_without_margins * average_row_margins

  # Recompute Marginal Average
  recompute_margins_setup <- ftable(recompute_margins)
  recompute_margins_sum<-apply(recompute_margins_setup,1,sum)

  recompute_margins_table<-matrix(recompute_margins_sum,nrow=3,byrow = T)
  #sub_sum_no_margins_table <- sub_sum_no_margins_table[c(1,2),]
  rownames(recompute_margins_table) <- c("B","A", "R")
  colnames(recompute_margins_table) <- c("NT","T")

  recompute_margins_table_avg = recompute_margins_table/max((recounted_df)$sub_series)

  # recompute values
  #recompute_values<-sub_series_prop_tabs_without_margins *  average_tab_no_margins
  recomputed_sub_series_cell_wise<-list()
  for(i in 1:max((recounted_df)$sub_series)){
    recomputed_sub_series_cell_wise[i]<-list(sub_series_tables_without_margins[,,i]*average_tab_no_margins)
  }



  list(process_list = list(sub_series_with_margins = sub_series_tables,                                         # returns sub - series sep tab
                           sum_sub_series_with_margins = sum_table,                                             # returns sum of sub-series w/margins
                           sub_series_tables_without_margins = sub_series_tables_without_margins,               # returns
                           sub_sum_no_margins = sub_sum_no_margins_table,
                           sub_series_prop_tabs_without_margins = sub_series_prop_tabs_without_margins,         # used for average probabilities with ftable
                           average_tab_with_margins = average_tab_with_margins,
                           row_average_totals = average_row_margins,
                           recompute_margins = recompute_margins,
                           average_tab_no_margins = average_tab_no_margins,
                           recomputed_sub_series_cell_wise = recomputed_sub_series_cell_wise,
                           recompute_margins_sum = recompute_margins_sum,
                           recompute_margins_table =recompute_margins_table,
                           recompute_margins_table_avg = recompute_margins_table_avg),
       output_list = list(avg_prob = avg_prob_list,
                          avg_diff = avg_diff,
                          sum_recount_table = sum_table,
                          average_table = average_tab_with_margins,
                          recompute_frequencies =  addmargins(recompute_margins_table_avg))
  )
}



### Statistical Tests

# Likert Test
allison_liker_z <- function(pta,ptt,pa,n,k){
  #pta
  #pt
  #pa
  #n
  #k
  z_num = pta - ptt
  z_denom = (ptt*((1-ptt)*(1-pa)))/((n-k)*pa)
  Z_1 = z_num/sqrt(z_denom)
  Z_1
}




## DATASETS #############
# The elevator data set contains

# ABE shows a mixed pattern - where he pushes (no stop) and pushes again and it stops (episode 1)
# Episode 2 there is a prompt stop
# This reinforces pushing again for episode 3 where it eventually stops
# We can assume here that the pushing / stop pattern has been reinforced in the past and that the
## mix of stop / non stop reinforces a MAINTENANCE
# We can also see the contingency change here...


# JAN shows a change in reinforcing NOPUSH
# for the first two episodes NOT Pushing yields a stop
# In episode 3 there is a quick change, where not pushing no longer stops the elevator
# This leads to an increase in Pushing which seems to reinforce more pushing


elevator<-tidyr::tibble(
  EPISODE = c(rep(1,4),rep(2,2), rep(3,6),
              rep(1,2),rep(2,2),rep(3,4),rep(4,2)),
  PERSON = c(rep("ABE",12),rep("JAN",10)),
  TIME = c(1,2,3,4,5,6,7,8,9,10,11,12,
           1,2,3,4,5,6,7,8,9,10,11,12),
  BEHAVIOR = c("PUSH","NOTSTOP","PUSH","STOP", # CONTINGENCY (.5)
          "PUSH","STOP", # (1)
          "PUSH","NOTSTOP","PUSH","NOTSTOP","PUSH","STOP", # no reinforcement (.33)
          "NOPUSH","STOP", # (0)
          "NOPUSH","STOP", # (0)
          "NOPUSH","NOSTOP","PUSH","STOP", # (1)
          "PUSH","STOP", # (1)
          "PUSH","NOSTOP","PUSH","STOP") # (.5)
)


# Note this is all contingent
# A classroom example
# One focal person
# One episode consisting of 25 observations

# The overall probability of ON-TASK (ON)
# The strength of the relationship between ACTIVE an APPROVAL decreases over time
# HOWEVER - active INCREASES OVER TIME
# This suggests a REINFORCING EFFECT (Despite low contingency!)

picture_stream<-tidyr::tibble(
  EPISODE = rep(1,25),
  PERSON = rep("S",25),
  BEHAVIOR = c("PASSIVE","PASSIVE","PASSIVE","ACTIVE", #4
          "APPROVAL", #1
          "ACTIVE","PASSIVE","PASSIVE", #3
          "APPROVAL", #1
          "ACTIVE","ACTIVE","PASSIVE","PASSIVE","ACTIVE","ACTIVE","PASSIVE","ACTIVE", #8
          "APPROVAL", #1
          "ACTIVE","ACTIVE","ACTIVE","ACTIVE","ACTIVE","ACTIVE","PASSIVE")
)

#This dataset consists of two DUPLICATE classroom observations (say one is a mistake)
#This is meant to illustrate the effects of nesting (for GLM) and repeated measurements (fixing the 'fixed effects')

# OVERALL STATS
# # ReenforcinateR::recounter(two_person_picture, BEHAVIOR,"ACTIVE","APPROVAL", actor = PERSON)$descriptive


# The overall probability of ACTIVE is : # $prob_tar_uncorrected [1] 0.4666667 # $prob_tar_corrected # [1] 0.5384615

# The overall probability of PASSIVE is :
# The contingency of APPROVAL and ACTIVE is :
# Overall number of observations is : # n_obs = 30

# BY PERSON STATS:
# split_two_person<-ReenforcinateR::group_splitter(two_person_picture,BEHAVIOR,"ACTIVE","APPROVAL",group = "PERSON")
ReenforcinateR::group_split_recounter_desc(split_two_person,"BEHAVIOR","ACTIVE","APPROVAL","TAR")



two_person_picture<-tidyr::tibble(
  EPISODE = c(rep(1,7),rep(2,8), rep(3,7), rep(4,8)),
  PERSON = c(rep("ABE",15),rep("JAN",15)),
  BEHAVIOR = c("PASSIVE","PASSIVE","PASSIVE","ACTIVE", #4
          "APPROVAL", #1
          "ACTIVE","PASSIVE", ### end 1
          "ACTIVE", #3
          "APPROVAL", #1
          "ACTIVE","ACTIVE","x","x","ACTIVE","ACTIVE", #6 # end two
          "PASSIVE","PASSIVE","PASSIVE","ACTIVE", #4
          "APPROVAL",
          "ACTIVE","PASSIVE","ACTIVE",
          "APPROVAL",
          "ACTIVE","ACTIVE","PASSIVE","PASSIVE","ACTIVE","ACTIVE"),
  PARTNER = c("SELF","SELF","SELF","SELF",
           "FRIEND",
           "SELF","SELF",
           "SELF",
           "STRANGER",
           "SELF","SELF","SELF","SELF","SELF","SELF",
           "SELF","SELF","SELF","SELF",
           "STRANGER",
           "SELF","SELF","SELF",
           "FRIEND",
           "SELF","SELF","SELF","SELF","SELF","SELF")
)


# $n_tar
# n_targets
# 1        14
# $n_reinf
# n_reinforcers
# 1             4
# $n_actor
# n_unique_actors
# 1               2
# n_cont
# 1      4




#Husband and Wife
# https://d1wqtxts1xzle7.cloudfront.net/41643964/Predicting-Marital-Happiness-and-Stability-from-Newlywed-Interactions-with-cover-page-v2.pdf?Expires=1641778260&Signature=EDzEEzUA~c96uPyf44cmOO3LgBnlOicBj7cnbLWYyMrlR0cRGROUN~RBx10YzY9BquoUKLVUJ5xyFPXLcPNED6NQI7TyAJDqGsOgqOws4BvNvlN6eqK7nJmUWA8i8dhEM1mPsceQgzQBwlNc3KxSXABGorWMY5txMGqpZI5bZVedhN~8WvBgWnkwe57dD5oWVsFv91w44sqaFcVxVEJHsLtL2J62nEUAk7gMmegQcHZizo0qCJif48LXRzPcMLPxW2Jq264JlOfTUH9rFujnIxK2wNW9QZ2Xpl57T9P2H~QhNNO~2kCAJoHiFfS2sT-JWok63m876AxuWWS9rf3guA__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA
# Codes
# Positive : Interest , validation , affection, humor, joy
# Negative : Disgust, Contempt, Belligerence, Domineering, Anger, Tension,
### Defensiveness, Whining, sadness, stonewalling
# Neutral

noeffect<-tidyr::tibble(
  EPISODE = rep(1,11),
  PERSON = rep("HUSBAND",11),
  PARTNER = c("HUSBAND","WIFE","HUSBAND","WIFE",
              "HUSBAND", "WIFE","HUSBAND", "WIFE",
              "HUSBAND","WIFE","HUSBAND"),
  BEHAVIOR = c("JOY","NEUTRAL",
               "NEUTRAL", "NEUTRAL", #1
          "JOY","INTEREST",
          "NEUTRAL", "NEUTRAL", #1
          "JOY","NEUTRAL",
          "NEUTRAL")
)


punishment<-tidyr::tibble(
  EPISODE = rep(1,11),
  PERSON = rep("HUSBAND",11),
  PARTNER = c("HUSBAND","WIFE","HUSBAND","WIFE",
              "HUSBAND", "WIFE","HUSBAND", "WIFE",
              "HUSBAND","WIFE","HUSBAND"),
  BEHAVIOR = c("JOY","NEUTRAL",
               "JOY", "NEUTRAL", #1
               "JOY","DISGUST",
               "NEUTRAL", "NEUTRAL", #1
               "NEUTRAL","VALIDATION",
               "NEUTRAL")
)



reinforcement<-tidyr::tibble(
  EPISODE = rep(1,11),
  PERSON = rep("HUSBAND",11),
  PARTNER = c("HUSBAND","WIFE","HUSBAND","WIFE",
              "HUSBAND", "WIFE","HUSBAND", "WIFE",
              "HUSBAND","WIFE","HUSBAND"),
  BEHAVIOR = c("JOY","NEUTRAL",
               "NEUTRAL", "NEUTRAL", #1
               "JOY","VALIDATION",
               "JOY", "VALIDATION", #1
               "JOY","NEUTRAL",
               "JOY")
)



################################
# Plotting Functions
plotting_restructure <- function(recounted_df){

  recounted_df  <- recounted_df %>% dplyr::group_by(sub_series) %>%
    mutate(sub_series_sum = cumsum(recount_recode_numeric),   # uses the total sum ... not running
           sub_series_total = max(recount_stream_index),
           sub_series_cum_run_prob = sub_series_sum / sub_series_total, # total sum as denom
           sub_series_run_prob = sub_series_sum / recount_stream_index) %>% ungroup()

  recounted_df

}



