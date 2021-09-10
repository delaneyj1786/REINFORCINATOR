recounter<-function(data,behaviorstream,
                       behavior,consequence,
                       actor = NULL,
                       missing_data = NULL,
                       contingency = NULL){ # adding actor = FALSE ... for now ...

  # reserve quoting for column names (probably symb for characters)
  behaviorstream <- enquo(behaviorstream)
  actor <- enquo(actor)
  #missing_data <- enquo(missing_data)

  data <-as.data.frame(data)

  ## Descriptive stats from cols ##
  n_obs <- data %>% summarize(n_obs = length(!!behaviorstream))
  n_tar <- data %>% summarize(n_targets = length(which(!!behaviorstream == behavior)))
  n_reinf <- data %>% summarize(n_reinforcers = length(which(!!behaviorstream == consequence)))
  n_actor <- data %>% summarize(n_unique_actors = length(unique(!!actor)))
  n_cont <- data %>% summarize(n_cont = length(which(!!behaviorstream == consequence & lag(!!behaviorstream == behavior))))
  #  list(n_obs,n_tar[[1]])

  ## Probabilities ##
  ## currently unlabeled ##
  prob_tar_uncorrected <- n_tar[[1]]/n_obs[[1]]
  prob_tar_corrected <- n_tar[[1]]/(n_obs[[1]]-n_reinf[[1]])


  ## For Calculations ##
  # Overall Reinforcers
  reinf_index<- data %>% summarize(reinf_index = which(!!behaviorstream==consequence))
  # contingencies
  contingency_index <-data %>% summarize(contingency_index = which(!!behaviorstream == consequence & lag(!!behaviorstream == behavior)))


  #reinf_index$reinf_index # returns the vector

  # replace NA values
  na_index <- data %>% summarize(na_index = which(is.na(!!behaviorstream)))

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

      recount_stream = data %>% summarize(recount_stream = rep(!!behaviorstream,n_reinf))


      #   # 1.1 recount actor stream
      recount_actor = data %>% summarize(rep(!!actor, n_reinf))


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
        mutate(recount_sequence = fct_relevel(recount_sequence, "B","A","R"))

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

      recount_stream = data %>% summarize(recount_stream = rep(!!behaviorstream,n_cont))


      #   # 1.1 recount actor stream
      recount_actor = data %>% summarize(rep(!!actor, n_cont))


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
        mutate(recount_sequence = fct_relevel(recount_sequence, "B","A","R"))

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

Recounter5<-function(data,behaviorstream,
                     behavior,consequence,
                     actor = NULL,
                     missing_data = NULL){ # adding actor = FALSE ... for now ...

  data <-as.data.frame(data)
  # only if there are reinforcers ...
  if(length(which(data[,behaviorstream]==consequence))> 0){

    if(which(data[,behaviorstream]==consequence)[length(which(data[,behaviorstream] == consequence))] == length(data[,behaviorstream][[1]])){
      data[,behaviorstream][[1]][length(data[,behaviorstream][[1]])] <- "ELSE"
    }
  }else{
    data[,behaviorstream][[1]][length(data[,behaviorstream][[1]])] <- data[,behaviorstream][[1]][length(data[,behaviorstream][[1]])]
  }



  ## ** Descriptive Check ** #
  # n_obs <- length(data[,behaviorstream][[1]])     # n observations      length(data[,behaviorstream])
  n_obs <- length(data[,behaviorstream])     # n observations      length(data[,behaviorstream])

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
      mutate(recount_sequence = fct_relevel(recount_sequence, "B","A","R"))

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




group_splitter <- function(data, behaviorstream, behavior,consequence,
                           group,
                           actor = NULL, missing_data = NULL,
                           contingency = NULL){
  # this just splits the data frame
  split(data, data[,group])

  # test
  #group_wise_test(two_person_picture,BEH, "x","A",group = "VIDELT", actor = "TAR")

}

group_split_recounter<- function(list, behaviorstream, behavior,consequence,actor){
  list %>% map(~Recounter5(.x,behaviorstream,behavior,consequence,actor)$recounted_data_frame) %>%
    map_dfr(~as.data.frame(.), .id = "GROUP")

  # ## TESTING
  # group_split_recounter(gs_test,behaviorstream = "BEH","x","A",actor = "TAR")
}


###################
### Reformatting Functions ####
# Combiner function
combiner <- function(behaviorstream, code1,code2){

  # Step 1 : find indices of code 1 and 2
  # find all code 1
  code_index_1 <- which(behaviorstream==code1)
  # find all code 2
  code_index_2 <- which(behaviorstream==code2)


  # step 2 : concatenate into one list of codes
  code_index_combined <- c(code_index_1,code_index_2)



  # Step 3 : replace all values with 'RECODE'

  for(i in seq_along(code_index_combined)){
    behaviorstream[code_index_combined]<-code1

  }

  # print behaviorstream
  behaviorstream
}

# Deleter function
deleter <- function(behaviorstream, code1){

  # Step 1 : find indices of code
  # find all code 1
  code_index_1 <- which(behaviorstream==code1)


  # Step 2 : replace all values with 'RECODE'

  for(i in seq_along(code_index_1)){
    behaviorstream[code_index_1]<-NA

  }

  # Convert to data frame

  (as_tibble(behaviorstream) %>%
      filter(!is.na(value)))[[1]] # filter out missing values
  # returns vector
}



#################
## DATASETS #############

elevator<-tidyr::tibble(
  VIDELT = rep(2,14),
  TAR = rep("S",14),
  BEH = c("o","x",
          "x","x",
          "A",
          "o","x","x","x",
          "A",
          "o","o","o","x")
)


# Note this is all contingent
picture_stream<-tidyr::tibble(
  VIDELT = rep(2,25),
  TAR = rep("S",25),
  BEH = c("x","x","x","o", #4
          "A", #1
          "o","x","o", #3
          "A", #1
          "o","o","x","x","o","o","x","o", #8
          "A", #1
          "o","o","o","o","o","o","x")
)

#

two_person_picture<-tidyr::tibble(
  VIDELT = c(rep(1,15),rep(2,15)),
  TAR = c(rep("ABE",15),rep("JAN",15)),
  BEH = c("x","x","x","o", #4
          "A", #1
          "o","x","o", #3
          "A", #1
          "o","o","x","x","o","o", #6
          "x","x","x","o", #4
          "A",
          "o","x","o",
          "A",
          "o","o","x","x","o","o")
)



noeffect<-tidyr::tibble(
  VIDELT = rep(2,11),
  TAR = rep("ABE",11),
  BEH = c("o","x","x", #4
          "A", #1
          "o","x","x", #3
          "A", #1
          "o","x","x")
)


punishment<-tibble(
  VIDELT = rep(2,11),
  TAR = rep("ABE",11),
  BEH = c("o","o","o", #4
          "A", #1
          "o","o","x", #3
          "A", #1
          "o","x","x")
)


reinforcement<-tibble(
  VIDELT = rep(2,11),
  TAR = rep("ABE",11),
  BEH = c("o","x","x", #4
          "A", #1
          "o","o","x", #3
          "A", #1
          "o","o","o")
)
