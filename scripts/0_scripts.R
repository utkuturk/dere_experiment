read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

################################################################################
################################################################################
################################################################################

se_cousineau <- function(df, n_conditions, subject, DV, group, is_proportion = NULL)
{
  stopifnot(!"avgDV" %in% colnames(df))
  subject_var <- substitute(subject) %>% deparse() %>% gsub("\"", "", .)
  DV <- substitute(DV) %>% deparse() %>% gsub("\"", "", .)
  stopifnot( subject_var %in% colnames(df) && DV %in% colnames(df) )
  
  subj_means <- df %>% group_by(.dots = subject_var) %>% 
    dplyr::summarize(avgDV := mean(!!as.name(DV), na.rm = T), 
                     .groups = "drop")
  GM <- mean(subj_means$avgDV)
  df %<>% group_by(.dots = subject_var) %>% 
    dplyr::mutate(nDV = !!as.name(DV) - mean(!!as.name(DV), na.rm = T) + GM) %>%
    ungroup()
  
  if (is.null(is_proportion)) {
    dv <- df[[DV]]
    dv_unique <- unique(dv)
    if ( is.logical(dv) || (length(dv_unique) == 2 && all(dv_unique %in% c(0,1))) ) {
      is_proportion <- TRUE
    } else {
      is_proportion <- FALSE
    }
  }
  
  var_correction_factor <- n_conditions/(n_conditions-1)
  df %>% group_by(.dots = group) %>%
    dplyr::summarize(M = mean(nDV, na.rm = T),
                     Var = ifelse(is_proportion, M*(1-M), var(nDV, na.rm = T)) * var_correction_factor,
                     N = sum(!is.na(nDV)),
                     SE = sqrt(Var/N), 
                     .groups = "drop" )
}

################################################################################
################################################################################
################################################################################

# Function to calculate standard error.
se <- function(x) {
  return(sqrt(var(x,na.rm=TRUE)/length(na.omit(x))))
}

################################################################################
################################################################################
################################################################################

# Function to calculate binomial standard error.
se.bin <- function(x){
  n.success = sum(na.omit(x)) #x is coded as 1 (success) or 0 (failure), so number of successes is the sum of x = 1 
  n         = length(na.omit(x))
  p         = n.success / n
  return(sqrt(p*(1-p)/n))
}

################################################################################
################################################################################
################################################################################

encode_expected <- function(df) {
  df %>% mutate(is_expected = case_when(
    answer == "2ndOne" & is_Evet == FALSE ~ TRUE,
    answer == "1stOne" & is_Evet == TRUE ~ TRUE,
    TRUE ~ FALSE
  ))
}

################################################################################
################################################################################
################################################################################

excl <- function(df, col, excl_list) {
  noexcl = nrow(df)
  df = df %>% subset(!get(col) %in% excl_list)
  print(round((noexcl - nrow(df)) / noexcl, digits = 2))
  df
}

################################################################################
################################################################################
################################################################################

read_feathers <- function(path) {
  ## get the files
  files <- list.files(path, 
                      pattern = "\\.feather$", 
                      full.names = T)
  
  ## form object names
  object_names <- files %>% basename(.) %>% 
    tools::file_path_sans_ext(.) %>% 
    gsub("data_", '', .)
  
  ## read files and read them to environment
  lapply(files, 
         read_feather) %>% 
    setNames(object_names) %>%
    list2env(envir=globalenv())
}