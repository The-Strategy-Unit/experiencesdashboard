
# split dataframe into equal chunks and find average score

split_data_spc <- function(data, variable = "service", chunks){
  
  if(chunks == "monthly"){
    
    return(
      data %>% 
        dplyr::mutate(month = as.Date(cut(date, "month"))) %>% 
        dplyr::mutate(fft = .data[[variable]] * 20)
    )
  } else {
    
    return(
      data %>% 
        dplyr::filter(.data[[variable]] %in% 1 : 5) %>% 
        dplyr::mutate(group = ceiling(dplyr::cur_group_rows() / nrow(.) * chunks)) %>% 
        dplyr::group_by(group) %>% 
        dplyr::mutate(date = min(date)) %>% 
        dplyr::mutate(fft = service * 20)
    )
  }
}

plot_fft_spc <- function(data){
  
  data %>% 
    qicharts2::qic(data = .,
                   x = date,
                   y = fft,
                   chart = "xbar",
                   xlab = "Date",
                   ylab = "% FFT score")
}
