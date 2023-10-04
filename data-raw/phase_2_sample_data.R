## code to prepare `phase_2_sample_data` dataset

library(dplyr)
library(readr)
library(tidyr)

# create dummy location and demographic data

# Set seed for reproducibility
set.seed(123)
n_rows <- 4000

# Create vectors for each column

# Division
division <- sample(1:3, n_rows, replace = TRUE)

# Team based on Division
specialty <- character(n_rows)
for (i in 1:n_rows) {
  if (division[i] == 1) {
    specialty[i] <- sample(LETTERS[1:5], 1)
  } else if (division[i] == 2) {
    specialty[i] <- sample(LETTERS[10:16], 1)
  } else {
    specialty[i] <- sample(LETTERS[20:26], 1)
  }
}

# team based on specialty
team <- character(n_rows)
for (i in 1:n_rows) {
  if (specialty[i] %in% c(LETTERS[1:5])) {
    team[i] <- sample(c("Cardiology", "Neurology", "Orthopedics"), 1)
  } else if (specialty[i] %in% c(LETTERS[10:16])) {
    team[i] <- sample(c("Pediatrics", "Oncology", "Gynecology"), 1)
  } else {
    team[i] <- sample(c("Dermatology", "ENT", "Urology"), 1)
  }
}

# Create the dataframe
hospital_data <- data.frame(
  date = sample(seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "days"), n_rows, replace = TRUE),
  Division = paste0("Division_", division),
  Specialty = paste0("Specialty_", specialty),
  Team = paste0(team, "_team"),
  fft_score = sample(1:5, n_rows, replace = TRUE),
  age = runif(n_rows, min = 10, max = 90),
  sex = sample(c("Female", "Male", "Unknown"), n_rows, replace = TRUE),
  ethnicity = sample(c("White", "Asian", "Black", "Unknown", "Mixed"), n_rows, replace = TRUE)
) |>
  dplyr::mutate(
    age = dplyr::case_when(
      age < 12 ~ "0 - 11",
      age < 18 ~ "12 - 17",
      age < 26 ~ "18 - 25",
      age < 40 ~ "26 - 39",
      age < 65 ~ "40 - 64",
      age < 80 ~ "65 - 79",
      age > 79 ~ "80+",
      TRUE ~ as.character(age)
    )
  ) |>
  arrange(date) |>
  mutate(comment_id = row_number())

# Create comment and question data from public data 
df = read_csv("https://raw.githubusercontent.com/CDU-data-science-team/pxtextmining/main/datasets/v6framework_230718.csv")

comment_data <- df |>
  filter(`FFT question` != 'What was good?') |>
  mutate(
    question_type = case_when(
      .data$`FFT question` == 'FFT Why?' ~ 'question_1',
      # .data$`FFT question` == 'What was good?' ~ 'question_1',
      TRUE ~ "question_2"),
    comment_id = row_number()
  ) |>
  rename(comment_txt = `FFT answer`) |>
  select(comment_id, question_type, comment_txt) |>
  pivot_wider(names_from = question_type, values_from = comment_txt) 


question1 <- comment_data |>
  filter(!is.na(question_1)) |>
  select(question_1) |>
  mutate(comment_id = row_number())

question2 <- comment_data |>
  filter(!is.na(question_2))|>
  select(question_2) |>
  mutate(comment_id = row_number())

comment_data <- question1 |>
  right_join(question2, by = 'comment_id') |>
  select(comment_id, everything()) |>
  mutate(
    across(where(is.character), \(.x) gsub('Did not answer|Na|N/A.', NA_character_, .x, ignore.case=T))
  ) |>
  filter(!(is.na(.data$question_2) &
             is.na(.data$question_1))) |># |>
  mutate(comment_id = row_number()) 

phase_2_sample_data <- comment_data |>
  left_join(hospital_data, by = 'comment_id') |>
  rename(location_1 = Division, location_2 = Specialty, location_3 = Team) |>
  select(
    date, location_1, location_2, location_3, fft_score, question_1, question_2, age, sex, ethnicity
  )

phase_2_sample_data |> write.csv('data-raw/phase_2_sample_data.csv', row.names = F)
