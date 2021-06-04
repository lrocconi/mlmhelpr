## code to prepare `DATASET` dataset goes here
  # I created this file and folder by using usethis::use_data_raw()

# load high school and beyond data ---
hsb <- haven::read_spss("data-raw/HSB.sav")

# prep data
hsb <- hsb %>%
  mutate_at(vars(minority, female, catholic),
            as_factor)


# load nurses data ----

nurses <- haven::read_spss("data-raw/nurses.sav")

# prep data
nurses <- nurses %>%
  mutate_at(vars(expcon, specarewrd, hospsize),
            as_factor) %>%
  select(-13:-21)

# load gpa2 data ----

gpa <- haven::read_spss("data-raw/gpa2.sav")

# prep data

gpa <- gpa %>%
  mutate_at(vars(sex, admitted), as_factor) %>%
  pivot_longer(gpa1:gpa6, names_to="gpa_occasion",
               values_to="gpa") %>%
  group_by(student) %>%
  mutate(occas = row_number(), .before=1) %>%
  pivot_longer(job1:job6,  names_to="job_occasion",
               values_to="job") %>%
  distinct(student, occas, .keep_all=T)

# load UTHAI1 data for logistic regression ----

uthai <- haven::read_spss("data-raw/UTHAI1.sav")

usethis::use_data(hsb, nurses, gpa, uthai, overwrite = TRUE)
