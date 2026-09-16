student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
            
section <- factor(section, levels = c("A", "B"))

students <- data.frame(
  student_id = student_id,
  section = section,
  quiz1 = quiz1,
  quiz2 = quiz2,
  passed = passed
)

score_matrix <- matrix(
  c(quiz1, quiz2),
  nrow = 6,
  ncol = 2,
  dimnames = list(student_id, c("quiz1", "quiz2"))
)

cutoffs <- c(pass = 70, excellent = 90)
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = cutoffs
)

# A vector is a one-dimensional collection of values that normally has the same data type.

# A factor is a vector used to store categorical data with defined levels.

# A matrix is a two-dimensional data structure where all values must have the same data type.

# A data frame is a two-dimensional structure that can contain different data types in different columns, while a list can contain different types of objects and can have different lengths.

score_matrix["S04", "quiz2"]
score_matrix[1:2, , drop = FALSE]
course_record["course"]
course_record[["course"]]
course_record$course



# [ returns a subset that can still contain the original structure.

# [[ extracts a single element from a list.

# $ extracts a named element from a list using its name.

students$average <- rowMeans(students[, c("quiz1", "quiz2")])
students$excellent <- students$average >= 90
section_a <- students[students$section == "A" & students$average >= 80, ]
section_a_selected <- section_a[, c("student_id", "section", "average")]
student_averages <- students$average
names(student_averages) <- students$student_id

students
section_a_selected
student_averages

# These calculations are vectorized because they operate on entire vectors or rows at once instead of using a loop to process each student individually.

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))
measurements_complete <- measurements[complete.cases(measurements), ]
measurements$sample_id[!complete.cases(measurements)]
# x == NA is not a valid missing-value test because NA represents an unknown or missing value, so comparing a value to NA produces NA rather than TRUE or FALSE. is.na(x) should be used to test for missing values.

measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- ifelse(
  is.na(measurements$ph),
  NA,
  measurements$ph < 7
)

selected <- measurements[
  complete.cases(measurements) & measurements$site %in% c("North", "South") & measurements$status == "ok",
]

selected <- selected[
  , c("sample_id", "site" ,"temp_c", "temp_f", "ph")
]

overall_mean_c <- mean(measurements$temp_c, na.rm = TRUE)

south_mean_c <-mean(
  measurements$temp_c[measurements$site == "South"],
  na.rm =TRUE
)

selected 
overall_mean_c
south_mean_c

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

A * B
A %*% B
dim(A * B)
dim(A %*% B)



# Element-wise multiplication multiplies corresponding elements of two matrices.
# Matrix multiplication uses rows from the first matrix and columns from the second matrix.
# A * B and A %*% B are different operations even when the matrices have the same dimensions.

student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
)
{
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return ("A")
  } else if (score >= b_min) {
    return ("B")
  } else if (score >= c_min) {
    return ("C")
  } else if (score >= d_min) {
    return ("D")
  } else {
    return ("F")
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

#> grade_one(NA)
#[1] NA
#> grade_one(90)
#[1] "A"
#> grade_one(80)
#[1] "B"
#> grade_one(85)
#[1] "B"
#> grade_one(74)
#[1] "C"

grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades
# During the loop, i represents the position of the current score in the scores vector.

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  
  total_count <- length(x)
  missing_count <- sum(is.na(x))
  
  mean_score <- mean(x, na.rm = na.rm)
  sd_score <- sd(x, na.rm = na.rm)
  min_score <- min(x, na.rm = na.rm)
  max_score <- max(x, na.rm = na.rm)
  
  result <- c(
      count = total_count,
      missing = missing_count,
      mean = round(mean_score, digits),
      sd = round(sd_score, digits),
      minimum = round(min_score, digits),
      maximum = round(max_score, digits)
  )
  return(result)
}

summarize_scores(scores)
summarize_scores( x = scores, na.rm = TRUE, digits = 2 )

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}
  plot_scores(
    scores,
    type = "b",
    pch = 19,
    xlab = "Position",
    ylab = "Score",
    main = "Student Scores"
)
# The plot will show student scores on the y-axis and their positions in the scores vector on the x-axis. The missing value at position 3 will not have a plotted point because the score is NA.

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )
