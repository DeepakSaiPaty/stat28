library(stringr)

grade = c()
for (i in 1:7){
    grade = c(grade, capture.output(test_dir(paste0("q/q", i), reporter="minimal")))
}
write.table(sum(str_count(grade, "\\.")), "grade.txt")



as.numeric(factor(c("fri", "two", "leuf")))

dnorm(0, 1, 2)
