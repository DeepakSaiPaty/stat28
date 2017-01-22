
# ============== Grading script for Gradescope ==============
# ====== For Exercise 1-5 with Exercise 4 (d) excluded ======

library(rjson)

grade = c()
for (i in 1:7){
    grade = c(grade, capture.output(test_dir(paste0("q/q", i), reporter="minimal")))
}

scores <- c(grade[1] == ".",
            substring(grade[2], 1, 1) == ".",
            substring(grade[2], 2, 2) == ".",
            grade[3:7] == paste0("vec", 1:5, ": ."),
            grade[8] == "a: ...",
            grade[9] == "b: ...",
            grade[10] == "c: .",
            grade[12] == "...")

scores[4:5] <- T

max_scores <- c(10, rep(8, 10), 10)

qnames <- c("Exercise 1", "Exercise 2 (a)", "Exercise 2 (b)",
            paste0("Exercise 3 (", c("a", "b", "c", "d", "e"), ")"),
            paste0("Exercise 4 (", c("a", "b", "c"), ")"),
            "Exercise 5")

scores.df <- data.frame(score = max_scores*scores,
                        max_score = max_scores,
                        name = qnames,
                        stringsAsFactors = FALSE)

dlist <- list(score = sum(scores.df$score), 
              max_score = sum(max_scores), 
              output = "Check coding answers relevant to the entire submission",
              tests = unname(split(scores.df, 1:nrow(scores.df))))

write(rjson::toJSON(dlist), file = "results.json")
