# rec = recipe(Status ~ Seniority + Marital + Records + Job, data = credit_data)
# rec = rec %>% step_WOE(all_numeric(),outcome="Status")
# rec =rec %>% prep(training=credit_data)
# bake(rec, newdata = credit_data)
