install.packages("stargazer")
library(stargazer)

summary(Oxf_mult_var_reg_2016_lm)
summary(Oxf_mult_var_reg_2020_lm)
summary(Oxf_mult_var_reg_2021_lm)
summary(Oxf_mult_var_reg_2023_lm)


stargazer(Oxf_mult_var_reg_2016_lm, Oxf_mult_var_reg_2020_lm, Oxf_mult_var_reg_2021_lm, Oxf_mult_var_reg_2023_lm,
          type = "text")

stargazer(Oxf_mult_var_reg_2016_lm, type = "text")
stargazer(Oxf_mult_var_reg_2020_lm, type = "text")
stargazer(Oxf_mult_var_reg_2021_lm, type = "text")
stargazer(Oxf_mult_var_reg_2023_lm, type = "text")


stargazer(Oxf_mult_var_reg_2016_lm, Oxf_mult_var_reg_2020_lm, Oxf_mult_var_reg_2021_lm, Oxf_mult_var_reg_2023_lm, 
          type = "text")

#Just the linear regression?

summary(Oxf_reg_2016)
summary(Oxf_reg_2020)
summary(Oxf_reg_2021)
summary(Oxf_reg_2023)

stargazer(Oxf_reg_2016,Oxf_reg_2020,Oxf_reg_2021,Oxf_reg_2023,
          type = "text")

stargazer(Oxf_reg_2016, Oxf_mult_var_reg_2016_lm,type = "text")
stargazer(Oxf_reg_2020, Oxf_mult_var_reg_2020_lm, type = "text")
stargazer(Oxf_reg_2021, Oxf_mult_var_reg_2021_lm, type = "text")
stargazer(Oxf_reg_2023, Oxf_mult_var_reg_2023_lm, type = "text")





#################### General assembly


stargazer(lin_model, type = "text")
stargazer(GA_Sent_Scor_Mul_Reg, type = "text")
