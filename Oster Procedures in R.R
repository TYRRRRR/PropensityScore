# Osters
# Load data
lalonde<-read.dta("C:\\Users\\msgc\\Dropbox (Penn GSE)\\Stats3PHUDCFILY\\Fourth week\\lalonde.dta")
dim(lalonde)
fix(lalonde)
head(lalonde)
summary(lalonde)
table(lalonde$treat)
set.seed(1)
###Last week's 1st stage specification
# treat ~ age + educ + married + nodegree + black + hispan + re74 + re75 + I(age^2) + I(educ*age) + I(educ^2)

# twang does not allow to include transformations, these need to be conducted prior
# lalonde$age2 <- lalonde$age^2
# lalonde$educ2 <- lalonde$educ^2
# lalonde$educage <- lalonde$educ*lalonde$age
### Estimate  propensity score lalonde:
ps.lalonde <- ps(treatment ~ age + age_sq + educ + educ_sq + married + nodegree + black + hisp + re74 + re75 + re74_sq + re75_sq + u74 + u75 + u74_hisp + u74_black,
	data = lalonde,
	n.trees=1000,#maximum number of iterations that gbm will run.
	interaction.depth=3,#controls the level of interactions allowed in the GBM, with default =3
	shrinkage=0.01,# 0.05 Shrinkage is commonly used in ridge regression where it literally shrinks regression coefficients to zero and, thus, reduces the impact of potentially unstable regression coefficients. In the context of GBMs, shrinkage is used for reducing, or shrinking, the impact of each additional fitted base-learner. It reduces the size of incremental steps and thus penalizes the importance of each consecutive iteration http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3885826/
	perm.test.iters=100,#of Monte Carlo trials are run to establish the reference distribution of KS statistics for each covariate
	stop.method=c("es.mean"),#specifies a set (or sets) of rules and measures for assessing the balance, or equivalence, established on the pretreatment covariates of the weighted treatment and control groups. The ps function selects the optimal number of GBM iterations to minimize the differences between the treatment and control groups as measured by the rules of the given stop.method object.
	estimand = "ATT",
	verbose=FALSE)

summary(ps.lalonde)

lalonde.balance <- bal.table(ps.lalonde)
lalonde.balance

install.packages("xtable")
library(xtable)
pretty.tab <- lalonde.balance$es.mean.ATT[,c("tx.mn","ct.mn","p")]
pretty.tab <- cbind(pretty.tab, lalonde.balance$unw[,c("ct.mn","p")])
xtable(pretty.tab,
       caption = "Balance of the treatment and comparison groups",
       label = "tab:balance",
       digits = c(0, 2, 2, 4, 2,2),
       align=c("l","r","r","r","r","r"))



lalonde$pscore<-ps.lalonde$ps$ es.mean.ATT
lalonde$w<-ps.lalonde$w$es.mean.ATT
o_delta_psw <- 
function (y, x, con, w, m = "none", id = "none", time = "none", 
    beta = 0, R2max, type, data) 
{
    if (type == "lm") {
        data <- data %>% dplyr::rename(y_var = all_of(y), x_var = all_of(x), w_var = all_of(w))
    }
    if (type == "plm") {
        data <- data %>% dplyr::rename(y_var = all_of(y), x_var = all_of(x), w_var = all_of(w), 
            id_var = all_of(id), time_var = all_of(time))
        data_plm <- pdata.frame(data, index = c("id_var", 
            "time_var"), drop.index = TRUE, row.names = TRUE)
    }
    if (m == "none") {
        model0_formula <- as.formula("y_var ~ x_var")
        model1_formula <- as.formula(paste("y_var ~ x_var +", 
            con))
        aux_model_formula <- as.formula(paste("x_var ~", 
            con))
        if (type == "plm") {
            sigma_xx_model_formula <- as.formula(paste("x_var ~ factor(id_var)"))
        }
    }
    else {
        model0_formula <- as.formula(paste("y_var ~ x_var +", 
            m))
        model1_formula <- as.formula(paste("y_var ~ x_var +", 
            con, "+", m))
        aux_model_formula <- as.formula(paste("x_var ~", 
            con, "+", m))
        if (type == "lm") {
            sigma_xx_model_formula <- as.formula(paste("x_var ~ ", 
                m))
        }
        if (type == "plm") {
            sigma_xx_model_formula <- as.formula(paste("x_var ~ factor(id_var) +", 
                m))
        }
    }
    if (type == "lm") {
        model0 <- lm(model0_formula, data = data, na.action = na.exclude, weights=w_var)
        model1 <- lm(model1_formula, data = data, na.action = na.exclude, weights=w_var)
        aux_model <- lm(aux_model_formula, data = data, na.action = na.exclude, weights=w_var)
        if (m != "none") {
            model_xx <- lm(sigma_xx_model_formula, data = data, 
                na.action = na.exclude, weights=w_var)
        }
    }
    if (type == "plm") {
        model0 <- plm(model0_formula, data = data_plm, model = "within", 
            na.action = na.exclude, weights=w_var)
        model1 <- plm(model1_formula, data = data_plm, model = "within", 
            na.action = na.exclude, weights=w_var)
        aux_model <- plm(aux_model_formula, data = data_plm, 
            model = "within", na.action = na.exclude, weights=w_var)
        model_xx <- lm(sigma_xx_model_formula, data = data, na.action = na.exclude, weights=w_var)
    }
    if (type == "lm") {
        b0 = as.numeric(tidy(model0)[2, 2])
    }
    else if (type == "plm") {
        b0 = as.numeric(tidy(model0)[1, 2])
    }
    if (type == "lm") {
        b1 = as.numeric(tidy(model1)[2, 2])
    }
    else if (type == "plm") {
        b1 = as.numeric(tidy(model1)[1, 2])
    }
    if (type == "lm") {
        R20 = summary(model0)$r.squared
    }
    else if (type == "plm") {
        R20 = as.numeric(summary(model0)$r.squared[1])
    }
    if (type == "lm") {
        R21 = summary(model1)$r.squared
    }
    else if (type == "plm") {
        R21 = as.numeric(summary(model1)$r.squared[1])
    }
    sigma_yy = var(data$y_var, na.rm = T)
    if (m == "none") {
        if (type == "lm") {
            sigma_xx = var(data$x_var, na.rm = T)
        }
        else if (type == "plm") {
            sigma_xx = var(model_xx$residuals)
        }
    }
    else {
        if (type == "lm") {
            sigma_xx = var(model_xx$residuals)
        }
        else if (type == "plm") {
            sigma_xx = var(model_xx$residuals)
        }
    }
    t_x = var(aux_model$residuals)
    bt_m_b = b1 - beta
    rt_m_ro_t_syy = (R21 - R20) * sigma_yy
    b0_m_b1 = b0 - b1
    rm_m_rt_t_syy = (R2max - R21) * sigma_yy
    num1 = bt_m_b * rt_m_ro_t_syy * t_x
    num2 = bt_m_b * sigma_xx * t_x * b0_m_b1^2
    num3 = 2 * bt_m_b^2 * (t_x * b0_m_b1 * sigma_xx)
    num4 = bt_m_b^3 * (t_x * sigma_xx - t_x^2)
    num = num1 + num2 + num3 + num4
    den1 = rm_m_rt_t_syy * b0_m_b1 * sigma_xx
    den2 = bt_m_b * rm_m_rt_t_syy * (sigma_xx - t_x)
    den3 = bt_m_b^2 * (t_x * b0_m_b1 * sigma_xx)
    den4 = bt_m_b^3 * (t_x * sigma_xx - t_x^2)
    den = den1 + den2 + den3 + den4
    delta_star = num/den
    result_delta <- tribble(~Name, ~Value, "delta*", round(delta_star, 
        6), "Uncontrolled Coefficient", b0, "Controlled Coefficient", 
        b1, "Uncontrolled R-square", R20, "Controlled R-square", 
        R21, "Max R-square", R2max, "beta hat", beta)
    if (R21 > R2max) 
        warning("The max R-square value is smaller than the R-square of the controlled model")
    return(result_delta)
}

o_delta_psw(y = "re78",           # dependent variable
x = "treatment",            # independent treatment variable
con = "age + age_sq + educ + educ_sq + married + nodegree + black + hisp + re74 + re75 + re74_sq + re75_sq + u74 + u75 + u74_hisp + u74_black",   # other control variables
w = "w",            # PSW
beta = 0,            # beta
type = "lm",         # model type
R2max = .32136,         # maximum R-square
data = lalonde)   # dataset
