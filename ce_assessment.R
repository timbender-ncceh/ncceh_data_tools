# next----
ce2 <- data.table::melt(as.data.table(ce), 
                 id.vars = c("Client ID", "Household ID", "Entry Date", 
                             "Exit Date", "Race", "Ethnicty", "Gender", 
                             "Region", "Provider", "Provider Updating", 
                             "Note")) %>%
  as.data.frame() %>%
  as_tibble()

ce2$variable %>% levels()
ce2$value %>% unique()

ce2

as.data.table(ce2) %>%
  dcast(variable ~ `Client ID`) %>%
  as.data.frame()  %>% as_tibble()


#https://trevorfrench.github.io/R-for-Data-Analysis/p4c2-regression.html
data("faithful")
head(faithful)

lm.oldfaith <- lm(faithful$eruptions ~ 
                    faithful$waiting)
View(summary(lm.oldfaith))

fun_eruptions <- function(waiting){
  out.eruptions <- (waiting * 
    as.data.frame(summary(lm.oldfaith)[["coefficients"]])$Estimate[2]) + 
    as.data.frame(summary(lm.oldfaith)[["coefficients"]])$Estimate[1]
  return(out.eruptions)
}
  
fun_eruptions(65)




library(ggplot2)

ggplot() + 
  geom_point(data = faithful, 
             aes(x = waiting, y = eruptions), 
             color = "grey") +
  geom_point(data = data.frame(waiting = 40:100, 
                               eruptions = unlist(lapply(X = 40:100, 
                                                         FUN = fun_eruptions))), 
             aes(x = waiting, y = eruptions)) +
  scale_x_continuous(name = "waiting [independent]\n(time between eruptions)")+
  scale_y_continuous(name = "eruptions [dependent]\n(duration of eruption)")
?faithful


# https://trevorfrench.github.io/R-for-Data-Analysis/p4c2-regression.html#multiple-regression
data("mtcars")
head(mtcars)

lmc <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb
         , data = mtcars)
summary(lmc)

# https://trevorfrench.github.io/R-for-Data-Analysis/p4c2-regression.html#logistic-regression
# logistic regression

head(mtcars)

glm1 <- glm(am ~ cyl + hp + wt, family = binomial, data = mtcars)
summary(glm1)
plot(glm1)


faithful
head(faithful)

glm(waiting <= 60 ~ eruptions + waiting, 
    data = faithful) %>% plot()

# factors
library(forcats)
data("gss_cat")

fct_count(gss_cat$relig)
fct_count(fct_lump(gss_cat$relig))

# top 6 religions
fct_lump_n(f = gss_cat$relig, 
           n = 6) %>% fct_count

# religions with at least 1% representation
fct_lump_min(f = gss_cat$relig, 
           min = nrow(gss_cat)/100) %>% fct_count

# prop at least 1%
fct_lump_prop(f = gss_cat$relig, 
           prop = 0.01) %>% fct_count

fct_lump_lowfreq(f = gss_cat$relig) %>% fct_count
fct_lump_lowfreq(f = gss_cat$marital) %>% fct_count
fct_lump_lowfreq(f = gss_cat$race) %>% fct_count
fct_lump_lowfreq(f = gss_cat$rincome) %>% fct_count
fct_lump_lowfreq(f = gss_cat$partyid) %>% fct_count
fct_lump_lowfreq(f = gss_cat$denom) %>% fct_count
