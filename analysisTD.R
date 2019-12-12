library(lmtest)


d = read.csv("/home/draget/w241/prj/w241_prj_analysis/PrelimRESULTSTD.csv")
#d = read.csv("C:\\Users\\uclj\\Downloads\\w241_prj_analysis-master\\PrelimRESULTSTD.csv")

# Get rid of the lines which were not allocated to a treatment/control group.
d = d[d$TREATMENT != "", ]
d$TREATMENT = factor(d$TREATMENT)

# Binarise the outcome variables.
d$ACTIVE_IN._EXPERIMENT[is.na(d$ACTIVE_IN._EXPERIMENT)] = 0
d$VOUCHER[is.na(d$VOUCHER)] = 0

# Cleanup independents.
d$MALE[is.na(d$MALE)] = 0

cg = 100
for(i in 1:nrow(d)) {
  if(is.na(d[i, "CARER.GROUP"])) {
    d[i, "CARER.GROUP"] = cg
    cg = cg + 1
  }
}

d$CARER.GROUP = factor(d$CARER.GROUP)
d$MALE = factor(d$MALE)
d$Address.GROUP = factor(d$Address.GROUP)


special_sum = function(x) {
  
  if(is.factor(x)) {
    return(x[1])
  }
  else {
    return(sum(x))
  }
  
}

d = aggregate(d, by = list(d$CARER.GROUP), FUN = special_sum)

# Create derived dummys.
d$DiscountTreatment = d$TREATMENT == "GROUP 1 TREATMENT" | d$TREATMENT == "Tr1.1 SMS & TREATMENT" | d$TREATMENT == "Tr2.1 NO SMS TREATMENT"
d$SMS = d$TREATMENT == "Tr1.0 SMS & CONTROL" | d$TREATMENT == "Tr1.1 SMS & TREATMENT"
d$Conversion = d$ACTIVE_IN._EXPERIMENT == 1 & d$X3MONTH_ACTIVE.NEW. == 0
d$DropOut = d$ACTIVE_IN._EXPERIMENT == 0 & d$X3MONTH_ACTIVE.NEW. == 1
d$Consistent = d$ACTIVE_IN._EXPERIMENT == 1 & d$X3MONTH_ACTIVE.NEW. == 1


# Unexpected result - No SMS group appears to have done significantly better in terms of activity.
# Are our SMS offputting? (not consistent with lm6) 
# Or is this reflective of people who got an SMS re-ordering by SMS reply instead of app?
lm1 = lm(ACTIVE_IN._EXPERIMENT ~ TREATMENT, data = d)
coeftest(lm1)

# No significant result, mixed data.
lm2 = lm(VOUCHER ~ TREATMENT, data = d)
coeftest(lm2)

# Significant 0.1 level, however it only tells us that there was an estimable uptake rate 
# for those offered a voucher.
lm3 = lm(VOUCHER ~ DiscountTreatment, data = d)
coeftest(lm3)

# No significant relationship for activity and treatments.
lm4 = lm(ACTIVE_IN._EXPERIMENT ~ SMS + DiscountTreatment + SMS*DiscountTreatment, data = d)
coeftest(lm4)

# No significant effect on user-conversion.
lm5 = lm(Conversion ~ SMS + DiscountTreatment, data = d)
coeftest(lm5)

# No significant effect on user dropout.
lm6 = lm(DropOut ~ SMS + DiscountTreatment, data = d)
coeftest(lm6)

# No significant effect on user retention.
lm7 = lm(Consistent ~ SMS + DiscountTreatment, data = d)
coeftest(lm7)

# Users before seem to be users after at 66% rate (significant).
lm8 = lm(ACTIVE_IN._EXPERIMENT ~ X3MONTH_ACTIVE.NEW. + SMS + DiscountTreatment, data = d)
coeftest(lm8)

# For those in discount group, no significant improvement with SMS.
# ...although it is a positive coefficient at least.
d9 = d[d$DiscountTreatment == TRUE, ]
lm9 = lm(VOUCHER ~ SMS, data = d9)
coeftest(lm9)

# Interesting result - Males significantly Drop Out of app usage unless offered a discount.
lm10 = lm(DropOut ~ MALE + SMS + DiscountTreatment + SMS*MALE + DiscountTreatment*MALE, data = d)
coeftest(lm10)

# Male correlated with negative consistency (per above)
lm11 = lm(Consistent ~ MALE, data = d)
coeftest(lm11)

# Similar finding to above 2.
lm12 = lm(ACTIVE_IN._EXPERIMENT ~ X3MONTH_ACTIVE.NEW. + SMS + DiscountTreatment + MALE + DiscountTreatment*MALE, data = d)
coeftest(lm12)
