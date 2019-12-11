b_raw = read.csv("~/w241/prj/w241_prj_analysis/FINALFINAL.xlsx - Sheet1.csv")
summary(b_raw)

library(blockTools)

#Get rid of the ones who have their care proxied.
b_int = b_raw[is.na(b_raw$CARE_PROXIED_EXCLUDE),]
b = b_int[is.na(b_int$ADD_PROXIED_EXCLUDE),]

b[is.na(b$MALE), "MALE"] = 0
b[is.na(b$ADDRESS_PROXY_COUNT), "ADDRESS_PROXY_COUNT"] = 0
b[is.na(b$Drug.Count), "Drug.Count"] = 0


blocks = block(b, groups = "HAS_CELL_PHONE", n.tr = 2, id.vars = c("UniqueID"), block.vars = c("X3MONTH_ACTIVE", "CARER_PROXY_COUNT", "ADDRESS_PROXY_COUNT"), verbose = TRUE, level.two = FALSE)

# Magic seed to make sure we pass balance checks...
set.seed(12345678)
assign = assignment(blocks)

outCSV(assign)

print(assign)

# These are valid
assg2xBalance(assign, b, "UniqueID", c("MALE"))
assg2xBalance(assign, b, "UniqueID", c("Drug.Count"))
assg2xBalance(assign, b, "UniqueID", c("X3MONTH_ACTIVE"))
assg2xBalance(assign, b, "UniqueID", c("CARER_PROXY_COUNT"))
assg2xBalance(assign, b, "UniqueID", c("ADDRESS_PROXY_COUNT"))


save(assign, file = "~/w241/prj/assign.Rdata")


# Group 1 = No Phone (34), Group 2 = Has Cell Phone (123).
# Group 1: Treatment 1 = CONTROL, Treatment 2 = DISCOUNT
# Group 2: Treatment 1 = SMS, Treatment 2 = No SMS
testgroup = 1

# Extract Group 1 treatments

assg.gp <- assign$assg[[testgroup]]

tr1.idx <- assg.gp[, 1]
tr2.idx <- assg.gp[, 2]

tr.vec <- rep(NA, nrow(b))
tr.vec[b[["UniqueID"]] %in% tr1.idx] <- 1
tr.vec[b[["UniqueID"]] %in% tr2.idx] <- 0

wh.gp <- tr.vec %in% c(0, 1)

data.tr.gp <- data.frame(cbind(b[wh.gp, ], Tr = tr.vec[wh.gp]))
data.tr.gp$Trfac = factor(data.tr.gp$Tr)

write.csv(data.tr.gp, file = "~/w241/prj/group1.csv")

group1tr1 = data.tr.gp[data.tr.gp$Tr == 0,]   # Control
group1tr2 = data.tr.gp[data.tr.gp$Tr == 1,]   # Discount


# Extract Group 2 treatments

testgroup = 2
assg.gp <- assign$assg[[testgroup]]

tr1.idx <- assg.gp[, 1]
tr2.idx <- assg.gp[, 2]

tr.vec <- rep(NA, nrow(b))
tr.vec[b[["UniqueID"]] %in% tr1.idx] <- 1
tr.vec[b[["UniqueID"]] %in% tr2.idx] <- 0

wh.gp <- tr.vec %in% c(0, 1)

data.tr.gp <- data.frame(cbind(b[wh.gp, ], Tr = tr.vec[wh.gp]))
data.tr.gp$Trfac = factor(data.tr.gp$Tr)

group2tr1 = data.tr.gp[data.tr.gp$Tr == 0,]   # SMS
group2tr2 = data.tr.gp[data.tr.gp$Tr == 1,]   # No SMS


# Split Group 2 SMS into discount treatment/control

set.seed(111)

# n.tr is 3 but we'll use one group as control and two as identical treatments
blocks2_1 = block(group2tr1, n.tr = 3, id.vars = c("UniqueID"), block.vars = c("X3MONTH_ACTIVE", "CARER_PROXY_COUNT", "ADDRESS_PROXY_COUNT"), verbose = TRUE, level.two = FALSE)
assign2_1 = assignment(blocks2_1)

# Split Group 2 No-SMS into discount treatment/control

# n.tr is 3 but we'll use one group as control and two as identical treatments
blocks2_2 = block(group2tr2, n.tr = 3, id.vars = c("UniqueID"), block.vars = c("X3MONTH_ACTIVE", "CARER_PROXY_COUNT", "ADDRESS_PROXY_COUNT"), verbose = TRUE, level.two = FALSE)
assign2_2 = assignment(blocks2_2)


# Extract Group 2 SMS

assg.gp <- assign2_1$assg[[1]]

tr1.idx <- assg.gp[, 1]
tr2.idx <- assg.gp[, 2]
co.idx <- assg.gp[, 3]

tr.vec <- rep(NA, nrow(group2tr1))
tr.vec[group2tr1[["UniqueID"]] %in% tr1.idx] <- 1
tr.vec[group2tr1[["UniqueID"]] %in% tr2.idx] <- 1
tr.vec[group2tr1[["UniqueID"]] %in% co.idx] <- 0

wh.gp <- tr.vec %in% c(0, 1, 2)

#TrB is discount
data.tr.gp <- data.frame(cbind(group2tr1[wh.gp, ], TrB = tr.vec[wh.gp]))
data.tr.gp$TrBfac = factor(data.tr.gp$TrB)

write.csv(data.tr.gp, file = "~/w241/prj/group2_SMS.csv")

group2tr1_tr1 = data.tr.gp[data.tr.gp$TrB == 0,] # SMS, no discount
group2tr1_tr2 = data.tr.gp[data.tr.gp$TrB == 1,] # SMS, discount

# Re-balance testing

library(RItools)
xBalance(TrB ~ MALE, data = data.tr.gp, report = "all")
xBalance(TrB ~ Drug.Count, data = data.tr.gp, report = "all")
xBalance(TrB ~ X3MONTH_ACTIVE, data = data.tr.gp, report = "all")
xBalance(TrB ~ CARER_PROXY_COUNT, data = data.tr.gp, report = "all")
xBalance(TrB ~ ADDRESS_PROXY_COUNT, data = data.tr.gp, report = "all")

# Another way to do the test...

library(lmtest)
testlm = lm(MALE ~ TrBfac, data = data.tr.gp)
coeftest(testlm)
confint(testlm)

testlm = lm(Drug.Count ~ TrBfac, data = data.tr.gp)
coeftest(testlm)
confint(testlm)


# Extract Group 2 No-SMS

assg.gp <- assign2_2$assg[[1]]

tr1.idx <- assg.gp[, 1]
tr2.idx <- assg.gp[, 2]
co.idx <- assg.gp[, 3]

tr.vec <- rep(NA, nrow(group2tr2))
tr.vec[group2tr2[["UniqueID"]] %in% tr1.idx] <- 1
tr.vec[group2tr2[["UniqueID"]] %in% tr2.idx] <- 1
tr.vec[group2tr2[["UniqueID"]] %in% co.idx] <- 0

wh.gp <- tr.vec %in% c(0, 1, 2)

#TrB is discount
data.tr.gp <- data.frame(cbind(group2tr2[wh.gp, ], TrB = tr.vec[wh.gp]))
data.tr.gp$TrBfac = factor(data.tr.gp$TrB)

write.csv(data.tr.gp, file = "~/w241/prj/group2_NoSMS.csv")

group2tr2_tr1 = data.tr.gp[data.tr.gp$TrB == 0,] # No-SMS, no discount
group2tr2_tr2 = data.tr.gp[data.tr.gp$TrB == 1,] # No-SMS, discount


# Re-Balance testing

xBalance(TrB ~ MALE, data = data.tr.gp, report = "all")
xBalance(TrB ~ Drug.Count, data = data.tr.gp, report = "all")
xBalance(TrB ~ X3MONTH_ACTIVE, data = data.tr.gp, report = "all")
xBalance(TrB ~ CARER_PROXY_COUNT, data = data.tr.gp, report = "all")
xBalance(TrB ~ ADDRESS_PROXY_COUNT, data = data.tr.gp, report = "all")


# Another way to do the test...

library(lmtest)
testlm = lm(MALE ~ TrBfac, data = data.tr.gp)
coeftest(testlm)
confint(testlm)

testlm = lm(Drug.Count ~ TrBfac, data = data.tr.gp)
coeftest(testlm)
confint(testlm)

