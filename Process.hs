module Process where

import Types
import Dsl

absoluteMaxAmount :: Amount -> RuleExpr Amount
absoluteMaxAmount amountLimit = do
    amount <- getAmount
    if amount > amountLimit then 
        return amountLimit
    else
        return amount
    
maxAmountForAge :: Amount -> Age -> RuleExpr Amount
maxAmountForAge amountLimit ageLimit = do
    age <- getIntValue "Age"
    amount <- getAmount
    if age > ageLimit && amount > amountLimit then
        return amountLimit
    else
        return amount
    
maxTotalDebt :: Int -> RuleExpr Amount
maxTotalDebt debtLimit = do
    creditA <- getIntValue "CreditA"
    creditB <- getIntValue "CreditB"
    if creditA + creditB > debtLimit then
        return 0
    else
        getAmount
    
ceProcessExample :: Rule
ceProcessExample =
    rule "absoluteMaxAmount" (absoluteMaxAmount 1000) `andThen`
    rule "maxAmountForAge" (maxAmountForAge 750 85) `andThen`
    rule "maxTotalDebt" (maxTotalDebt 500)    
