{-# LANGUAGE RebindableSyntax #-}

module Process where

import Prelude
import Types
import Dsl

conditionalLookupTest :: Amount -> RuleExpr Amount
conditionalLookupTest amountLimit = do
    age <- getIntValue "Age"
    if age < 20 then do
        amount <- getAmount
        acceptLimit $ min amount amountLimit
    else do
        name <- getStringValue "Name"
        amount <- getAmount
        acceptLimit $ amount + length name + 123

absoluteMaxAmount :: Amount -> RuleExpr Amount
absoluteMaxAmount amountLimit = do
    amount <- getAmount
    acceptLimit $ min amount amountLimit
    
maxAmountForAge :: Amount -> Age -> RuleExpr Amount
maxAmountForAge amountLimit ageLimit = do
    age <- getIntValue "Age"
    amount <- getAmount
    if age > ageLimit && amount > amountLimit then
        acceptLimit amountLimit
    else
        accept
    
maxTotalDebt :: Int -> RuleExpr Amount
maxTotalDebt debtLimit = do
    creditA <- getIntValue "CreditA"
    creditB <- getIntValue "CreditB"
    if creditA + creditB > debtLimit then
        reject
    else
        accept

hasAddress :: RuleExpr Amount
hasAddress = do
    address <- getStringValue "Address"
    if address == "" then
        reject
    else
        accept

ceProcessExample :: Rule
ceProcessExample =
    rule "conditionalLookupTest" (conditionalLookupTest 1000) `andThen`
    rule "absoluteMaxAmount" (absoluteMaxAmount 1000) `andThen`
    rule "maxAmountForAge" (maxAmountForAge 750 85) `andThen`
    rule "maxTotalDebt" (maxTotalDebt 500) `andThen`
    rule "hasAddress" hasAddress   
