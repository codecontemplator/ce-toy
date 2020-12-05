# What is CE-toy?

CE-toy is a toy implementation of a credit evaluation service. Credit evaluation is the business process that takes place when one or more applicants apply for a loan (or some other event that require credits). Data about the applicant(s) are retrieved and evaluated according to a fixed framework called an credit process.

# Overall design

In CE-toy, a credit evaluation has been separated into two concerns:

* Evaluation of credit process rules
* Loading of applicant data

These topics are treated in separate sections below. One of the benefits of this separation is that the impure loading of data is kept separate from the pure domain logic related to evaluation of given credit process rules.

# Evaluation of credit process rules

Credit rules might be intricate and to express them we need an expressive language. One option is to build up a new specialized language from scratch but that is both time consuming and hard. A common solution is instead to create a DSL (domain specific language) within the application programming language. That is the approach used here. A requirement from the data loading is that it must be possible to analyze the rules to find out which applicant data parameters that is used and thus might need to be loaded. One solution that satisfies these constraints is the free monad interpreter pattern. It might sound scary but when to use it does not require a deep understanding of the underlying mechanism. Instead it can just be seen as a pattern to follow.

The proposed DSL consists of *rules* and *rule expressions*. Rules are basically named rule expressions that can be linked together in a chain. 

```haskell
data Rule = Rule String (RuleExpr Amount) | RAndThen Rule Rule
```

A rule expression must be able to express retrieval of data parameters and the requested credit amount. Data parameters might be of different types. String and int values are currently supported.

```haskell
data RuleExprF a = 
      GetIntValue Key (Int -> a) 
    | GetStringValue Key (String -> a)
    | GetAmount (Amount -> a)  
        deriving Functor

type RuleExpr = Free RuleExprF
```

The definition is a bit cryptic but lets ignore that and just accept it as a pattern. 

In order to get a proper DSL and not just data constructors the following helper functions are defined:

```haskell
rule :: String -> RuleExpr Amount -> Rule
rule name expr = Rule name expr

andThen :: Rule -> Rule -> Rule
andThen r1 r2 = RAndThen r1 r2

getIntValue :: Key -> RuleExpr Int
getIntValue key = liftF $ GetIntValue key id

getStringValue :: Key -> RuleExpr String
getStringValue key = liftF $ GetStringValue key id

getAmount :: RuleExpr Amount
getAmount = liftF $ GetAmount id
```

We can now benefit from the trick used in the definition of the *rule expressions* and build up an entire credit process. It might look something like below.

```haskell
absoluteMaxAmount :: Amount -> RuleExpr Amount
absoluteMaxAmount amountLimit = do
    amount <- getAmount
    return $ min amount amountLimit
        
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
    rule "maxTotalDebt" (maxTotalDebt 500)
```
## Version control

Since the credit evaluation process is expressed in a file we can simply use git for versioning control.

# Loading of a applicant data
