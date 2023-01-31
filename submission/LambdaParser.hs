module LambdaParser where

import Control.Applicative
import Data.Builder
import Data.Lambda
import Data.List.NonEmpty (NonEmpty ((:|)))
import Parser

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Parser combinator taken from tutorials
-}

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = P . const . Error . UnexpectedChar

readInt :: String -> Maybe (Int, String)
readInt s = case reads s of
  [(x, rest)] -> Just (x, rest)
  _ -> Nothing

-- | Parse numbers as int until non-digit
int :: Parser Int
int = P f
  where
    -- This is okay because the case statement is small
    f "" = Error UnexpectedEof
    f x = case readInt x of
      Just (v, rest) -> Result rest v
      Nothing -> Error $ UnexpectedChar (head x)

-- | `chain p op` parses 1 or more instances of `p` separated by `op`
-- | (see chainl1 from Text.Parsec)
-- | This can be a very useful parser combinator
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        ||| pure a

sepby :: Parser a -> Parser s -> Parser [a]
sepby p s = sepby1 p s ||| pure []

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p s = do
  a <- p
  --repeat this part with list
  b <- list (sepby1Help p s)
  pure (a : b)

sepby1Help :: Parser a -> Parser s -> Parser a
sepby1Help p s = do
  _ <- s
  a <- p
  pure a

-- | Write a function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
tok :: Parser a -> Parser a
tok p = p >>= (\r -> spaces >> pure r)

-- | Write a function that parses the given char followed by 0 or more spaces.
charTok :: Char -> Parser Char
charTok c = tok (is c)

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
commaTok :: Parser Char
commaTok = charTok ','

-- | Write a function that parses the given string, followed by 0 or more
stringTok :: String -> Parser String
stringTok s = tok (string s)

-- | Write a function that produces a parser for an array (list)
array :: Parser a -> Parser [a]
array p = do
  is '['
  spaces
  b <- sepby p (tok (is ','))
  spaces
  is ']'
  spaces
  pure b

{-|
    the above Parser combinator taken from tutorials
-}

-- should fail if peek is not a expected element
peek :: Char -> Parser ()
peek v = P f
  where
    f "" = Error UnexpectedEof
    f x = case x of
      (x : xs) -> if x == v then Result (x : xs) () else Error $ UnexpectedChar x
      [] -> Error UnexpectedEof

bool :: Parser Bool
bool = (string "True" >> return True) ||| (string "False" >> return False)

boolTok :: Parser Bool
boolTok = tok bool

intTok :: Parser Int
intTok = tok int

intB :: Parser Builder
intB = intToLam <$> intTok

boolB :: Parser Builder
boolB = boolToLam <$> boolTok

(~) :: Builder -> Builder -> Builder
(~) = ap

(~~) :: Parser Builder -> Parser Builder -> Parser Builder
(~~) = liftA2 ap

fun :: [Char] -> Builder -> Builder
fun xs body = foldr lam body xs

-- choose one from a list of NonEmpty parsers
choose :: NonEmpty (Parser Builder) -> Parser Builder
choose (x :| xs) = foldr (|||) x xs

-- parenthesize
pa :: Parser a -> Parser a
pa = between (charTok '(') (charTok ')')

letter :: Parser Char
letter = oneof ['a' .. 'z']

letterTok :: Parser Char
letterTok = tok letter

{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "xx"
-- UnexpectedChar 'x'

-- BNF: BELOW 
-- <longLam> ::= '('<object>')'
-- <lam> ::= <lam> <term> | <term>
-- <term> ::= '(' <object> ')' | <letter>
-- <object> ::=  λ' <letter> '.' <lam> |  <lam>
-- <letter> ::= [a-z]
longLambdaP :: Parser Lambda
longLambdaP = peek '(' >> build <$> lambda
  where
    lambda = chain termin (pure ap)
    termin = fmap term letter ||| pa object
    object = lambda ||| (lam <$> (charTok 'λ' >> letter) <*> (charTok '.' >> lambda))

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy

-- BNF: BELOW 
-- <shortLam> ::= '(' <lam> ')' |  'λ' <letterList> '.' <lam>
-- <lam> ::= <lam> <term> | <term>
-- <term> ::= '(' <lam> ')' | <letter> | λ' <letterList> '.' <lam>
-- <letterList> ::= <letter> | <letter> <letterList>
-- <letter> ::= 'a' | 'b' ... | 'z'
shortLambdaP :: Parser Lambda
shortLambdaP = (peek '(' ||| peek 'λ') >> build <$> lambda
  where
    lambda = chain termin (pure ap)
    termin = pa lambda ||| fmap term letter ||| (charTok 'λ' >> list letter >>= expandLambda (charTok '.' >> lambda))
    expandLambda body [] = body
    expandLambda body (x : xs) = fmap (lam x) (expandLambda body xs)

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "xx"
-- UnexpectedChar 'x'
lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

-- >>> lamToBool <$> parse logicP "if True then False else True"
-- Result >< Just False

logicP :: Parser Lambda
logicP = build <$> ifB
  where
    ifB = ((stringTok "if" >> ifB) ~~ (stringTok "then" >> ifB) ~~ (stringTok "else" >> ifB)) ||| orB
    orB = chain andB (stringTok "or" >> return (\x y -> lamOr ~ x ~ y))
    andB = chain notB (stringTok "and" >> return (\x y -> lamAnd ~ x ~ y))
    notB = end ||| (ap <$> (stringTok "not" >> return lamNot) <*> notB)
    end = boolB ||| pa ifB

lamTrue :: Builder
lamTrue = boolToLam True

lamFalse :: Builder
lamFalse = boolToLam False

lamIf :: Builder
lamIf = fun "btf" (b ~ t ~ f)
  where
    (b, t, f) = (term 'b', term 't', term 'f')

lamAnd :: Builder
lamAnd = fun "xy" $ lamIf ~ x ~ y ~ lamFalse
  where
    (x, y) = (term 'x', term 'y')

lamOr :: Builder
lamOr = fun "xy" $ lamIf ~ x ~ lamTrue ~ y
  where
    (x, y) = (term 'x', term 'y')

lamNot :: Builder
lamNot = fun "x" $ lamIf ~ x ~ lamFalse ~ lamTrue
  where
    x = term 'x'

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ m
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + (9 - 3) + 2"
-- Result >< Just 13
basicArithmeticP :: Parser Lambda
basicArithmeticP = fmap build expression
  where
    expression = chain end operators
    operators = toOperator lamAdd "+" ||| toOperator lamSub "-"
    end = pa expression ||| intB

toOperator :: Builder -> String -> Parser (Builder -> Builder -> Builder)
toOperator o sym = stringTok sym >> return (\x y -> o ~ x ~ y)

lamSucc :: Builder
lamSucc = fun "nfx" $ f ~ (n ~ f ~ x)
  where
    (n, f, x) = (term 'n', term 'f', term 'x')

lamPred :: Builder
lamPred = fun "nfx" $ n ~ fun "gh" (h ~ (g ~ f)) ~ fun "u" x ~ fun "u" u
  where
    (n, f, x, u) = (term 'n', term 'f', term 'x', term 'u')
    (g, h) = (term 'g', term 'h')

lamAdd :: Builder
lamAdd = fun "xy" $ y ~ lamSucc ~ x
  where
    (x, y) = (term 'x', term 'y')

lamSub :: Builder
lamSub = fun "xy" $ y ~ lamPred ~ x
  where
    (x, y) = (term 'x', term 'y')

lamMultiply :: Builder
lamMultiply = fun "xyf" $ x ~ (y ~ f)
  where
    (x, y, f) = (term 'x', term 'y', term 'f')

lamExp :: Builder
lamExp = fun "xy" (term 'y' ~ term 'x')

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68
arithmeticP :: Parser Lambda
arithmeticP = build <$> expression
  where
    expression = chain factor addOrSub
    factor = chain expo (toOperator lamMultiply "*")
    expo = chain end (toOperator lamExp "**")
    end = intB ||| pa expression
    addOrSub = toOperator lamAdd "+" ||| toOperator lamSub "-"

-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False
complexCalcP :: Parser Lambda
complexCalcP = fmap build ifB
  where
    ifB = (stringTok "if" >> ifB) ~~ (stringTok "then" >> ifB) ~~ (stringTok "else" >> ifB) ||| orB
    orB = chain andB (stringTok "or" >> return (\x y -> lamOr ~ x ~ y))
    andB = chain notB (stringTok "and" >> return (\x y -> lamAnd ~ x ~ y))
    notB = conditionalB ||| boolB ||| pa ifB ||| (ap <$> (stringTok "not" >> return lamNot) <*> notB)
    conditionalB = chain (fmap lamToBuilder arithmeticP) comparisonOperators
    comparisonOperators =
      toOperator lamLessEqual "<="
        ||| toOperator lamEqual "=="
        ||| toOperator lamGreaterEqual ">="
        ||| toOperator lamLessThan "<"
        ||| toOperator lamNotEqual "!="
        ||| toOperator lamGreaterThan ">"

lamEqual :: Builder
lamEqual = fun "mn" $ lamAnd ~ (lamLessEqual ~ term 'm' ~ term 'n') ~ (lamLessEqual ~ term 'n' ~ term 'm')

lamIsZero :: Builder
lamIsZero = fun "n" $ term 'n' ~ fun "x" lamFalse ~ lamTrue

lamNotEqual :: Builder
lamNotEqual = fun "mn" $ lamNot ~ lamEqual

lamLessEqual :: Builder
lamLessEqual = fun "mn" $ lamIsZero ~ (lamSub ~ term 'm' ~ term 'n')

lamLessThan :: Builder
lamLessThan = fun "mn" $ lamAnd ~ (lamNotEqual ~ term 'm' ~ term 'n') ~ (lamLessEqual ~ term 'm' ~ term 'n')

lamGreaterThan :: Builder
lamGreaterThan = fun "mn" $ lamNot ~ (lamLessEqual ~ term 'm' ~ term 'n')

lamGreaterEqual :: Builder
lamGreaterEqual = fun "mn" $ lamNot ~ (lamLessThan ~ term 'm' ~ term 'n')

-- lamGraterEqual :: Builder
-- lamGraterEqual = fun "mn" $ lamLessEqual ~ n ~ m
--   where (m, n) = (term 'm', term 'n')

{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\t_.t)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
listP :: Parser Lambda
listP = fmap build $ arr >>= \xs -> return (foldr (\x y -> lamCons ~ x ~ y) lamNull xs)
  where
    arr = array (fmap lamToBuilder complexCalcP)

lamNull :: Builder
lamNull = fun "cn" $ term 'n'

lamCons :: Builder
lamCons = fun "htcn" $ c ~ h ~ (t ~ c ~ n)
  where
    (c, n, t, h) = (term 'c', term 'n', term 't', term 'h')

-- | listOpP
-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
--
-- >>> lamToInt <$> parse listOpP "head cons 5 [1, 2, 3]"
-- Result >< Just 5
listOpP :: Parser Lambda
listOpP = fmap build listB
  where
    listB =
      (consB ~~ element ~~ listB)
        ||| (listOperation ~~ listB)
        ||| fmap lamToBuilder listP
        ||| pa listB
    element = fmap lamToBuilder complexCalcP
    consB = stringTok "cons" >> return lamCons
    listOperation =
      choose
        ( (stringTok "isNull" >> return lamIsNUll)
            :| [ stringTok "head" >> return lamHead,
                 (stringTok "tail" ||| stringTok "rest") >> return lamTail])

lamIsNUll :: Builder
lamIsNUll = fun "l" $ term 'l' ~ fun "ht" lamFalse ~ lamTrue

lamHead :: Builder
lamHead = fun "l" $ l ~ fun "ht" h ~ lamFalse
  where
    (l, h) = (term 'l', term 'h')

lamTail :: Builder
lamTail = fun "lcn" $ l ~ fun "htg" (g ~ h ~ (t ~ c)) ~ fun "t" n ~ fun "ht" t
  where
    (l, c, n, t, g, h) = (term 'l', term 'c', term 'n', term 't', term 'g', term 'h')

-- | Exercise 2

-- | Implement your function(s) of choice below!
-- >>> lamToInt <$> parse extensionP "head (map succ) (cons 5) [1, 2, 3]"
-- Result >< Just 6
--
-- >>> lamToInt <$> parse extensionP "head rest rest [12,3,4]"
-- Result >< Just 4
--
-- >>> lamToInt <$> parse extensionP "head (map (add 2)) [12,3,4]"
-- Result >< Just 14
--
-- >>> lamToInt <$> parse extensionP "head (map (add 2)) (filter (greaterThan 10)) [12,3,4]"
-- Result >< Just 5
--
-- >>> lamToInt <$> parse extensionP "(foldr add) [12,3,4]"
-- Result >< Just 19
--
-- >>> lamToInt <$> parse extensionP "factorial 5"
-- Result >< Just 120
extensionP :: Parser Lambda
extensionP = fmap build extensionB
  where
    extensionB = (element ~~ extensionB) ||| element
    element =
      fmap lamToBuilder complexCalcP
        ||| fmap lamToBuilder listP
        ||| function
        ||| pa extensionB

    function =
      choose
        ( (stringTok "isNull" >> return lamIsNUll)
            :| [ stringTok "head" >> return lamHead,
                 (stringTok "tail" ||| stringTok "rest") >> return lamTail,
                 stringTok "add" >> return lamAdd,
                 stringTok "sub" >> return lamSub,
                 stringTok "multiply" >> return lamMultiply,
                 stringTok "not" >> return lamNot,
                 stringTok "succ" >> return lamSucc,
                 stringTok "equal" >> return lamEqual,
                 stringTok "lessThan" >> return lamLessThan,
                 stringTok "greaterThan" >> return lamGreaterThan,
                 stringTok "factorial" >> return lamFactorial,
                 stringTok "foldr" >> return lamFoldr,
                 stringTok "pred" >> return lamPred,
                 stringTok "cons" >> return lamCons,
                 stringTok "filter" >> return lamFilter,
                 stringTok "map" >> return lamMap
               ])

    (f, x, y, c, n, l, h, t, g) = (term 'f', term 'x', term 'y', term 'c', term 'n', term 'l', term 'h', term 't', term 'g')

    -- recursive list functions
    lamY :: Builder
    lamY = fun "f" (lam 'x' (f ~ (x ~ x)) ~ lam 'x' (f ~ (x ~ x)))
    lamMap = lamY ~ fun "hfl"
          ( lamIf
              ~ (lamIsNUll ~ l)
              ~ l
              ~ (lamCons ~ (f ~ (lamHead ~ l)) ~ (h ~ f ~ (lamTail ~ l))))

    lamFilter = lamY ~ fun "hfl"
          ( lamIf ~ (lamIsNUll ~ l) ~ l ~ ( lamIf
                    ~ (f ~ (lamHead ~ l))
                    ~ (lamCons ~ (lamHead ~ l) ~ (h ~ f ~ (lamTail ~ l)))
                    ~ (h ~ f ~ (lamTail ~ l))))
    lamFoldr = lamY ~ fun "hfl"
          ( lamIf
              ~ (lamIsNUll ~ l)
              ~ l
              ~ (f ~ (lamHead ~ l) ~ (h ~ f ~ (lamTail ~ l))))

    -- other functions
    lamFactorial = lamY ~ fun "hn"
          ( lamIf
              ~ (lamEqual ~ n ~ intToLam 0)
              ~ intToLam 1
              ~ (lamMultiply ~ n ~ (h ~ (lamSub ~ n ~ intToLam 1))))
