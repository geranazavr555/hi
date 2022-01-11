{-# LANGUAGE ScopedTypeVariables #-}

module HW3.Parser where

import Control.Monad (void)
import Control.Monad.Combinators ((<|>), optional, manyTill)
import Data.Void (Void)
import HW3.Base
import qualified Data.Text as T
import Text.Megaparsec (Parsec, choice, hidden, skipMany, try, runParser, eof, between, sepBy1, (<?>), notFollowedBy)
import Text.Megaparsec.Char (space1, string, char)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Data.ByteString as B
import Data.Word (Word8)

type Parser = Parsec Void String

sc :: Parser ()
sc = skipMany $ hidden space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

optionalParens :: Parser a -> Parser a
optionalParens parser = do
  lb <- optional . try $ symbol "("
  case lb of
    Just _ -> do
      res <- optionalParens parser
      void $ symbol ")"
      return res
    Nothing -> parser

pBuiltInFunc :: Parser HiValue
pBuiltInFunc = HiValueFunction <$> choice [
    HiFunDiv <$ symbol "div",
    HiFunMul <$ symbol "mul",
    HiFunAdd <$ symbol "add",
    HiFunSub <$ symbol "sub",
    HiFunAnd <$ symbol "and",
    HiFunOr <$ symbol "or",
    HiFunLessThan <$ symbol "less-than",
    HiFunGreaterThan <$ symbol "greater-than",
    HiFunEquals <$ symbol "equals",
    HiFunNotLessThan <$ symbol "not-less-than",
    HiFunNotGreaterThan <$ symbol "not-greater-than",
    HiFunNotEquals <$ symbol "not-equals",
    HiFunIf <$ symbol "if",
    HiFunNot <$ symbol "not",
    HiFunLength <$ symbol "length",
    HiFunToUpper <$ symbol "to-upper",
    HiFunToLower <$ symbol "to-lower",
    HiFunReverse <$ symbol "reverse",
    HiFunTrim <$ symbol "trim",
    HiFunList <$ symbol "list",
    HiFunRange <$ symbol "range",
    HiFunFold <$ symbol "fold",
    HiFunPackBytes <$ symbol "pack-bytes",
    HiFunUnpackBytes <$ symbol "unpack-bytes",
    HiFunZip <$ symbol "zip",
    HiFunUnzip <$ symbol "unzip",
    HiFunEncodeUtf8 <$ symbol "encode-utf8",
    HiFunDecodeUtf8 <$ symbol "decode-utf8",
    HiFunSerialise <$ symbol "serialise",
    HiFunDeserialise <$ symbol "deserialise"
  ] <?> "function"

pNumberLiteral :: Parser HiValue
pNumberLiteral = (do
  number <- L.signed sc (lexeme L.scientific)
  return $ HiValueNumber (toRational number)) <?> "number"

pBoolLiteral :: Parser HiValue
pBoolLiteral = (HiValueBool <$> choice [
    True <$ symbol "true",
    False <$ symbol "false"
  ]) <?> "bool"

pNullLiteral :: Parser HiValue
pNullLiteral = HiValueNull <$ symbol "null"

pStringLiteral :: Parser HiValue
pStringLiteral = do
  void $ char '"'
  res <- manyTill L.charLiteral (char '"')
  sc
  return $ HiValueString (T.pack res)

pByteLiteral :: Parser Word8
pByteLiteral = do
  let (pBytePart :: Parser Word8) = choice [
                                      0 <$  char '0',
                                      1 <$  char '1',
                                      2 <$  char '2',
                                      3 <$  char '3',
                                      4 <$  char '4',
                                      5 <$  char '5',
                                      6 <$  char '6',
                                      7 <$  char '7',
                                      8 <$  char '8',
                                      9 <$  char '9',
                                      10 <$ char 'a',
                                      11 <$ char 'b',
                                      12 <$ char 'c',
                                      13 <$ char 'd',
                                      14 <$ char 'e',
                                      15 <$ char 'f'
                                    ]
  first <- pBytePart
  second <- pBytePart
--  space1 <* notFollowedBy (string "#")
  sc
  return $ first * 16 + second


pBytesLiteral :: Parser HiValue
pBytesLiteral = HiValueBytes . B.pack <$> (symbol "[#" >> manyTill pByteLiteral (symbol "#]"))
--pBytesLiteral = try (do
--    void $ symbol "[#"
--    void $ symbol "#]"
--    return $ HiValueBytes B.empty
--  ) <|> (do
--    bytes <- between (symbol "[#") (symbol "#]") (pByteLiteral `sepBy1` (char ' '))
--    return $ HiValueBytes $ B.pack bytes
--  )

pListLiteral :: Parser HiExpr
pListLiteral = try (do
    void $ symbol "["
    void $ symbol "]"
    return $ HiExprApply (HiExprValue $ HiValueFunction $ HiFunList) []
  ) <|> (do
    args <- between (symbol "[") (symbol "]") (pOperator `sepBy1` (symbol ","))
    return $ HiExprApply (HiExprValue $ HiValueFunction $ HiFunList) args
  )

pHiValue :: Parser HiValue
pHiValue = (pNumberLiteral <|> pBoolLiteral <|> pBuiltInFunc <|> pNullLiteral <|> pStringLiteral <|> pBytesLiteral)
  <?> "value"

pArgsList :: Parser [HiExpr]
pArgsList = (parens $ pOperator `sepBy1` (symbol ",")) <?> "args list"

applyArgs :: HiExpr -> Parser HiExpr
applyArgs hiExpr = (do
  args <- pArgsList
  let exprApply = HiExprApply hiExpr args
  (applyArgs exprApply) <|> (return $ exprApply)) <?> "apply args"


pTerm :: Parser HiExpr
pTerm = choice [
    parens pOperator,
    HiExprValue <$> pHiValue,
    pListLiteral
  ]


pHiExpr :: Parser HiExpr
pHiExpr = (do
    hiValue <- pTerm
    (applyArgs hiValue) <|> (return hiValue)
  ) <?> "expression"

binaryOpL :: String -> HiFun -> Operator Parser HiExpr
binaryOpL name hiFun = InfixL ((\a b -> HiExprApply (HiExprValue (HiValueFunction hiFun)) [a, b]) <$ symbol name)

binaryOpLTry :: String -> HiFun -> String -> Operator Parser HiExpr
binaryOpLTry name hiFun nfb = InfixL ((\a b -> HiExprApply (HiExprValue (HiValueFunction hiFun)) [a, b]) <$
  (try $ lexeme $ string name <* notFollowedBy (string nfb)))

binaryOpR :: String -> HiFun -> Operator Parser HiExpr
binaryOpR name hiFun = InfixR ((\a b -> HiExprApply (HiExprValue (HiValueFunction hiFun)) [a, b]) <$ symbol name)

binaryOpN :: String -> HiFun -> Operator Parser HiExpr
binaryOpN name hiFun = InfixN ((\a b -> HiExprApply (HiExprValue (HiValueFunction hiFun)) [a, b]) <$ symbol name)

opTable :: [[Operator Parser HiExpr]]
opTable = [
    [
      binaryOpLTry "/" HiFunDiv "=",
      binaryOpL "*" HiFunMul
    ],
    [
      binaryOpL "+" HiFunAdd,
      binaryOpL "-" HiFunSub
    ],
    [
      binaryOpN "<=" HiFunNotGreaterThan,
      binaryOpN "<"  HiFunLessThan,
      binaryOpN ">=" HiFunNotLessThan,
      binaryOpN ">"  HiFunGreaterThan,
      binaryOpN "/=" HiFunNotEquals,
      binaryOpN "==" HiFunEquals
    ],
    [
      binaryOpR "&&" HiFunAnd
    ],
    [
      binaryOpR "||" HiFunOr
    ]
  ]

pOperator :: Parser HiExpr
pOperator = makeExprParser pHiExpr opTable <?> "operator"

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (sc *> pOperator <* eof) "Parser.hs"