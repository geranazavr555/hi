{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module HW3.Evaluator
  ( eval,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.Text as T
import HW3.Base
import Data.Semigroup (stimes)
import Data.Ratio (denominator, numerator)
import Data.Sequence (Seq(..), (<|), fromList, length, reverse, (><), singleton, index, length, take, drop)
import qualified Data.Word as W
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Codec.Compression.Zlib as Z
import Codec.Serialise as S

type EvaluatorT m = ExceptT HiError m HiValue
type EvaluatorTypedT m a = ExceptT HiError m a

runEvaluatorT :: EvaluatorT m -> m (Either HiError HiValue)
runEvaluatorT = runExceptT

evalFunctionArg :: Monad m => HiExpr -> EvaluatorT m
evalFunctionArg (HiExprApply hiExprFunc hiExprArgsList) = evalHiExprApply hiExprFunc hiExprArgsList
evalFunctionArg (HiExprValue x) = return x

applyNumberFunc :: Monad m => (Rational -> Rational -> Rational) -> Rational -> Rational -> EvaluatorT m
applyNumberFunc f a b = pure $ HiValueNumber $ f a b

applyBoolFunc :: Monad m => (a -> a -> Bool) -> a -> a -> EvaluatorT m
applyBoolFunc f a b = pure $ HiValueBool $ f a b

evalBinaryNumberFunNoThrow :: Monad m => (Rational -> Rational -> Rational) -> HiExpr -> HiExpr -> EvaluatorT m
evalBinaryNumberFunNoThrow f a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  case arg1 of
    (HiValueNumber x) -> case arg2 of
      (HiValueNumber y) -> applyNumberFunc f x y
      _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalBinaryBoolFunNoThrow :: Monad m => (Bool -> Bool -> Bool) -> Bool -> HiExpr -> EvaluatorT m
evalBinaryBoolFunNoThrow f a b = do
  arg2 <- evalFunctionArg b
  case arg2 of
    (HiValueBool y) -> applyBoolFunc f a y
    _ -> throwError HiErrorInvalidArgument
    
evalBinaryCompare :: Monad m => (HiValue -> HiValue -> Bool) -> HiExpr -> HiExpr -> EvaluatorT m
evalBinaryCompare f a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  applyBoolFunc f arg1 arg2

getContainerMulArg :: Rational -> Maybe Integer
getContainerMulArg x = if denominator x == 1 then Just (numerator x) else Nothing

evalFold :: Monad m => HiFun -> Seq HiValue -> EvaluatorT m
evalFold hiFun (a :<| b :<| Empty) = evalBinaryFun hiFun (HiExprValue a) (HiExprValue b)
evalFold hiFun (a :<| b :<| rest) = do
  newHead <- evalBinaryFun hiFun (HiExprValue a) (HiExprValue b)
  evalFold hiFun (newHead <| rest)
-- evalFold _ (x :<| Empty) = return x
evalFold _ _ = return HiValueNull

evalBinaryFun :: Monad m => HiFun -> HiExpr -> HiExpr -> EvaluatorT m
evalBinaryFun HiFunAdd a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  case arg1 of
    (HiValueNumber x) -> case arg2 of
      (HiValueNumber y) -> applyNumberFunc (+) x y
      _ -> throwError HiErrorInvalidArgument
    (HiValueString x) -> case arg2 of
      (HiValueString y) -> return $ HiValueString $ T.concat [x, y]
      _ -> throwError HiErrorInvalidArgument
    (HiValueList x) -> case arg2 of
      (HiValueList y) -> return $ HiValueList $ x >< y
      _ -> throwError HiErrorInvalidArgument
    (HiValueBytes x) -> case arg2 of
      (HiValueBytes y) -> return $ HiValueBytes $ B.concat [x, y]
      _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalBinaryFun HiFunSub a b = evalBinaryNumberFunNoThrow (-) a b
evalBinaryFun HiFunMul a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  case arg1 of
    (HiValueNumber x) -> case arg2 of
      (HiValueNumber y) -> applyNumberFunc (*) x y
      _ -> throwError HiErrorInvalidArgument
    (HiValueString x) -> case arg2 of
      (HiValueNumber y) -> case getContainerMulArg y of
        Just z -> if z <= 0 then throwError HiErrorInvalidArgument else return $ HiValueString $ stimes z x
        Nothing -> return HiValueNull
      _ -> throwError HiErrorInvalidArgument
    (HiValueList x) -> case arg2 of
      (HiValueNumber y) -> case getContainerMulArg y of
        Just z -> if z <= 0 then throwError HiErrorInvalidArgument else return $ HiValueList $ stimes z x
        Nothing -> return HiValueNull
      _ -> throwError HiErrorInvalidArgument
    (HiValueBytes x) -> case arg2 of
      (HiValueNumber y) -> case getContainerMulArg y of
        Just z -> if z <= 0 then throwError HiErrorInvalidArgument else return $ HiValueBytes $ stimes z x
        Nothing -> return HiValueNull
      _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalBinaryFun HiFunDiv a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  case arg1 of
    (HiValueNumber x) -> case arg2 of
      (HiValueNumber y) -> if y == 0 then throwError HiErrorDivideByZero else applyNumberFunc (/) x y
      _ -> throwError HiErrorInvalidArgument
    (HiValueString x) -> case arg2 of
      (HiValueString y) -> return $ HiValueString $ T.concat [x, (T.pack "/"), y]
      _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalBinaryFun HiFunAnd a b = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueBool x) -> case x of
      False -> return arg1
      True -> evalBinaryBoolFunNoThrow (&&) True b
    _ -> throwError HiErrorInvalidArgument

evalBinaryFun HiFunOr  a b = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueBool x) -> case x of
      True -> return arg1
      False -> evalBinaryBoolFunNoThrow (||) False b
    _ -> throwError HiErrorInvalidArgument

evalBinaryFun HiFunLessThan       a b = evalBinaryCompare (<)  a b
evalBinaryFun HiFunGreaterThan    a b = evalBinaryCompare (>)  a b
evalBinaryFun HiFunEquals         a b = evalBinaryCompare (==) a b
evalBinaryFun HiFunNotLessThan    a b = evalBinaryCompare (>=) a b
evalBinaryFun HiFunNotGreaterThan a b = evalBinaryCompare (<=) a b
evalBinaryFun HiFunNotEquals      a b = evalBinaryCompare (/=) a b

evalBinaryFun HiFunRange a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  case arg1 of
    (HiValueNumber x) -> case arg2 of
      (HiValueNumber y) -> return $ HiValueList $ fromList (map HiValueNumber [x .. y])
      _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalBinaryFun HiFunFold a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  case arg1 of
    (HiValueFunction hiFun) -> case arg2 of
      (HiValueList values) -> evalFold hiFun values
      _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalBinaryFun _ _ _ = throwError HiErrorArityMismatch

evalUnaryBoolFunNoThrow :: Monad m => (Bool -> Bool) -> HiExpr -> EvaluatorT m
evalUnaryBoolFunNoThrow f a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueBool x) -> return $ HiValueBool $ f x
    _ -> throwError HiErrorInvalidArgument

evalUnaryStringFunNoThrow :: Monad m => (T.Text -> T.Text) -> HiExpr -> EvaluatorT m
evalUnaryStringFunNoThrow f a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueString x) -> return $ HiValueString $ f x
    _ -> throwError HiErrorInvalidArgument

numberToWord8 :: Monad m => HiValue -> EvaluatorTypedT m W.Word8
numberToWord8 (HiValueNumber number) = case getContainerMulArg number of
  Just x -> return $ fromIntegral x
  Nothing -> throwError HiErrorInvalidArgument
numberToWord8 _ = throwError HiErrorInvalidArgument

listToBytes :: Monad m => Seq HiValue -> EvaluatorT m
listToBytes seq_ = do
  elems <- traverse numberToWord8 seq_
  return $ HiValueBytes $ B.pack (toList elems)

bytesToList :: B.ByteString -> HiValue
bytesToList bs = do
  let wordList = B.unpack bs
  let integerList = map (fromIntegral :: W.Word8 -> Integer) wordList
  let rationalList = map toRational integerList
  let hiValueList = map HiValueNumber rationalList
  HiValueList $ Data.Sequence.fromList hiValueList

evalUnaryFun :: Monad m => HiFun -> HiExpr -> EvaluatorT m
evalUnaryFun HiFunNot x = evalUnaryBoolFunNoThrow not x

evalUnaryFun HiFunLength a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueString x) -> return $ HiValueNumber $ fromIntegral $ T.length x
    (HiValueList x) -> return $ HiValueNumber $ fromIntegral $ Data.Sequence.length x
    (HiValueBytes x) -> return $ HiValueNumber $ fromIntegral $ B.length x
    _ -> throwError HiErrorInvalidArgument

evalUnaryFun HiFunTrim    a = evalUnaryStringFunNoThrow T.strip a
evalUnaryFun HiFunToUpper a = evalUnaryStringFunNoThrow T.toUpper a
evalUnaryFun HiFunToLower a = evalUnaryStringFunNoThrow T.toLower a
evalUnaryFun HiFunReverse a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueString x) -> return $ HiValueString $ T.reverse x
    (HiValueList x) -> return $ HiValueList $ Data.Sequence.reverse x
    (HiValueBytes x) -> return $ HiValueBytes $ B.reverse x
    _ -> throwError HiErrorInvalidArgument

evalUnaryFun HiFunPackBytes a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueList x) -> listToBytes x
    _ -> throwError HiErrorInvalidArgument

evalUnaryFun HiFunUnpackBytes a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueBytes x) -> return $ bytesToList x
    _ -> throwError HiErrorInvalidArgument

evalUnaryFun HiFunEncodeUtf8 a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueString x) -> return $ HiValueBytes $ encodeUtf8 x
    _ -> throwError HiErrorInvalidArgument

evalUnaryFun HiFunDecodeUtf8 a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueBytes x) -> case decodeUtf8' x of
      Left _ -> return HiValueNull
      Right text -> return $ HiValueString text
    _ -> throwError HiErrorInvalidArgument

evalUnaryFun HiFunZip a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueBytes x) -> let compParams = Z.defaultCompressParams{compressLevel=Z.bestCompression} in
       return $ HiValueBytes $ BL.toStrict $ Z.compressWith compParams (BL.fromStrict x)
    _ -> throwError HiErrorInvalidArgument

evalUnaryFun HiFunUnzip a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueBytes x) ->
      return $ HiValueBytes $ BL.toStrict $ Z.decompressWith Z.defaultDecompressParams (BL.fromStrict x)
    _ -> throwError HiErrorInvalidArgument

evalUnaryFun HiFunSerialise a = do
  arg1 <- evalFunctionArg a
  return $ HiValueBytes $ BL.toStrict $ S.serialise arg1

evalUnaryFun HiFunDeserialise a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueBytes x) -> case S.deserialiseOrFail @HiValue (BL.fromStrict x) of
      Right hiValue -> return hiValue
      Left _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalUnaryFun _ _ = throwError HiErrorArityMismatch

evalTernaryFun :: Monad m => HiFun -> HiExpr -> HiExpr -> HiExpr -> EvaluatorT m
evalTernaryFun HiFunIf cond a b = do
  arg1 <- evalFunctionArg cond
  case arg1 of
    (HiValueBool x) -> if x then (evalHiExpr a) else (evalHiExpr b)
    _ -> throwError HiErrorInvalidArgument
evalTernaryFun _ _ _ _ = throwError HiErrorArityMismatch

getByIndex :: Monad m => Integer -> T.Text -> EvaluatorT m
getByIndex idx text = do
  let len = T.length text
  let idxInt = fromIntegral idx
  if (idxInt < 0) || (idxInt >= len) then
    return HiValueNull
  else
    return $ HiValueString $ T.pack [T.index text idxInt]

getSeqByIndex :: Monad m => Integer -> Seq HiValue -> EvaluatorT m
getSeqByIndex idx values = do
  let len = Data.Sequence.length values
  let idxInt = fromIntegral idx
  if (idxInt < 0) || (idxInt >= len) then
    return HiValueNull
  else
    return $ values `index` idxInt

getBytesByIndex :: Monad m => Integer -> B.ByteString -> EvaluatorT m
getBytesByIndex idx values = do
  let len = B.length values
  let idxInt = fromIntegral idx
  if (idxInt < 0) || (idxInt >= len) then
    return HiValueNull
  else
    return $ HiValueNumber $ toRational $ values `B.index` idxInt

evalUnaryString :: Monad m => T.Text -> HiExpr -> EvaluatorT m
evalUnaryString text a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueNumber x) -> case getContainerMulArg x of
      Just idx -> getByIndex idx text
      Nothing -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalUnaryList :: Monad m => Seq HiValue -> HiExpr -> EvaluatorT m
evalUnaryList values a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueNumber x) -> case getContainerMulArg x of
      Just idx -> getSeqByIndex idx values
      Nothing -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalUnaryBytes :: Monad m => B.ByteString -> HiExpr -> EvaluatorT m
evalUnaryBytes values a = do
  arg1 <- evalFunctionArg a
  case arg1 of
    (HiValueNumber x) -> case getContainerMulArg x of
      Just idx -> getBytesByIndex idx values
      Nothing -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

getSlice :: T.Text -> Maybe Integer -> Maybe Integer -> T.Text
getSlice text (Just a) (Just b)
  | a >= 0 && b >= 0 = naive text a b
  | a >= 0 = naive doubleText a (b + len)
  | b >= 0 = naive doubleText (a + len) b
  | otherwise = naive doubleText (a + len) (b + len)
    where
      len :: Integer
      len = fromIntegral $ T.length text

      doubleText :: T.Text
      doubleText = T.concat [text, text]

      naive :: T.Text -> Integer -> Integer -> T.Text
      naive text_ a_ b_
        = T.take (fromIntegral (b_ - a_)) (T.drop (fromIntegral a_) text_)
getSlice text Nothing Nothing = text
getSlice text (Just a) Nothing = getSlice text (Just a) (Just $ fromIntegral $ T.length text)
getSlice text Nothing (Just b) = getSlice text (Just 0) (Just b)

getSeqSlice :: Seq HiValue -> Maybe Integer -> Maybe Integer -> Seq HiValue
getSeqSlice values (Just a) (Just b)
  | a >= 0 && b >= 0 = naive values a b
  | a >= 0 = naive doubleSeq a (b + len)
  | b >= 0 = naive doubleSeq (a + len) b
  | otherwise = naive doubleSeq (a + len) (b + len)
    where
      len :: Integer
      len = fromIntegral $ Data.Sequence.length values

      doubleSeq :: Seq HiValue
      doubleSeq = values >< values

      naive :: Seq HiValue -> Integer -> Integer -> Seq HiValue
      naive values_ a_ b_
        = Data.Sequence.take (fromIntegral (b_ - a_)) (Data.Sequence.drop (fromIntegral a_) values_)
getSeqSlice values Nothing Nothing = values
getSeqSlice values (Just a) Nothing = getSeqSlice values (Just a) (Just $ fromIntegral $ Data.Sequence.length values)
getSeqSlice values Nothing (Just b) = getSeqSlice values (Just 0) (Just b)

getBytesSlice :: B.ByteString -> Maybe Integer -> Maybe Integer -> B.ByteString
getBytesSlice values (Just a) (Just b)
  | a >= 0 && b >= 0 = naive values a b
  | a >= 0 = naive doubleSeq a (b + len)
  | b >= 0 = naive doubleSeq (a + len) b
  | otherwise = naive doubleSeq (a + len) (b + len)
    where
      len :: Integer
      len = fromIntegral $ B.length values

      doubleSeq :: B.ByteString
      doubleSeq = B.concat [values, values]

      naive :: B.ByteString -> Integer -> Integer -> B.ByteString
      naive values_ a_ b_
        = B.take (fromIntegral (b_ - a_)) (B.drop (fromIntegral a_) values_)
getBytesSlice values Nothing Nothing = values
getBytesSlice values (Just a) Nothing = getBytesSlice values (Just a) (Just $ fromIntegral $ B.length values)
getBytesSlice values Nothing (Just b) = getBytesSlice values (Just 0) (Just b)

evalBinaryString :: Monad m => T.Text -> HiExpr -> HiExpr -> EvaluatorT m
evalBinaryString text a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  case arg1 of
    (HiValueNumber x) -> case arg2 of
      (HiValueNumber y) -> case getContainerMulArg x of
        Just idx1 -> case getContainerMulArg y of
          Just idx2 -> return $ HiValueString $ getSlice text (Just idx1) (Just idx2)
          Nothing -> throwError HiErrorInvalidArgument
        Nothing -> throwError HiErrorInvalidArgument
      HiValueNull -> case getContainerMulArg x of
        Just idx1 -> return $ HiValueString $ getSlice text (Just idx1) Nothing
        Nothing -> throwError HiErrorInvalidArgument
      _ -> throwError HiErrorInvalidArgument
    HiValueNull -> case arg2 of
      (HiValueNumber y) -> case getContainerMulArg y of
        Just idx2 -> return $ HiValueString $ getSlice text Nothing (Just idx2)
        Nothing -> throwError HiErrorInvalidArgument
      HiValueNull -> return $ HiValueString $ getSlice text Nothing Nothing
      _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument


evalBinaryList :: Monad m => Seq HiValue -> HiExpr -> HiExpr -> EvaluatorT m
evalBinaryList values a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  case arg1 of
    (HiValueNumber x) -> case arg2 of
      (HiValueNumber y) -> case getContainerMulArg x of
        Just idx1 -> case getContainerMulArg y of
          Just idx2 -> return $ HiValueList $ getSeqSlice values (Just idx1) (Just idx2)
          Nothing -> throwError HiErrorInvalidArgument
        Nothing -> throwError HiErrorInvalidArgument
      HiValueNull -> case getContainerMulArg x of
        Just idx1 -> return $ HiValueList $ getSeqSlice values (Just idx1) Nothing
        Nothing -> throwError HiErrorInvalidArgument
      _ -> throwError HiErrorInvalidArgument
    HiValueNull -> case arg2 of
      (HiValueNumber y) -> case getContainerMulArg y of
        Just idx2 -> return $ HiValueList $ getSeqSlice values Nothing (Just idx2)
        Nothing -> throwError HiErrorInvalidArgument
      HiValueNull -> return $ HiValueList $ getSeqSlice values Nothing Nothing
      _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument
    
evalBinaryBytes :: Monad m => B.ByteString -> HiExpr -> HiExpr -> EvaluatorT m
evalBinaryBytes values a b = do
  arg1 <- evalFunctionArg a
  arg2 <- evalFunctionArg b
  case arg1 of
    (HiValueNumber x) -> case arg2 of
      (HiValueNumber y) -> case getContainerMulArg x of
        Just idx1 -> case getContainerMulArg y of
          Just idx2 -> return $ HiValueBytes $ getBytesSlice values (Just idx1) (Just idx2)
          Nothing -> throwError HiErrorInvalidArgument
        Nothing -> throwError HiErrorInvalidArgument
      HiValueNull -> case getContainerMulArg x of
        Just idx1 -> return $ HiValueBytes $ getBytesSlice values (Just idx1) Nothing
        Nothing -> throwError HiErrorInvalidArgument
      _ -> throwError HiErrorInvalidArgument
    HiValueNull -> case arg2 of
      (HiValueNumber y) -> case getContainerMulArg y of
        Just idx2 -> return $ HiValueBytes $ getBytesSlice values Nothing (Just idx2)
        Nothing -> throwError HiErrorInvalidArgument
      HiValueNull -> return $ HiValueBytes $ getBytesSlice values Nothing Nothing
      _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalHiExprApply :: Monad m => HiExpr -> [HiExpr] -> EvaluatorT m
evalHiExprApply (HiExprValue (HiValueFunction HiFunList)) args = do
  elems <- traverse evalHiExpr args
  return $ HiValueList (fromList elems)

evalHiExprApply (HiExprValue (HiValueFunction hiFun)) args = do
  case args of
    [arg1] -> evalUnaryFun hiFun arg1
    [arg1, arg2] -> evalBinaryFun hiFun arg1 arg2
    [arg1, arg2, args3] -> evalTernaryFun hiFun arg1 arg2 args3
    _ -> throwError HiErrorArityMismatch

evalHiExprApply (HiExprApply hiExpr innerArgs) args = do
  func <- evalHiExprApply hiExpr innerArgs
  case func of
    hiValue@(HiValueFunction _) -> evalHiExprApply (HiExprValue hiValue) args
    hiValue@(HiValueString _)   -> evalHiExprApply (HiExprValue hiValue) args
    hiValue@(HiValueList _)     -> evalHiExprApply (HiExprValue hiValue) args
    hiValue@(HiValueBytes _)    -> evalHiExprApply (HiExprValue hiValue) args
    _ -> throwError HiErrorInvalidFunction

evalHiExprApply (HiExprValue (HiValueString text)) args = do
  case args of
    [arg1] -> evalUnaryString text arg1
    [arg1, arg2] -> evalBinaryString text arg1 arg2
    _ -> throwError HiErrorArityMismatch

evalHiExprApply (HiExprValue (HiValueList values)) args = do
  case args of
    [arg1] -> evalUnaryList values arg1
    [arg1, arg2] -> evalBinaryList values arg1 arg2
    _ -> throwError HiErrorArityMismatch

evalHiExprApply (HiExprValue (HiValueBytes values)) args = do
  case args of
    [arg1] -> evalUnaryBytes values arg1
    [arg1, arg2] -> evalBinaryBytes values arg1 arg2
    _ -> throwError HiErrorArityMismatch

evalHiExprApply _ _ = throwError HiErrorInvalidFunction

evalHiExpr :: Monad m => HiExpr -> EvaluatorT m
evalHiExpr (HiExprValue hiValue) = return hiValue
evalHiExpr (HiExprApply hiExprFunc hiExprArgsList) = evalHiExprApply hiExprFunc hiExprArgsList

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval hiExpr = runEvaluatorT $ evalHiExpr hiExpr