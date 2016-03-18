{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

module Reflex.FormValidation where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Digest.Pure.MD5
import Reflex
import Reflex.Dom
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = mainWidget $ do
  rec email <- textInput $ dynTextConfig emailTextAttr
      firstPassword <- textInput $ dynTextConfig textAttr
      secondPassword <- textInput $ dynTextConfig textAttr
      matchingPasswords <- combineDyn matchingAndNonZero (_textInput_value firstPassword) (_textInput_value secondPassword)
      isEmail <- mapDyn isValidEmail (_textInput_value email)
      emailTextAttr <- mapDyn dependentInputStyle isEmail
      textAttr <- mapDyn dependentInputStyle matchingPasswords
      validForm <- combineDyn (&&) matchingPasswords isEmail
      dynamicGravatarImage (_textInput_value email)
      dependentlyEnabledButton "Submit" validForm
  return ()

dynamicGravatarImage :: (MonadWidget t m) => Dynamic t String -> m ()
dynamicGravatarImage s = do
  md5email <- mapDyn (show . md5 . BS.pack) s
  gravatarURL <- mapDyn ((++) "http://www.gravatar.com/avatar/") md5email
  attrs <- mapDyn (Map.singleton "src") gravatarURL
  elDynAttr "img" attrs $ return ()
  return ()

dynTextConfig :: (Reflex t) => Dynamic t (Map String String) -> TextInputConfig t
dynTextConfig m = TextInputConfig { _textInputConfig_inputType = "text"
                                  , _textInputConfig_initialValue = ""
                                  , _textInputConfig_setValue = never
                                  , _textInputConfig_attributes = m
                                  }

isValidEmail :: String -> Bool
isValidEmail s = '@' `elem` s && '.' `elem` s

dependentInputStyle :: Bool -> Map String String
dependentInputStyle b
  | b = Map.singleton "style" "border-color: green;"
  | otherwise = Map.singleton "style" "border-color: red;"

dependentlyEnabledButton :: (MonadWidget t m) => String -> Dynamic t Bool -> m ()
dependentlyEnabledButton bText s = do
    buttonAttributes <- mapDyn disabledButtonAttributes s
    elDynAttr "button" buttonAttributes $ text bText

matchingPasswordsText :: Bool -> String
matchingPasswordsText b
  | b = "Valid"
  | otherwise = "Invalid!"

disabledButtonAttributes :: Bool -> Map String String
disabledButtonAttributes b
  | b = Map.empty
  | otherwise = Map.singleton "disabled" "true"

matchingAndNonZero :: String -> String -> Bool
matchingAndNonZero x y = (x == y) && (length x > 0)
