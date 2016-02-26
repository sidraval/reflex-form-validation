{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

module Reflex.FormValidation where

import Data.Map(Map)
import qualified Data.Map as Map
import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
  rec firstPassword <- textInput $ dynTextConfig textAttr
      secondPassword <- textInput $ dynTextConfig textAttr
      matchingPasswords <- combineDyn matchingAndNonZero (_textInput_value firstPassword) (_textInput_value secondPassword)
      textAttr <- mapDyn dependentInputStyle matchingPasswords
      dependentlyEnabledButton "Submit" matchingPasswords
  return ()

dynTextConfig :: (Reflex t) => Dynamic t (Map String String) -> TextInputConfig t
dynTextConfig m = TextInputConfig { _textInputConfig_inputType = "text"
                                  , _textInputConfig_initialValue = ""
                                  , _textInputConfig_setValue = never
                                  , _textInputConfig_attributes = m
                                  }

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
