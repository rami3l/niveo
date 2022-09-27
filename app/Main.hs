module Main where

import Dispatch (args, dispatch)
import Options.Applicative
  ( execParser,
    fullDesc,
    header,
    helper,
    info,
  )
import Relude

main :: IO ()
main = execParser opts >>= dispatch
  where
    opts = (args <**> helper) `info` infoMod
    infoMod = fullDesc <> header "niveo - A programmable configuration language."
