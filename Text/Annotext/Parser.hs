
module Text.Annotext.Parser where

import Prelude hiding (null, takeWhile, concat)
import Control.Applicative hiding (empty)
import Data.ByteString.Char8 hiding (takeWhile)

import Data.Attoparsec.Char8


schemes                      =  [ "http://", "https://",
                                  "ssh://", "sftp://", "scp://",
                                  "amqp://", "amqps://",
                                  "irc://",
                                  "xmpp://",
                                  "file://"
                                ]


url                          =  do
  scheme                    <-  (choice . fmap chars) schemes
  body                      <-  no_ASCII_weirdness
  return (scheme, body)

email_bytes                  =  do
  preamble                  <-  chars "mailto:" <|> pure empty
  user                      <-  email_part
  at                        <-  char '@'
  realm                     <-  email_part
  return (preamble, user `snoc` at `append` realm)
 where
  email_part                 =  takeWhile part_char
   where
    part_char c              =  c /= '@' && not_weird c

present_directory_path       =  do
  chars "./"

no_ASCII_weirdness           =  takeWhile not_weird


not_weird c                  =  (c >= '!') && (c <= '~') || (c > '\DEL')
                            --  Recognize ASCII characters between '!' and '~'
                            --  or accept all higher-valued Unicode chars.

chars                        =  string . pack
