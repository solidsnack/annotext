
module Text.Annotext.Parser where

import Prelude hiding (null, takeWhile)
import Control.Applicative
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
  preamble                  <-  chars "mailto:" <|> pure null
  addy                      <-  match (email_part >> char '@' >> email_part)
  return (preamble, addy)
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


chars                        =  string . fromString

