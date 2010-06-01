
module Text.Annotext.Parser where

import Control.Applicative
import Data.ByteString.Char8

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
  addy                      <-  match (email_part; char '@'; email_part)
  return (preamble, addy)
 where
  email_part                 =  takeWhile part_char
   where
    part_char c              =  c /= '@' && not_weird c

present_directory_path       =  do
  chars "./"

no_ASCII_weirdness           =  takeWhile not_weird


not_weird c = c >= '!' && c <= '~' -- Good, ol'-fashioned American characters.
               || c > '\DEL' -- Never know what to expect with those foreign
                             -- characters...

chars                        =  string . fromString

