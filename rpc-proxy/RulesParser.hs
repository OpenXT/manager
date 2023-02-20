--
-- Copyright (c) 2012 Citrix Systems, Inc.
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

{-# LANGUAGE TupleSections,NoMonomorphismRestriction,FlexibleContexts #-}
module RulesParser where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.List ( foldl' )
import Data.Maybe
import Data.String ( fromString )
import qualified Data.Text.Lazy as TL
import Text.ParserCombinators.Parsec
import Text.Printf

import Rules
import Tools.Db

type RulesParser a = GenParser Token () a
type Token = (SourcePos, TokenData)
data TokenData = Str String | Int Int | EOL deriving (Show)

-- lexical parser
positioned x = (,x) <$> getPosition
lexeol_p = char '\n' >> positioned EOL
lextabspace_p = char ' ' <|> tab
lexint_p = positioned . Int . read =<< many1 digit
lexstr_p = positioned . Str =<< (:) <$> alphaNum <*> many (alphaNum <|> oneOf "_.-")
lexcomment_p = char '#' >> many (noneOf "\n") >> lexeol_p
lexer_p = catMaybes <$> manyTill
          (Nothing <$ lextabspace_p <|>
           Just <$> choice [lexstr_p, lexint_p, lexcomment_p, lexeol_p])
          eof

lexer :: String -> Either ParseError [Token]
lexer = parse lexer_p ""

token' :: (TokenData -> Maybe a) -> RulesParser a
token' test = token showToken posToken testToken where
    showToken (p,t) = show t
    posToken  (p,t) = p
    testToken (p,t) = test t

-- semantic parser
keyword_p word = token' f where f (Str s) | word == s = Just ()
                                f _                   = Nothing
str_p = token' f where f (Str s) = Just s
                       f _ = Nothing
eol_p = token' f where f EOL = Just ()
                       f _ = Nothing
num_p = token' f where f (Int i) = Just i
                       f _ = Nothing

data Spec = Sender String
          | Dest String
          | Intf String
          | Member String
          | DomainUuid String
          | DomainID Int
          | Stubdom Bool
          | IfBool String Bool
          | DomainByType String
          | DomainByName String

-- If no subject specified, it's any incoming message.
default_subject = RuleSubject Incoming TagAny

matchAll :: Match
matchAll = Match Nothing Nothing Nothing Nothing Nothing Nothing (Just False) []

matchOfSpecifiers :: [Spec] -> Match
matchOfSpecifiers = foldl' f matchAll where
    f m (Dest p) = m { destination = Just $ TL.pack p }
    f m (Sender p) = m { sender = Just $ TL.pack p }
    f m (Intf p) = m { interface = Just $ TL.pack p }
    f m (Member p) = m { member = Just $ TL.pack p }
    f m (DomainUuid p) = m { domain_uuid = Just $ fromString p }
    f m (DomainID p) = m { domain_id = Just $ fromIntegral p }
    f m (Stubdom p) = m { stubdom = Just p }
    f m (IfBool p v) = m { properties = PropertyMatchB p v Nothing : properties m }
    f m (DomainByType v) = m { properties = PropertyMatchS "type" v Nothing : properties m }
    f m (DomainByName v) = m { properties = PropertyMatchS "name" v Nothing : properties m }

ruletype_p =
     Allow <$ keyword_p "allow"
 <|> Deny  <$ keyword_p "deny"
 <?> "type of rule: allow or deny"

bool_p =
     True  <$ keyword_p "true"
 <|> False <$ keyword_p "false"
 <?> "boolean: true or false"

subject_p = choice
     [ RuleSubject Incoming TagMethodCall <$ k "inc-method-call"
     , RuleSubject Outgoing TagMethodCall <$ k "out-method-call"
     , RuleSubject Incoming TagSignal <$ k "inc-signal"
     , RuleSubject Outgoing TagSignal <$ k "out-signal"
     , RuleSubject Incoming TagMethodReturn <$ k "inc-method-return"
     , RuleSubject Outgoing TagMethodReturn <$ k "out-method-return"
     , RuleSubject Incoming TagError <$ k "inc-error"
     , RuleSubject Outgoing TagError <$ k "out-error"
     , RuleSubject Incoming TagAny <$ k "inc-any"
     , RuleSubject Outgoing TagAny <$ k "out-any" ]

     <?> "rule subject: inc-call-method, out-call-method, inc-signal, out-signal, inc-method-return, out-method-return, inc-error, out-error, inc-any, out-any"
  where k = keyword_p

specifier_p = choice
    [ k "destination" >> Dest <$> str_p
    , k "interface"   >> Intf <$> str_p
    , k "member"      >> Member <$> str_p
    , k "dom-uuid"    >> DomainUuid <$> str_p
    , k "dom-id"      >> DomainID <$> num_p
    , k "dom-type"    >> DomainByType <$> str_p
    , k "dom-name"    >> DomainByName <$> str_p
    , k "sender"      >> Sender <$> str_p
    , k "stubdom"     >> Stubdom <$> bool_p
    , k "if-boolean"  >> IfBool <$> str_p <*> bool_p
      ]
    <?> "match specifier, one of: destination,interface,member,dom-uuid,dom-id,sender"
  where k = keyword_p

match_p = matchAll <$ keyword_p "all"
      <|> matchOfSpecifiers <$> many specifier_p
      <?> "match definition or 'all'"

rule_p =
    Rule <$> ruletype_p <*> option default_subject subject_p <*> match_p
    <?> "rule definition"

rules_p = skipMany eol_p >> sepEndBy rule_p (skipMany1 eol_p) <* eof
          <?> "list of rules"


parseRules :: String -> Either ParseError [Rule]
parseRules input = parse rules_p "" =<< lexer input

instance Marshall Rule where
    -- dbWrite not implemented.
    dbRead p = do ruleStr <- dbReadStr p
                  case parse rule_p p =<< parse lexer_p p ruleStr of
                      Left errors -> error $ printf
                                     "Malformed rule (%s) in database at path %s: %s."
                                     ruleStr p (show errors)
                      Right rule -> return rule
