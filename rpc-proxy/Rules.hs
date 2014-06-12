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

module Rules ( Match (..)
             , Rule (..)
             , RuleType (..)
             , RuleSubject (..)
             , MessageTag (..)
             , Artefact (..)
             , ArtefactSource (..)
             , Direction (..)
             , PropertyMatch (..)
             , testPure ) where

import Data.List (foldl', all)
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Text.Lazy (Text)
import Types

-- ToDo: Fix typo (typ -> type).
data Rule = Rule { typ :: RuleType
                 , subject :: RuleSubject
                 , match :: Match } deriving ( Eq, Show )

data RuleType = Allow | Deny deriving ( Eq, Show )

data Direction = Incoming | Outgoing deriving (Eq, Show)

{-| Identifies what the rule applies to (might have separate rules from
    incoming / outgoing messages hence the direction -}
data RuleSubject = RuleSubject { direction :: !Direction,
                                 tag :: !MessageTag } deriving (Eq, Show )
data MessageTag = TagMethodCall | TagMethodReturn | TagSignal | TagError | TagAny deriving (Eq, Show)

-- match can be applied to artefact
data Match = Match { domain_id :: Maybe DomID
                   , domain_uuid :: Maybe Uuid
                   , sender :: Maybe Text
                   , destination :: Maybe Text
                   , interface :: Maybe Text
                   , member :: Maybe Text
                   , stubdom :: Maybe Bool
                   , properties :: [PropertyMatch]
                   }
           deriving (Eq, Show)

-- matches on vm dbus property
data PropertyMatch
   = PropertyMatchB PropertyName Bool (Maybe Bool) -- name, expected value, actual value (if already read)
   | PropertyMatchS PropertyName String (Maybe String)
   deriving (Eq, Show)

type PropertyName = String

propertyMatchMatches :: PropertyMatch -> Bool
propertyMatchMatches (PropertyMatchB _ a b) = Just a == b
propertyMatchMatches (PropertyMatchS _ a b) = Just a == b

-- artefact is the incoming thingy
class Artefact a where
    artefactType :: a -> MessageTag
    artefactSource :: a -> ArtefactSource
    artefactSender :: a -> Maybe Text
    artefactDestination :: a -> Maybe Text
    artefactInterface :: a -> Maybe Text
    artefactMember :: a -> Maybe Text
    artefactPropertyInterface :: a -> Maybe Text

-- where is the thingy incoming from
data ArtefactSource
   = ArtefactSource { sourceDomainID :: !DomID
                    , sourceUuid :: Maybe Uuid
                    , sourceIsStubdom :: Bool
                    } deriving (Eq, Show)

fieldMatches :: (Eq a) => Maybe a -> Maybe a -> Bool
fieldMatches (Just v) (Just v') = v == v'
fieldMatches _ _ = True

-- we assume non existing fields are matching
matches :: Artefact a => a -> Match -> Bool
matches a match =
    let ArtefactSource domid uuid is_stubdom = artefactSource a
    in
    and [ fieldMatches (Just domid) (domain_id match)
        , fieldMatches uuid (domain_uuid match)
        , fieldMatches (artefactSender a) (sender match)
        , fieldMatches (artefactDestination a) (destination match)
        ,    fieldMatches (artefactInterface a) (interface match)
          || fieldMatches (artefactPropertyInterface a) (interface match)
        , fieldMatches (artefactMember a) (member match)
        , fieldMatches (Just is_stubdom) (stubdom match)
        , all propertyMatchMatches (properties match) ]

rulesFor :: RuleSubject -> [Rule] -> [Rule]
rulesFor (RuleSubject dir tag_) =
    filter (same_tag . tag . subject) . filter ((dir==) . direction . subject)
    where same_tag TagAny = True
          same_tag tag' = tag_ == tag'

{- Deny by default.
   Later rules overrule earlier ones. -}
testPure :: Artefact a => a -> RuleSubject -> [Rule] -> Bool
testPure art subj = fromMaybe False . msum . map g . reverse . rulesFor subj where
     g (Rule Allow _ m) | matches art m = Just True
                        | otherwise = Nothing
     g (Rule Deny _ m)  | matches art m = Just False
                        | otherwise = Nothing
