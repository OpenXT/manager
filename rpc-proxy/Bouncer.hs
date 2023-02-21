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

{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable,MultiParamTypeClasses,FlexibleContexts,OverloadedStrings,ExistentialQuantification,TypeSynonymInstances,FlexibleInstances #-}

module Bouncer where

import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Control.Exception as E
import Control.Monad.Loops
import Msg.DBus
import Channel
import Tools.Log
import Tools.IfM
import Tools.Misc

-- | one-way bouncer reads messages from source, applies transform and forwards to destination
data Bouncer a b = Bouncer (Route a b) Transform

data Route a b =
    (ReceiveMessages Channel a, SendMessages Channel b) =>
     Route Channel Channel

reverseRoute :: (ReceiveMessages Channel b, SendMessages Channel a) => Route a b -> Route b a
reverseRoute (Route from to) = Route to from

-- | On the fly message transformation. Transform can also drop the message
newtype Transform = Transform (Msg -> MaybeT IO Msg)
runTransform (Transform f) = runMaybeT . f

-- dropMsg should be equivalent to: fail undefined
dropMsg :: MaybeT IO Msg
dropMsg = MaybeT $ return Nothing

instance Semigroup Transform where
  (Transform f) <> (Transform g) = Transform (f >=> g)

instance Monoid Transform where
  mempty = Transform return

class ReceiveMessages src a where receiveMessages ::  src -> IO [a]
class SendMessages    dst a where sendMessage :: dst -> a -> IO ()

instance ReceiveMessages Channel Msg where receiveMessages = unmarshaledMessages
instance SendMessages    Channel Msg where sendMessage ch (Msg _ buf) = send_all ch buf

-- | conversion between message formats
class MsgConvert m a b where msgconvert :: a -> MaybeT m b
liftMsgConvertBase f = mapMaybeT f . msgconvert

-- | no-op default conversion
instance (Monad m) => MsgConvert m a a where msgconvert = return

-- | bounce messages from source to destination
-- | We stop trying to bounce, as soon as sending fails.
bounce :: forall m a b.
          ( MonadIO m
          , MsgConvert m a Msg
          , MsgConvert m Msg b )
          => Bouncer a b -> m ()
bounce (Bouncer (Route from to) (Transform xform))
  = void . allM bounce1 <=< liftIO . receiveMessages $ from
  where
    bounce1 :: a -> m Bool
    bounce1 = liftM (fromMaybe True) . runMaybeT . (lift . deliver <=< msgconvert <=< xform' <=< msgconvert)

    xform' = mapMaybeT liftIO . xform

    -- Bool stands for "Did sending succeed?"
    deliver :: b -> m Bool
    deliver = handle <=< liftIO . E.try . sendMessage to
    handle (Right _) = return True
    handle (Left (err :: E.SomeException)) = warn (show err) >> return False

    -- ToDo: 'void' should come out of Tools.Misc, but for some
    -- reasons it doesn't.
    void m = m >> return ()
