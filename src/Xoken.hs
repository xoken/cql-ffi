{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module Xoken
    ( someFunc
    ) where

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Data.Int

someFunc :: IO ()
someFunc = cass_init >>= print >> c_insert_misc_store >>= print -- >>runXCql-- >> runXCql

{-
conc :: IO ()
conc = do
    t1 <- getPOSIXTime
    i <- concurrent
    t2 <- getPOSIXTime
    print (t1,t2,t2-t1)
-}

data CassSession

type CassSessionPtr = Ptr CassSession

foreign import ccall "xoken.h insert_misc_store"
    c_insert_misc_store :: IO CInt
{-
foreign import ccall "xoken.h insert_misc_store"
    c_insert_misc_store :: CString -> CInt -> CString -> IO CInt

foreign import ccall "xoken.h run"
    run :: IO CInt

insert_misc_store :: String -> Int32 -> String -> IO CInt
insert_misc_store k ht hs = withCString k (\ks -> withCString hs (\hss -> c_insert_misc_store ks (CInt ht) hss))
-}
foreign import ccall "bindings.c &session"
    session :: CassSessionPtr

foreign import ccall "cassandra.h cass_session_new"
    cassSessionNew :: IO CassSessionPtr

foreign import ccall "bindings.h init"
    cass_init :: IO CInt

concHs :: IO ()
concHs = print "run"
