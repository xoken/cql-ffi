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
someFunc = 
    cass_init >>= print >>
    insertTxidOutputs "nohardcode1" 100 "nhevqv" "qwfqgwes" True 100 >>= print >>
    insertTxidOutputs "nohardcode1" 101 "nhevqv" "qwfqgwes" False 110 >>= print -- >>runXCql-- >> runXCql

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

foreign import ccall "xoken.h insert_txid_outputs"
    c_insert_txid_outputs
        :: CString
        -> CInt
        -> CString
        -> CString
        -> CBool
        -> CLong
        -> IO CInt

insertTxidOutputs :: String -> Int32 -> String -> String -> Bool -> Int64 -> IO CInt
insertTxidOutputs txid output_index address scripthash is_recv value =
    withCString txid
        $ \txs -> 
            withCString address
                $ \ads ->
                    withCString scripthash
                        $ \shs -> c_insert_txid_outputs txs (CInt output_index) ads shs (CBool $ if is_recv then 1 else 0) (CLong value)
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
