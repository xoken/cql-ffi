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
    insertTxidOutputs "nohardcode1" 105 "nhevqv" "qwfqgwes" True "bi" 42 12 "a" 5 2 "b" 13 100 >>= print >>
    insertTxidOutputs "nohardcode1" 106 "nhevqv" "qwfqgwes" False "bi" 12 51 "a" 5 2 "b" 13 110 >>= print -- >>runXCql-- >> runXCql

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
        -> CString -> CInt -> CInt
        -> CString -> CInt -> CInt -> CString -> CLong
        -> CLong
        -> IO CInt

insertTxidOutputs 
    :: String
    -> Int32
    -> String
    -> String
    -> Bool
    -> String -> Int32 -> Int32
    -> String -> Int32 -> Int32 -> String -> Int64
    -> Int64
    -> IO CInt
insertTxidOutputs txid output_index address scripthash is_recv bi bi1 bi2 ota ota' otb otc otc' value =
    withCString txid
        $ \txs -> withCString address
            $ \ads -> withCString scripthash
                $ \shs -> withCString bi
                    $ \bis -> withCString ota
                        $ \otas -> withCString otc
                            $ \otcs -> c_insert_txid_outputs txs (CInt output_index) ads shs (CBool $ if is_recv then 1 else 0) bis (CInt bi1) (CInt bi2) otas (CInt ota') (CInt otb) otcs (CLong otc') (CLong value)
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
