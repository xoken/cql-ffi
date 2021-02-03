{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module Database.CQL.FFI.Xoken
    ( insertTxIdOutputs
    , someFunc
    , cassInit
    ) where

import Foreign.C
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Int

someFunc :: IO ()
someFunc = 
    cassInit >>= print
    --insertTxidOutputs "nohardcode1" 105 "nhevqv" "qwfqgwes" True "bi" 42 12 [(("a", 5), 2, ("b", 13))] 100 >>= print >>
    --insertTxidOutputs "nohardcode1" 106 "nhevqv" "qwfqgwes" False "bi" 12 51 [(("a", 5), 2, ("b", 13)), (("bda", 45), 21, ("bd", 11))] 110 >>= print -- >>runXCql-- >> runXCql

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
        -> CInt -> Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CString -> Ptr CLong
        -> CLong
        -> IO CInt

unzipOther :: [((String,Int32),Int32,(String,Int64))] -> ([String],[Int32],[Int32],[String],[Int64])
unzipOther inp = let (a,b,c) = unzip3 inp
                     (a1,a2) = unzip a
                     (c1,c2) = unzip c
                 in (a1,a2,b,c1,c2)

insertTxIdOutputs 
    :: String
    -> Int32
    -> String
    -> String
    -> Bool
    -> String -> Int32 -> Int32
    -> [((String,Int32),Int32,(String,Int64))]
    -> Int64
    -> IO CInt
insertTxIdOutputs txid output_index address scripthash is_recv bi bi1 bi2 oth value = do
    let ol = CInt $ fromIntegral $ length oth
        (ota,ota',otb,otc,otc') = unzipOther oth
    withCString txid
        $ \txs -> withCString address
            $ \ads -> withCString scripthash
                $ \shs -> withCString bi
                    $ \bis -> do
                        cota <- mapM newCString ota
                        cotc <- mapM newCString otc
                        let cota' = fmap CInt ota'
                        let cotb = fmap CInt otb
                        let cotc' = fmap CLong otc'
                        withArray cota $ \aota -> withArray cota' $ \aota' -> withArray cotb $ \aotb -> withArray cotc $ \aotc -> withArray cotc' $ \aotc' -> do
                            ret <- c_insert_txid_outputs txs (CInt output_index) ads shs (CBool $ if is_recv then 1 else 0) bis (CInt bi1) (CInt bi2) ol aota aota' aotb aotc aotc' (CLong value)
                            mapM_ free (cota ++ cotc)
                            return ret
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
    cassInit :: IO CInt

concHs :: IO ()
concHs = print "run"
