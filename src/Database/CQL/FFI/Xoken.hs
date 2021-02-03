{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module Database.CQL.FFI.Xoken
    ( insertTxIdOutputs
    , someFunc
    , cassInit
    , insertTx
    ) where

import Foreign.C
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Int
import qualified Data.ByteString.Lazy as B

someFunc :: IO ()
someFunc = 
    cassInit >>= print >>
    insertTx "nohardcode" "bi" 12 51 "serialized_tx" [(("a", 5), 2, ("b", 13)), (("bda", 45), 21, ("bd", 11))] 110 >>= print -- >>runXCql-- >> runXCql

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

unzipOther :: [((String,Int32),Int32,(String,Int64))] -> ([String],[Int32],[Int32],[String],[Int64])
unzipOther inp = let (a,b,c) = unzip3 inp
                     (a1,a2) = unzip a
                     (c1,c2) = unzip c
                 in (a1,a2,b,c1,c2)

foreign import ccall "xoken.c insert_txid_outputs"
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

foreign import ccall "xoken.c insert_tx"
    c_insert_tx
        :: CString
        -> CString -> CInt -> CInt
        -> CInt -> Ptr CUChar
        -> CInt -> Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CString -> Ptr CLong
        -> CLong
        -> IO CInt

insertTx
    :: String
    -> String -> Int32 -> Int32
    -> B.ByteString
    -> [((String,Int32),Int32,(String,Int64))]
    -> Int64
    -> IO CInt
insertTx txid bi bi1 bi2 tx_s oth fees = do
    let ol = CInt $ fromIntegral $ length oth
        (ota,ota',otb,otc,otc') = unzipOther oth
        blob = fmap CUChar $ B.unpack tx_s
        blob_len = CInt $ fromIntegral $ B.length tx_s
    withCString txid
        $ \txs -> withCString bi
                    $ \bis -> do
                        cota <- mapM newCString ota
                        cotc <- mapM newCString otc
                        let cota' = fmap CInt ota'
                        let cotb = fmap CInt otb
                        let cotc' = fmap CLong otc'
                        withArray cota $ \aota -> withArray cota' $ \aota' -> withArray cotb $ \aotb -> withArray cotc $ \aotc -> withArray cotc' $ \aotc' -> withArray blob $ \blobptr -> do
                            ret <- c_insert_tx txs bis (CInt bi1) (CInt bi2) blob_len blobptr ol aota aota' aotb aotc aotc' (CLong fees)
                            mapM_ free (cota ++ cotc)
                            return ret

foreign import ccall "bindings.c &session"
    session :: CassSessionPtr

foreign import ccall "cassandra.h cass_session_new"
    cassSessionNew :: IO CassSessionPtr

foreign import ccall "bindings.h init"
    cassInit :: IO CInt

concHs :: IO ()
concHs = print "run"
