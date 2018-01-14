{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface, RecordWildCards #-}
module System.Hardware.Modbus.LowLevel
  ( ModbusHandle
  , ModbusException
  , Parity(..)
  , newRTU
  , connect
  , setSlave
  , readInputBits
  , writeBit
  , close
  ) where

import Control.Exception
import Control.Monad (when)
import Data.Typeable
import Data.Word
import Data.Int (Int32)
import Foreign.C
import Foreign.C.Error
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr

data ModbusException = ModbusException { modbusErrno    :: Int32
                                       , modbusStrerror :: String
                                       } deriving (Show, Typeable)
instance Exception ModbusException

data ModbusContext
type ModbusHandle = ForeignPtr ModbusContext

foreign import ccall "modbus_new_rtu" modbus_new_rtu :: CString -> Int -> Char -> Int -> Int -> IO (Ptr ModbusContext)
foreign import ccall "modbus_connect" modbus_connect :: Ptr ModbusContext -> IO Int
foreign import ccall "modbus_set_slave" modbus_set_slave :: Ptr ModbusContext -> Int -> IO Int
foreign import ccall "modbus_read_input_bits" modbus_read_input_bits :: Ptr ModbusContext -> Int -> Int -> Ptr Word8 -> IO Int
foreign import ccall "modbus_write_bit" modbus_write_bit :: Ptr ModbusContext -> Int -> Int -> IO Int
foreign import ccall "modbus_strerror" modbus_strerror :: Errno -> IO CString
foreign import ccall "modbus_close" modbus_close :: Ptr ModbusContext -> IO ()
foreign import ccall "&modbus_free" modbus_free :: FunPtr (Ptr ModbusContext -> IO ())

failModbus :: IO ()
failModbus = do
  errno <- getErrno
  let modbusErrno = (\(Errno (CInt i)) -> i) errno
  modbusStrerror <- modbus_strerror errno >>= peekCString
  throw ModbusException{..}

-- Public parts

data Parity = ParityNone | ParityEven | ParityOdd deriving Show

newRTU :: String -> Int -> Parity -> Int -> Int -> IO ModbusHandle
newRTU device baud parity dataBit stopBit = do
  let parityC = case parity of
        ParityNone -> 'N'
        ParityEven -> 'E'
        ParityOdd  -> 'O'
  ptr <- withCString device $ \devC -> modbus_new_rtu devC baud parityC dataBit stopBit
  when (ptr == nullPtr) $ failModbus
  newForeignPtr modbus_free ptr

connect :: ModbusHandle -> IO ()
connect h = do
  out <- withForeignPtr h modbus_connect
  when (out /= 0) failModbus

setSlave :: ModbusHandle -> Int -> IO ()
setSlave h slaveID = do
  out <- withForeignPtr h $ \p -> modbus_set_slave p slaveID
  when (out /= 0) failModbus

readInputBits :: ModbusHandle -> Int -> Int -> IO [Bool]
readInputBits h addr nb = allocaArray nb $ \dest -> do
  out <- withForeignPtr h $ \p -> modbus_read_input_bits p addr nb dest
  when (out /= nb) failModbus
  map (==1) <$> peekArray nb dest

writeBit :: ModbusHandle -> Int -> Bool -> IO ()
writeBit h addr status = do
  out <- withForeignPtr h $ \p -> modbus_write_bit p addr (fromEnum status)
  when (out /= 1) $ failModbus

close :: ModbusHandle -> IO ()
close h = withForeignPtr h modbus_close
