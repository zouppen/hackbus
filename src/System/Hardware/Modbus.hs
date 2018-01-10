{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module System.Hardware.Modbus ( ModbusHandle
                              , Parity(..)
                              , newRTU
                              , connect
                              , setSlave
                              , readInputBits
                              , writeBit
                              , close
                              ) where

import Data.Word
import Foreign.C
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Control.Monad (when)

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

getModbusError :: IO String
getModbusError = getErrno >>= modbus_strerror >>= peekCString

failModbus :: IO ()
failModbus = getModbusError >>= fail

-- Public parts

data Parity = ParityNone | ParityEven | ParityOdd deriving Show

newRTU :: String -> Int -> Parity -> Int -> Int -> IO ModbusHandle
newRTU device baud parity dataBit stopBit = do
  let parityC = case parity of
        ParityNone -> 'N'
        ParityEven -> 'E'
        ParityOdd  -> 'O'
  ptr <- withCString device $ \devC -> modbus_new_rtu devC baud parityC dataBit stopBit
  when (ptr == nullPtr) $ getModbusError >>= fail
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
