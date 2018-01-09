{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module System.Hardware.Modbus ( Parity(..)
                              , newRTU
                              , connect
                              , close
                              ) where

import Foreign.C
import Foreign.C.Error
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad (when)

data ModbusContext
type ModbusHandle = ForeignPtr ModbusContext

foreign import ccall "modbus_new_rtu" modbus_new_rtu :: CString -> Int -> Char -> Int -> Int -> IO (Ptr ModbusContext)
foreign import ccall "modbus_connect" modbus_connect :: Ptr ModbusContext -> IO Int
foreign import ccall "modbus_strerror" modbus_strerror :: Errno -> IO CString
foreign import ccall "modbus_close" modbus_close :: Ptr ModbusContext -> IO ()
foreign import ccall "&modbus_free" modbus_free :: FunPtr (Ptr ModbusContext -> IO ())

getModbusError :: IO String
getModbusError = getErrno >>= modbus_strerror >>= peekCString

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
  when (out /= 0) $ getModbusError >>= fail

close :: ModbusHandle -> IO ()
close h = do
  withForeignPtr h modbus_close
  finalizeForeignPtr h
