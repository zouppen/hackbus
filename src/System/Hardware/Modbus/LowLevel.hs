{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface, RecordWildCards #-}
module System.Hardware.Modbus.LowLevel
  ( ModbusHandle
  , ModbusException
  , Parity(..)
  , newRTU
  , connect
  , setSlave
  , readInputBits
  , readRegisters
  , writeBit
  , writeRegister
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
import Foreign.Storable (Storable)
import Foreign.Ptr

data ModbusException = ModbusException { modbusErrno    :: Int32
                                       , modbusStrerror :: String
                                       } deriving (Show, Typeable)
instance Exception ModbusException

data ModbusContext
type ModbusHandle = ForeignPtr ModbusContext
type ModbusPtr = Ptr ModbusContext

foreign import ccall "modbus_new_rtu" modbus_new_rtu :: CString -> Int -> Char -> Int -> Int -> IO (ModbusPtr)
foreign import ccall "modbus_connect" modbus_connect :: ModbusPtr -> IO Int
foreign import ccall "modbus_set_slave" modbus_set_slave :: ModbusPtr -> Int -> IO Int
foreign import ccall "modbus_read_input_bits" modbus_read_input_bits :: ModbusPtr -> Int -> Int -> Ptr Word8 -> IO Int
foreign import ccall "modbus_read_registers" modbus_read_registers :: ModbusPtr  -> Int -> Int -> Ptr Word16 -> IO Int
foreign import ccall "modbus_write_bit" modbus_write_bit :: ModbusPtr -> Int -> Int -> IO Int
foreign import ccall "modbus_write_register" modbus_write_register :: ModbusPtr -> Int -> Word16 -> IO Int
foreign import ccall "modbus_strerror" modbus_strerror :: Errno -> IO CString
foreign import ccall "modbus_close" modbus_close :: ModbusPtr -> IO ()
foreign import ccall "&modbus_free" modbus_free :: FunPtr (ModbusPtr -> IO ())

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

-- |Internal function for reading an array. Wraps all C stuff inside.
readRaw :: Storable a => (ModbusPtr -> Int -> Int -> Ptr a -> IO Int) -> ([a] -> b) -> ModbusHandle -> Int -> Int -> IO b
readRaw action f h addr nb = allocaArray nb $ \dest -> do
  out <- withForeignPtr h $ \p -> action p addr nb dest
  when (out /= nb) failModbus
  f <$> peekArray nb dest

-- |The function uses the Modbus function code 0x02 (read input status).
readInputBits :: ModbusHandle -> Int -> Int -> IO [Bool]
readInputBits = readRaw modbus_read_input_bits $ map (==1)

-- |The function uses the Modbus function code 0x03 (read holding registers).
readRegisters :: ModbusHandle -> Int -> Int -> IO [Word16]
readRegisters = readRaw modbus_read_registers id

writeRaw :: (ModbusPtr -> Int -> b -> IO Int) -> (a -> b) -> ModbusHandle -> Int -> a -> IO ()
writeRaw action f h addr value = do
  out <- withForeignPtr h $ \p -> action p addr (f value)
  when (out /= 1) $ failModbus

-- |The function uses the Modbus function code 0x05 (force single coil).
writeBit :: ModbusHandle -> Int -> Bool -> IO ()
writeBit = writeRaw modbus_write_bit fromEnum

-- |The function uses the Modbus function code 0x06 (preset single register).
writeRegister :: ModbusHandle -> Int -> Word16 -> IO ()
writeRegister = writeRaw modbus_write_register id

close :: ModbusHandle -> IO ()
close h = withForeignPtr h modbus_close
