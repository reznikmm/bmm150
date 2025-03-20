--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

--  This package provides a low-level interface for interacting with the
--  sensor. Communication with the sensor is done by reading/writing one
--  or more bytes to predefined registers. The interface allows the user to
--  implement the read/write operations in the way they prefer but handles
--  encoding/decoding register values into user-friendly formats.
--
--  For each request to the sensor, the interface defines a subtype-array
--  where the index of the array element represents the register number to
--  read/write, and the value of the element represents the corresponding
--  register value.
--
--  Functions starting with `Set_` prepare values to be written to the
--  registers. Conversely, functions starting with `Get_` decode register
--  values. Functions starting with `Is_` are a special case for boolean
--  values.
--
--  The user is responsible for reading and writing register values!

package BMM150.Raw is

   subtype Suspend_Data is Byte_Array (16#4B# .. 16#4B#);
   --  Register (0x4B) contains control bits for power control, soft reset and
   --  interface SPI mode selection.

   function Set_Suspend
     (Suspend    : Boolean;
      SPI_3_Wire : Boolean := False) return Suspend_Data;
   --  Put the chip into Suspend mode or back to Sleep by setting the power
   --  bit. Wake up form Suspend mode takes up to 3 milliseconds.

   function Reset return Suspend_Data;
   --  Soft reset does not execute a full POR sequence, but all registers are
   --  reset except for the “trim” registers above register 0x54 and the
   --  power control register (0x4B). Soft reset always brings the device into
   --  sleep mode. When device is in the suspend mode, soft reset is ignored
   --  and the device remains in suspend mode.

   function Is_Reseting (Raw : Byte_Array) return Boolean
     with Pre => Suspend_Data'First in Raw'Range;
   --  The two “Soft Reset” bits are reset to “0” automatically after
   --  soft reset was completed.

   subtype Chip_Id_Data is Byte_Array (16#40# .. 16#40#);
   --  Register (0x40) Chip ID contains the magnetometer chip identification
   --  number, which is 0x32.

   function Get_Chip_Id (Raw : Byte_Array) return Byte is
     (Raw (Chip_Id_Data'First))
       with Pre => Chip_Id_Data'First in Raw'Range;
   --  Read the chip ID. Raw data should contain Chip_Id_Data'First item.

   subtype Repetitions_Data is Byte_Array (16#51# .. 16#52#);
   --  Number of repetitions control registers

   function Set_Repetitions (Preset : Setting) return Repetitions_Data;
   --  Set the number of repetitions per measurement

   subtype Power_Mode_Data is Byte_Array (16#4C# .. 16#4C#);
   --  Register (0x4C) contains control bits for operation mode, output data
   --  rate and self-test.

   function Set_Power_Mode
     (Mode : Power_Mode;
      ODR  : Output_Data_Rate) return Power_Mode_Data;
   --  Change sensor power mode and output data rate. Mainly used to start one
   --  measurement or enable perpetual cycling of measurements and inactive
   --  periods.

   subtype Interrupt_Setting_Data is Byte_Array (16#4E# .. 16#4E#);
   --  Register (0x4E) contains control bits interrupt settings and axes enable
   --  bits.

   function Set_Interrupt_Setting
     (Interrupt_Pin   : Boolean := False;
      Data_Ready_Pin  : Boolean := False;
      Data_Ready_High : Boolean := True;
      Interrupt_High  : Boolean := True;
      Interrupt_Latch : Boolean := True) return Interrupt_Setting_Data;
   --  Change data ready (DRDY) and interrupt (INT) pins settings

   subtype Measurement_Data is Byte_Array (16#42# .. 16#49#);
   --  Magnetic field data, hall resistance and data ready status bit.

   function Is_Measuring (Raw : Byte_Array) return Boolean
     with Pre => 16#48# in Raw'Range;
   --  Check if a measurement is in progress in Data ready (DRDY) status bit.

   procedure Get_Raw_Measurement
     (Raw    : Byte_Array;
      Value  : out Raw_Density_Vector;
      R_Hall : out BMM150.Raw_Hall)
     with Pre =>
       Measurement_Data'First in Raw'Range and then
       Measurement_Data'Last in Raw'Range;
   --  Read the raw measurement values from the sensor.

   function Is_Valid (Raw : Byte_Array) return Boolean
     with Pre => 16#48# in Raw'Range and 16#49# in Raw'Range;
   --  Check if raw Measurement_Data could be decoded (hall resistance /= 0)

   function Get_Measurement
     (Raw  : Byte_Array;
      Trim : Trim_Registers) return Magnetic_Field_Vector
     with Pre => Is_Valid (Raw) and then
       Measurement_Data'First in Raw'Range and then
       Measurement_Data'Last in Raw'Range;
   --  Decode raw measurement. Raw data should contain Measurement_Data'Range
   --  items and hall resistance /= 0.

   subtype Trim_Registers_Data is Byte_Array (16#5D# .. 16#71#);
   --  Calibration constants registers

   function Get_Trim_Registers (Raw : Byte_Array) return Trim_Registers
     with Pre =>
       Trim_Registers_Data'First in Raw'Range and then
       Trim_Registers_Data'Last in Raw'Range;
   --  Decode the calibration constants from the registers

   ----------------------------------
   -- SPI/I2C Write/Read functions --
   ----------------------------------

   use type Byte;

   function SPI_Write (X : Register_Address) return Byte is
     (Byte (X) and 16#7F#);
   --  For read operation on the SPI bus the register address is passed with
   --  the highest bit off (0).

   function SPI_Read (X : Register_Address) return Byte is
     (Byte (X) or 16#80#);
   --  For write operation on the SPI bus the register address is passed with
   --  the highest bit on (1).

   function SPI_Write (X : Byte_Array) return Byte_Array is
     ((X'First - 1 => SPI_Write (X'First)) & X);
   --  Prefix the byte array with the register address for the SPI write
   --  operation

   function SPI_Read (X : Byte_Array) return Byte_Array is
     ((X'First - 1 => SPI_Read (X'First)) & X);
   --  Prefix the byte array with the register address for the SPI read
   --  operation

   function I2C_Write (X : Byte_Array) return Byte_Array is
     ((X'First - 1 => Byte (X'First)) & X);
   --  Prefix the byte array with the register address for the I2C write
   --  operation

   function I2C_Read (X : Byte_Array) return Byte_Array renames I2C_Write;
   --  Prefix the byte array with the register address for the I2C read
   --  operation

private

   function Set_Suspend
     (Suspend    : Boolean;
      SPI_3_Wire : Boolean := False) return Suspend_Data is
        (16#4B# => (if Suspend then 0 elsif SPI_3_Wire then 4 else 1));

   function Reset return Suspend_Data is (16#4B# => 16#83#);

   function Is_Reseting (Raw : Byte_Array) return Boolean is
      ((Raw (Suspend_Data'First) and 16#82#) /= 0);

   function Set_Repetitions (Preset : Setting) return Repetitions_Data is
      (Byte (Preset.X_Y / 2), Byte (Preset.Z / 2));

   function Is_Measuring (Raw : Byte_Array) return Boolean is
      ((Raw (16#48#) and 1) = 0);

   function Is_Valid (Raw : Byte_Array) return Boolean is
      ((Raw (16#48#) and 16#FC#) /= 0 or Raw (16#49#) /= 0);

   function Set_Interrupt_Setting
     (Interrupt_Pin   : Boolean := False;
      Data_Ready_Pin  : Boolean := False;
      Data_Ready_High : Boolean := True;
      Interrupt_High  : Boolean := True;
      Interrupt_Latch : Boolean := True) return Interrupt_Setting_Data is
       (Interrupt_Setting_Data'First =>
          (if Interrupt_High  then 2**0 else 0) +
          (if Interrupt_Latch then 2**1 else 0) +
          (if Data_Ready_High then 2**2 else 0) +
          (if Interrupt_Pin   then 2**6 else 0) +
          (if Data_Ready_Pin  then 2**7 else 0));

end BMM150.Raw;
