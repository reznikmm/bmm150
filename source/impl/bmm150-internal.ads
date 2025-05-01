--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

private generic
   type Device_Context (<>) is limited private;

   with procedure Read
     (Device  : Device_Context;
      Data    : out Byte_Array;
      Success : out Boolean);
   --  Read the values from the BMM150 chip registers into Data.
   --  Each element in the Data corresponds to a specific register address
   --  in the chip, so Data'Range determines the range of registers to read.
   --  The value read from register X will be stored in Data(X), so
   --  Data'Range should be of the Register_Address subtype.

   with procedure Write
     (Device  : Device_Context;
      Address : Register_Address;
      Data    : Interfaces.Unsigned_8;
      Success : out Boolean);
   --  Write the value to the BMM150 chip register with given Address.

package BMM150.Internal is

   procedure Set_Suspend
     (Device     : Device_Context;
      Suspend    : Boolean;
      SPI_3_Wire : Boolean := False;
      Success    : out Boolean);
   --  Put the chip into Suspend mode or back to Sleep by setting the power
   --  bit.

   function Check_Chip_Id
     (Device : Device_Context;
      Expect : Interfaces.Unsigned_8) return Boolean;
   --  Read the chip ID and check that it matches

   procedure Reset
     (Device  : Device_Context;
      Success : out Boolean);
   --  Issue a soft reset and wait until the chip is ready.

   procedure Set_Repetitions
     (Device  : Device_Context;
      Preset  : Setting;
      Success : out Boolean);
   --  Set the number of repetitions per measurement

   procedure Set_Interrupt_Setting
     (Device          : Device_Context;
      Interrupt_Pin   : Boolean := False;
      Data_Ready_Pin  : Boolean := False;
      Data_Ready_High : Boolean := True;
      Interrupt_High  : Boolean := True;
      Interrupt_Latch : Boolean := True;
      Success         : out Boolean);
   --  Change data ready (DRDY) and interrupt (INT) pins settings

   procedure Set_Power_Mode
     (Device  : Device_Context;
      Mode    : Power_Mode;
      ODR     : Output_Data_Rate;
      Success : out Boolean);
   --  Change sensor power mode and output data rate. Mainly used to start one
   --  measurement or enable perpetual cycling of measurements and inactive
   --  periods.

   function Measuring (Device : Device_Context) return Boolean;
   --  Check if a measurement is in progress

   procedure Read_Measurement
     (Device  : Device_Context;
      Trim    : Trim_Registers;
      Value   : out Optional_Magnetic_Field_Vector;
      Success : out Boolean);
   --  Read the raw measurement values from the sensor

   procedure Read_Raw_Measurement
     (Device  : Device_Context;
      Value   : out Raw_Density_Vector;
      R_Hall  : out BMM150.Raw_Hall;
      Success : out Boolean);
   --  Read the raw measurement values from the sensor

   procedure Read_Trim_Registers
     (Device  : Device_Context;
      Value   : out Trim_Registers;
      Success : out Boolean);
   --  Read the calibration constants from the sensor

end BMM150.Internal;
