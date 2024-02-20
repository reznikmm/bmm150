--  SPDX-FileCopyrightText: 2024 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

--  This package offers a straightforward method for setting up the BMM150
--  when connected via I2C, especially useful when you need multiple sensors
--  of this kind. If you use only one sensor, it could be preferable to use the
--  BMM150.I2C generic package.

with HAL.I2C;
with HAL.Time;

package BMM150.I2C_Sensors is

   type BMM150_Sensor
     (I2C_Port    : not null HAL.I2C.Any_I2C_Port;
      I2C_Address : I2C_Address_Range) is tagged limited
   record
      Trim : Trim_Registers;
   end record;

   procedure Suspend_Off
     (Self    : BMM150_Sensor;
      Timer   : not null HAL.Time.Any_Delays;
      Success : out Boolean);
   --  Leave suspend mode. This puts the chip into Sleep mode. There is no
   --  access to registers before this, so no other operations are working.
   --  It takes 3 ms to enter sleep mode.

   procedure Suspend_On
     (Self    : BMM150_Sensor;
      Timer   : not null HAL.Time.Any_Delays;
      Success : out Boolean);
   --  Put the chip into suspend mode

   function Check_Chip_Id (Self : BMM150_Sensor) return Boolean;
   --  Read the chip ID and check that it matches

   procedure Reset
     (Self    : BMM150_Sensor;
      Success : out Boolean);
   --  Issue a soft reset and wait until the chip is ready.

   procedure Set_Power_Mode
     (Self    : BMM150_Sensor;
      Mode    : Power_Mode;
      ODR     : Output_Data_Rate := 2;
      Success : out Boolean);
   --  Change sensor power mode and output data rate. Mainly used to start one
   --  measurement or enable perpetual cycling of measurements and inactive
   --  periods.

   procedure Set_Repetitions
     (Self    : BMM150_Sensor;
      Preset  : Setting;
      Success : out Boolean);
   --  Set the number of repetitions per measurement

   function Measuring (Self : BMM150_Sensor) return Boolean;
   --  Check if a measurement is in progress

   procedure Read_Measurement
     (Self    : BMM150_Sensor;
      Value   : out Density_Vector;
      Success : out Boolean);
   --  Read the measurement values from the sensor and make the temperature
   --  compensation for X, Y and Z. Self.Trim should be read before calling
   --  this procedure, see Read_Trim_Registers.

   procedure Read_Raw_Measurement
     (Self    : BMM150_Sensor;
      Value   : out Raw_Density_Vector;
      R_Hall  : out Interfaces.Unsigned_16;
      Success : out Boolean);
   --  Read the raw measurement values from the sensor

   procedure Read_Trim_Registers
     (Self    : in out BMM150_Sensor;
      Success : out Boolean);
   --  Read the calibration constants from the sensor into Self.Trim

end BMM150.I2C_Sensors;
