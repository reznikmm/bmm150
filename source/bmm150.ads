--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

with Interfaces;

package BMM150 is
   pragma Preelaborate;
   pragma Discard_Names;

   type Power_Mode is (Normal, Forced, Sleep);

   type Output_Data_Rate is range 2 .. 30
     with Static_Predicate =>
       Output_Data_Rate in 2 | 6 | 8 | 10 | 15 | 20 | 25 | 30;
   --
   --  Magnetometer output data rate (ODR) in Hz

   subtype Repetition_Count is Positive range 1 .. 511
     with Dynamic_Predicate => Repetition_Count mod 2 = 1;
   --  FIXME: should be Static_Predicate when the compiler understand

   type Setting is record
      X_Y : Repetition_Count;
      Z   : Positive range 1 .. 256;
   end record;

   function Max_Measurement_Time (Preset : Setting) return Positive
     is (145 * Preset.X_Y + 500 * Preset.Z + 980);
   --  The maximum selectable read-out frequency in forced mode in µs.

   Low_Power_Preset        : constant Setting := (X_Y => 3, Z => 3);
   --  Noise: 1.0/1.4 uT, current 0.17 mA at ODR 10 Hz

   Regular_Preset          : constant Setting := (X_Y => 9, Z => 15);
   --  Noise: 0.6/0.6 uT, current 0.50 mA at ODR 10 Hz

   Enhanced_Regular_Preset : constant Setting := (X_Y => 15, Z => 27);
   --  Noise: 0.5/0.5 uT, current 0.80 mA at ODR 10 Hz

   High_Accuracy_Preset    : constant Setting := (X_Y => 15, Z => 27);
   --  Noise: 0.3/0.3 uT, current 4.90 mA at ODR 20 Hz

   use type Interfaces.Integer_16;
   use type Interfaces.Unsigned_16;

   type Trim_Registers is record
      X1, Y1, X2, Y2 : Interfaces.Integer_8;
      Z1 : Interfaces.Unsigned_16;
      Z2 : Interfaces.Integer_16 range 1 .. 2 ** 15 - 1;
      Z3, Z4 : Interfaces.Integer_16;
      XY1  : Interfaces.Unsigned_8;
      XY2  : Interfaces.Integer_8;
      XYZ1 : Interfaces.Unsigned_16 range 1 .. 2 ** 15 - 1;
   end record;
   --  Calibration constants per chip. Make visible to allow constant
   --  initialised to a value known in advance.
   --
   --  My guess, Z2 should be >=0 to avoid devide by zero in Z compensation.

   Small_4 : constant := 1.0 / 2.0**4;

   type Magnetic_Field is delta Small_4 range -2048.0 .. 2048.0 - Small_4;
   --  Magnetic flux density in uT (micro-tesla)

   type Magnetic_Field_Vector is record
      X, Y, Z : Magnetic_Field;
   end record;
   --  3D vector of magnetic flux density in uT (micro-tesla)

   subtype Raw_XY is Interfaces.Integer_16 range -(2 ** 12) .. 2 ** 12 - 1;
   subtype Raw_Z  is Interfaces.Integer_16 range -(2 ** 14) .. 2 ** 14 - 1;

   type Raw_Density_Vector is record
      X, Y : Raw_XY;
      Z    : Raw_Z;
   end record;
   --  3D vector of magnetic flux density as provided by the sensor

   subtype Raw_Hall is Interfaces.Unsigned_16 range 0 .. 2 ** 14 - 1;
   --  Hall resistor as provided by the sensor

   subtype I2C_Address_Range is Interfaces.Unsigned_8 range 16#10# .. 16#13#;

   subtype Byte is Interfaces.Unsigned_8;  --  Register value

   subtype Register_Address is Natural range 16#00# .. 16#FF#;
   --  Sensor's register address

   type Byte_Array is array (Register_Address range <>) of Byte;
   --  Bytes to be exchanged with registers. Index is a register address, while
   --  elements are corresponding register values.

   BMM150_Chip_Id : constant := 16#32#;

end BMM150;
