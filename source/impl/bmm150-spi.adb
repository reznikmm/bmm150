--  SPDX-FileCopyrightText: 2024 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

with BMM150.Internal;

package body BMM150.SPI is

   type Chip_Settings is null record;

   Chip : Chip_Settings;

   procedure Read
     (Ignore  : Chip_Settings;
      Data    : out Byte_Array;
      Success : out Boolean);
   --  Read registers starting from Data'First

   procedure Write
     (Ignore  : Chip_Settings;
      Address : Register_Address;
      Data    : Interfaces.Unsigned_8;
      Success : out Boolean);
   --  Write a register (at Address) with Data

   package Sensor is new Internal (Chip_Settings, Read, Write);

   -------------------
   -- Check_Chip_Id --
   -------------------

   function Check_Chip_Id return Boolean is
     (Sensor.Check_Chip_Id (Chip, BMM150_Chip_Id));

   function Measuring return Boolean is (False);

   ----------
   -- Read --
   ----------

   procedure Read
     (Ignore  : Chip_Settings;
      Data    : out Byte_Array;
      Success : out Boolean)
   is
      use type HAL.UInt8;
      use all type HAL.SPI.SPI_Status;

      Addr : constant HAL.UInt8 := HAL.UInt8 (Data'First) or 16#80#;
      Status : HAL.SPI.SPI_Status;
      Output : HAL.SPI.SPI_Data_8b (Data'Range);
   begin
      SPI.SPI_CS.Clear;

      SPI_Port.Transmit (HAL.SPI.SPI_Data_8b'(1 => Addr), Status);

      if Status = Ok then
         SPI_Port.Receive (Output, Status);

         for J in Data'Range loop
            Data (J) := Interfaces.Unsigned_8 (Output (J));
         end loop;
      end if;

      SPI.SPI_CS.Set;

      Success := Status = Ok;
   end Read;

   ----------------------
   -- Read_Measurement --
   ----------------------

   procedure Read_Measurement
     (Value   : out Magnetic_Field_Vector;
      Trim    : Trim_Registers;
      Success : out Boolean) is
   begin
      Sensor.Read_Measurement (Chip, Trim, Value, Success);
   end Read_Measurement;

   -------------------------
   -- Read_Trim_Registers --
   -------------------------

   procedure Read_Trim_Registers
     (Value   : out Trim_Registers;
      Success : out Boolean) is
   begin
      Sensor.Read_Trim_Registers (Chip, Value, Success);
   end Read_Trim_Registers;

   --------------------------
   -- Read_Raw_Measurement --
   --------------------------

   procedure Read_Raw_Measurement
     (Value   : out Raw_Density_Vector;
      R_Hall  : out Interfaces.Unsigned_16;
      Success : out Boolean) is
   begin
      Sensor.Read_Raw_Measurement (Chip, Value, R_Hall, Success);
   end Read_Raw_Measurement;

   -----------
   -- Reset --
   -----------

   procedure Reset (Success : out Boolean) is
   begin
      Sensor.Reset (Chip, Success);
   end Reset;

   --------------------
   -- Set_Power_Mode --
   --------------------

   procedure Set_Power_Mode
     (Mode    : Power_Mode;
      ODR     : Output_Data_Rate := 2;
      Success : out Boolean) is
   begin
      Sensor.Set_Power_Mode (Chip, Mode, ODR, Success);
   end Set_Power_Mode;

   ---------------------
   -- Set_Repetitions --
   ---------------------

   procedure Set_Repetitions
     (Preset  : Setting;
      Success : out Boolean) is
   begin
      Sensor.Set_Repetitions (Chip, Preset, Success);
   end Set_Repetitions;

   -----------------
   -- Suspend_Off --
   -----------------

   procedure Suspend_Off
     (Timer   : not null HAL.Time.Any_Delays;
      Success : out Boolean) is
   begin
      Sensor.Set_Suspend (Chip, Suspend => False, Success => Success);
      Timer.Delay_Milliseconds (3);
   end Suspend_Off;

   ----------------
   -- Suspend_On --
   ----------------

   procedure Suspend_On (Success : out Boolean) is
   begin
      Sensor.Set_Suspend (Chip, Suspend => True, Success => Success);
   end Suspend_On;

   -----------
   -- Write --
   -----------

   procedure Write
     (Ignore  : Chip_Settings;
      Address : Register_Address;
      Data    : Interfaces.Unsigned_8;
      Success : out Boolean)
   is
      use all type HAL.SPI.SPI_Status;

      Addr : constant HAL.UInt8 := HAL.UInt8 (Address);
      Status : HAL.SPI.SPI_Status;
   begin
      SPI.SPI_CS.Clear;

      SPI_Port.Transmit
        (HAL.SPI.SPI_Data_8b'(Addr, HAL.UInt8 (Data)),
         Status);

      SPI.SPI_CS.Set;

      Success := Status = Ok;
   end Write;

end BMM150.SPI;
